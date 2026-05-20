<#
.SYNOPSIS
  Hand off a task to Codex CLI in an isolated git worktree. Capture structured result.

.PARAMETER TaskId
  Numeric task ID from tasks/todo.md (matches "## Task <N>" heading).

.PARAMETER PlanFile
  Relative path to plan file from repo root. Default: tasks/todo.md

.PARAMETER Sandbox
  Codex sandbox policy. read-only | workspace-write | danger-full-access. Default: workspace-write

.PARAMETER ModelOverride
  Override the model from ~/.codex/config.toml. Optional.

.PARAMETER BaseBranch
  Branch to fork from. Default: main

.EXAMPLE
  pwsh -File scripts/bridge.ps1 -TaskId 1
#>
param(
    [Parameter(Mandatory=$true)][int]$TaskId,
    [string]$PlanFile = "tasks/todo.md",
    [ValidateSet("read-only","workspace-write","danger-full-access")][string]$Sandbox = "workspace-write",
    [string]$ModelOverride,
    [string]$BaseBranch = "main"
)

$ErrorActionPreference = "Stop"
$startedAt = Get-Date

$RepoRoot = (Resolve-Path "$PSScriptRoot\..").Path
Set-Location $RepoRoot

# --- 1. Preflight: find codex binary (cross-platform) ---
$codexCmd = Get-Command codex -ErrorAction SilentlyContinue
if ($codexCmd) {
    $codex = $codexCmd.Source
} else {
    # Fallback paths in order of likelihood per OS
    $fallbackPaths = @(
        # Windows
        "$env:USERPROFILE\AppData\Local\OpenAI\Codex\bin\codex.exe",
        # Mac (typical install locations)
        "/usr/local/bin/codex",
        "/opt/homebrew/bin/codex",
        "/Applications/Codex.app/Contents/Resources/codex",
        "$env:HOME/.local/bin/codex",
        # Linux
        "/usr/bin/codex"
    )
    $codex = $null
    foreach ($p in $fallbackPaths) {
        if ($p -and (Test-Path $p -ErrorAction SilentlyContinue)) {
            $codex = $p
            Write-Host "[preflight] codex not on PATH, using fallback: $codex" -ForegroundColor Yellow
            break
        }
    }
    if (-not $codex) {
        Write-Error "[preflight] codex binary not found. Install OpenAI Codex (https://openai.com/codex) and ensure 'codex' is on PATH or at one of: Windows $env:USERPROFILE\AppData\Local\OpenAI\Codex\bin\codex.exe ; Mac /usr/local/bin/codex or /Applications/Codex.app/Contents/Resources/codex"
        exit 2
    }
}

$PlanPath = Join-Path $RepoRoot $PlanFile
if (-not (Test-Path $PlanPath)) {
    Write-Error "[preflight] plan file not found: $PlanPath"
    exit 2
}

# --- 2. Extract task block ---
$planContent = Get-Content $PlanPath -Raw
$taskPattern = "(?ms)^##\s*Task\s*$TaskId\b.*?(?=^##\s*Task\s|\z)"
$match = [regex]::Match($planContent, $taskPattern)
if (-not $match.Success) {
    Write-Error "[plan] no '## Task $TaskId' heading found in $PlanFile"
    exit 3
}
$taskBlock = $match.Value.Trim()

# Slug from heading: "## Task 1 — Tiny greet CLI" → "tiny-greet-cli"
$titleLine = ($taskBlock -split "`n")[0]
$titleSlug = ($titleLine -replace '^##\s*Task\s*\d+\s*[—\-:]\s*', '' -replace '[^a-zA-Z0-9]+', '-' -replace '^-|-$', '').ToLower()
if ($titleSlug.Length -gt 40) { $titleSlug = $titleSlug.Substring(0, 40).TrimEnd('-') }
if ([string]::IsNullOrWhiteSpace($titleSlug)) { $titleSlug = "task" }

$branch = "feat/task-$TaskId-$titleSlug"
$worktreeParent = (Resolve-Path "$RepoRoot\..").Path
$repoName = Split-Path $RepoRoot -Leaf
$worktreePath = Join-Path $worktreeParent "$repoName-task-$TaskId"

# --- 3. Verify git state ---
$currentBranch = ((& git -C $RepoRoot rev-parse --abbrev-ref HEAD 2>$null) | Out-String).Trim()
if (-not $currentBranch) {
    Write-Error "[git] not a git repo or no commits yet: $RepoRoot"
    exit 4
}
$baseSha = ((& git -C $RepoRoot rev-parse --verify $BaseBranch 2>$null) | Out-String).Trim()
if (-not $baseSha) {
    Write-Error "[git] base branch '$BaseBranch' not found"
    exit 4
}

if (Test-Path $worktreePath) {
    Write-Error "[worktree] path collision: $worktreePath already exists. Remove with: git worktree remove '$worktreePath'"
    exit 4
}

# --- 4. Create worktree ---
$existingBranch = ((& git -C $RepoRoot branch --list $branch 2>$null) | Out-String).Trim()
Write-Host "[handoff] task $TaskId" -ForegroundColor Cyan
Write-Host "[handoff] branch:   $branch" -ForegroundColor Cyan
Write-Host "[handoff] worktree: $worktreePath" -ForegroundColor Cyan

if ($existingBranch) {
    & git -C $RepoRoot worktree add $worktreePath $branch 2>&1 | Out-Null
} else {
    & git -C $RepoRoot worktree add -b $branch $worktreePath $BaseBranch 2>&1 | Out-Null
}
if ($LASTEXITCODE -ne 0) {
    Write-Error "[git] worktree add failed"
    exit 5
}

# --- 5. Build prompt ---
$projectName = Split-Path $RepoRoot -Leaf
$prompt = @"
You are implementing a task for the ${projectName} project. Project rules live in AGENTS.md at the repo root — read it before editing if you're unfamiliar with the project.

Your working directory is the worktree: $worktreePath
The plan file in the parent repo: $RepoRoot\$PlanFile (read-only reference for context).

Focus on this task only:

$taskBlock

Rules:

1. **Reuse-first.** Before writing any new function, class, or module: grep the repo (`Get-ChildItem -Recurse -Filter '*.R'` + Select-String, or rg-equivalent) for existing utilities that solve part of the problem. If you find one, use it. Forking a near-duplicate is a charter violation. If your task block has a "Reuse existing:" section, treat that as load-bearing — use what it says to use.

2. **TDD discipline.** Write the failing test first (testthat test_that block). Confirm the test fails for the right reason before writing the implementation. The reviewer will check that tests assert real behavior, not `expect_true(TRUE)`.

3. **Engineering Charter compliance** (~/.claude/CLAUDE.md § Engineering Charter — Non-Negotiable). Every change must satisfy all that apply:
   - Functions ≤ 40 LOC, ≤ 1 concern. Split if approaching the limit.
   - Structured returns (named lists with `ok`, `reason`, `payload`) when callers branch on outcome — not bare logicals.
   - Observability at decision points: `message("[tag] ...")` (tag lowercase, ≤ 12 chars).
   - No silent catches — every `tryCatch`/`withCallingHandlers` either logs with context OR rethrows with added context.
   - Idempotency — operations safe to call twice.
   - Boundary-only validation — don't wrap deterministic internal calls in speculative tryCatch.
   - Comments explain WHY, not WHAT. No filler like `# set the delay`.

4. **Anti-patterns to avoid** (these are blocking on review):
   - Code duplication (you scanned in rule 1; don't reinvent)
   - Premature abstraction (S4 class with one implementation)
   - Files > 300 LOC (split into focused modules)
   - Functions with > 3 params (use a named list or ... for options)
   - Magic numbers (extract to named constant with WHY comment)
   - Defensive validation for impossible internal cases
   - Re-implementing base R or well-known package primitives
   - Diff churn unrelated to the task (renames, reformatting adjacent code)

5. **Match existing style.** roxygen2 for docs, testthat edition 3 for tests. Indentation, naming conventions, and assignment operators should match what the repo already uses.

6. **Stay within $worktreePath. Do NOT run git.** Write code + tests only; do NOT run `git add`, `git commit`, or any git write commands. The bridge handles staging and commit after you exit.

7. **Skip install/tests.** Outbound network is blocked by the sandbox — `install.packages()` / `BiocManager::install()` will fail. That is expected. Skip install; the reviewer runs tests during `/codex-review` and `/ship`.

8. **Write `.codex-commit-msg`** in the worktree root, single line, imperative mood, no AI labels. Example: `add UniFrac distance calculation with testthat coverage`. If you reused an existing util, mention which one. The bridge reads this file and uses it for `git commit -m`; the file itself is deleted before staging.

9. **Ambiguity protocol.** If a requirement is ambiguous, write your interpretation as a comment at the top of your first new file, then proceed. Claude will challenge on review if your interpretation is wrong.

10. **Read AGENTS.md** at the worktree root before editing if you're unfamiliar with the project's conventions.

11. **Stop on unexpected sandbox violations.** If a write to the worktree itself fails (not network or .git — those are expected), report and exit cleanly; do not retry with `--dangerously-bypass-approvals-and-sandbox`.

12. **One task per session.** Do not start a second task even if it seems related. Exit cleanly when done.
"@

# --- 6. Invoke Codex ---
$codexArgs = @(
    "exec",
    "--cd", $worktreePath,
    "--sandbox", $Sandbox,
    "--skip-git-repo-check"
)
if ($ModelOverride) { $codexArgs += @("--model", $ModelOverride) }
# Codex writes code only — the bridge handles git staging/commit after Codex exits.

$codexLog = Join-Path $RepoRoot "tasks\.codex-run-$TaskId.log"
Write-Host "[handoff] invoking codex (this may take several minutes)..." -ForegroundColor Cyan

$prompt | & $codex @codexArgs *>&1 | Tee-Object -FilePath $codexLog
$codexExitCode = $LASTEXITCODE

# --- 7. Bridge-side commit (Codex generates code; bridge handles version control) ---
$commitMsgPath = Join-Path $worktreePath ".codex-commit-msg"
$commitMsg = $null
if (Test-Path $commitMsgPath) {
    $commitMsg = (Get-Content $commitMsgPath -Raw).Trim()
}
if ([string]::IsNullOrWhiteSpace($commitMsg)) {
    # Fallback: derive from task title slug
    $titleClean = ($titleLine -replace '^##\s*Task\s*\d+\s*[—\-:]\s*', '').Trim()
    $commitMsg = "add task ${TaskId}: $titleClean"
    Write-Host "[commit] no .codex-commit-msg from Codex; using fallback: $commitMsg" -ForegroundColor Yellow
}

# Check if Codex actually wrote anything
$statusShort = ((& git -C $worktreePath status --porcelain 2>$null) | Out-String).Trim()
# Filter out the commit-msg file itself from the staging set
$stagedAnything = $false
if ($statusShort) {
    # Remove .codex-commit-msg from worktree before staging so it doesn't end up in commit
    if (Test-Path $commitMsgPath) { Remove-Item $commitMsgPath -Force }
    & git -C $worktreePath add -A 2>&1 | Out-Null
    $stagedDiff = ((& git -C $worktreePath diff --cached --shortstat 2>$null) | Out-String).Trim()
    if ($stagedDiff) {
        $stagedAnything = $true
        & git -C $worktreePath -c user.name="codex" -c user.email="codex@local" commit -m $commitMsg 2>&1 | Out-Null
    }
}

# --- 8. Capture result ---
$commits = @()
$filesChanged = @()
$diffStat = ""

$gitLog = & git -C $worktreePath log --oneline "$BaseBranch..HEAD" 2>$null
if ($gitLog) { $commits = @($gitLog -split "`n" | Where-Object { $_ }) }

if ($commits.Count -gt 0) {
    $namesOnly = & git -C $worktreePath diff --name-only "$BaseBranch..HEAD" 2>$null
    if ($namesOnly) { $filesChanged = @($namesOnly -split "`n" | Where-Object { $_ }) }
    $diffStat = ((& git -C $worktreePath diff --shortstat "$BaseBranch..HEAD" 2>$null) | Out-String).Trim()
}

$durationMs = [int]((Get-Date) - $startedAt).TotalMilliseconds
$ok = ($codexExitCode -eq 0 -and $commits.Count -gt 0)

$result = [ordered]@{
    ok            = $ok
    taskId        = $TaskId
    branch        = $branch
    worktree      = $worktreePath
    commits       = $commits
    filesChanged  = $filesChanged
    diffStat      = $diffStat
    commitMessage = $commitMsg
    codexExitCode = $codexExitCode
    durationMs    = $durationMs
    timestamp     = (Get-Date -Format o)
}

if (-not $ok) {
    if ($codexExitCode -ne 0) { $result.reason = "codex exited with code $codexExitCode" }
    elseif (-not $statusShort) { $result.reason = "codex completed but wrote no files" }
    elseif (-not $stagedAnything) { $result.reason = "files present but `git add` produced empty staging set" }
    else { $result.reason = "bridge-side commit failed — see worktree state" }
}

# --- 8. Telemetry: append JSONL line ---
$logLine = ($result | ConvertTo-Json -Compress -Depth 5)
Add-Content -Path (Join-Path $RepoRoot "tasks\handoff-log.jsonl") -Value $logLine

# --- 9. Human output ---
Write-Host ""
Write-Host "=== Handoff Result ===" -ForegroundColor Yellow
$result | ConvertTo-Json -Depth 5

if (-not $ok) { exit 1 }
