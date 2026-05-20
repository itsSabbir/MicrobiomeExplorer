<#
.SYNOPSIS
  Remove stale task worktrees and prune feature branches.

.PARAMETER DryRun
  Show what would be removed without doing it.
#>
param([switch]$DryRun)

$RepoRoot = (Resolve-Path "$PSScriptRoot\..").Path
Set-Location $RepoRoot

Write-Host "=== Worktrees registered with git ===" -ForegroundColor Cyan
$worktrees = & git worktree list --porcelain | Out-String
Write-Host $worktrees

$parent = (Resolve-Path "$RepoRoot\..").Path
$repoName = Split-Path $RepoRoot -Leaf
$pattern = "$repoName-task-*"

Write-Host "=== Candidate stale worktree dirs ($pattern) ===" -ForegroundColor Cyan
$candidates = Get-ChildItem -Path $parent -Directory -Filter $pattern -ErrorAction SilentlyContinue

if (-not $candidates) {
    Write-Host "  (none)"
} else {
    foreach ($c in $candidates) {
        $action = if ($DryRun) { "would remove" } else { "removing" }
        Write-Host "  $action $($c.FullName)"
        if (-not $DryRun) {
            & git worktree remove $c.FullName --force 2>&1 | Out-Null
            if (Test-Path $c.FullName) { Remove-Item -Recurse -Force $c.FullName }
        }
    }
}

Write-Host "=== Pruning git worktree metadata ===" -ForegroundColor Cyan
if ($DryRun) {
    & git worktree prune --dry-run --verbose
} else {
    & git worktree prune --verbose
}

Write-Host "=== Done ===" -ForegroundColor Green
