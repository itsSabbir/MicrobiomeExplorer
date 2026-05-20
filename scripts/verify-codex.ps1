<#
.SYNOPSIS
  Preflight check for the bridge: codex installed, on PATH, authenticated, git available.

.EXAMPLE
  pwsh -File scripts/verify-codex.ps1
#>

$ErrorActionPreference = "Continue"
$ok = $true

function Check($label, $scriptblock) {
    Write-Host -NoNewline "  $label ... "
    try {
        $result = & $scriptblock
        if ($result) { Write-Host "OK ($result)" -ForegroundColor Green }
        else         { Write-Host "OK" -ForegroundColor Green }
        return $true
    } catch {
        Write-Host "FAIL — $_" -ForegroundColor Red
        return $false
    }
}

Write-Host "=== codex-claude-bridge preflight ===" -ForegroundColor Cyan

$ok = (Check "PowerShell 7+" {
    if ($PSVersionTable.PSVersion.Major -lt 7) { throw "need pwsh 7+, have $($PSVersionTable.PSVersion)" }
    "$($PSVersionTable.PSVersion)"
}) -and $ok

$ok = (Check "git available" {
    $v = (& git --version 2>&1)
    if ($LASTEXITCODE -ne 0) { throw "git not on PATH" }
    $v
}) -and $ok

$script:codexPath = $null
$ok = (Check "codex available (PATH or fallback)" {
    $c = Get-Command codex -ErrorAction SilentlyContinue
    if ($c) {
        $script:codexPath = $c.Source
        return "PATH: $($c.Source)"
    }
    $fallback = "C:\Users\sabbir\AppData\Local\OpenAI\Codex\bin\codex.exe"
    if (Test-Path $fallback) {
        $script:codexPath = $fallback
        return "fallback: $fallback (PATH-update may need a fresh terminal)"
    }
    throw "codex not on PATH and fallback missing. Install OpenAI Codex desktop app."
}) -and $ok

$ok = (Check "codex --version" {
    $v = (& $script:codexPath --version 2>&1)
    if ($LASTEXITCODE -ne 0) { throw "codex --version failed: $v" }
    $v
}) -and $ok

$ok = (Check "codex authenticated" {
    $authJson = "$env:USERPROFILE\.codex\auth.json"
    if (-not (Test-Path $authJson)) { throw "no $authJson — run 'codex login'" }
    "auth.json present"
}) -and $ok

$ok = (Check "codex config.toml" {
    $cfg = "$env:USERPROFILE\.codex\config.toml"
    if (-not (Test-Path $cfg)) { throw "no $cfg" }
    "config.toml present"
}) -and $ok

$ok = (Check "R available (PATH or standard install)" {
    $rCmd = Get-Command Rscript -ErrorAction SilentlyContinue
    if ($rCmd) {
        $v = (& $rCmd.Source --version 2>&1)
        return "PATH: $v"
    }
    # Fallback: standard Windows install location
    $rDirs = Get-ChildItem "C:\Program Files\R" -Directory -ErrorAction SilentlyContinue | Sort-Object Name -Descending
    foreach ($d in $rDirs) {
        $rExe = Join-Path $d.FullName "bin\Rscript.exe"
        if (Test-Path $rExe) {
            $v = (& $rExe --version 2>&1)
            return "fallback: $rExe ($v)"
        }
    }
    throw "Rscript not on PATH and not found in C:\Program Files\R\. Install R or add bin dir to PATH."
}) -and $ok

Write-Host ""
if ($ok) {
    Write-Host "All checks passed. Bridge is ready." -ForegroundColor Green
    exit 0
} else {
    Write-Host "One or more checks failed. Fix above before running /handoff." -ForegroundColor Red
    exit 1
}
