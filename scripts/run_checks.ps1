param(
  [string]$RscriptPath = "Rscript"
)

$script = Join-Path -Path $PSScriptRoot -ChildPath "run_checks.R"
if (-not (Test-Path $script)) {
  Write-Error "Cannot find $script"
  exit 2
}

Write-Output "Running R script: $script using R executable: $RscriptPath"
& $RscriptPath $script
$code = $LASTEXITCODE
Write-Output "Rscript exit code: $code"
exit $code
