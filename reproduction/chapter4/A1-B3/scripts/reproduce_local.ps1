# Requires the original A1/B3 workspace and historical PT9007 baseline, not this repository's current package copy.
param([string]$NewRoot, [string]$HistoricalRoot=$env:PT_A1B3_ORIGINAL,
      [string]$DependencyLibrary=$env:PT_DEPENDENCY_LIBRARY,
      [string]$BaselineSource=$env:PT_BASELINE_SOURCE,
      [string]$RExecutable=$env:R_EXE, [string]$Rscript=$env:R_SCRIPT)
$ErrorActionPreference='Stop'
foreach ($taskName in @('NewRoot','HistoricalRoot','DependencyLibrary','BaselineSource')) {
 if ([string]::IsNullOrWhiteSpace((Get-Variable -Name $taskName -ValueOnly))) { throw "Missing configuration: supply -$taskName." }
}
$taskOriginal=(Resolve-Path -LiteralPath $HistoricalRoot).Path
$taskNew=[IO.Path]::GetFullPath($NewRoot)
if (Test-Path -LiteralPath $taskNew) { throw 'Output exists; preserving it. Choose another folder.' }
foreach ($taskInput in @($DependencyLibrary,(Join-Path $BaselineSource 'DESCRIPTION'),
 (Join-Path $taskOriginal 'PortfolioTesteR/DESCRIPTION'),
 (Join-Path $taskOriginal 'PortfolioTesteR_0.1.4.9008.tar.gz'),
 (Join-Path $taskOriginal 'thesis-reproduction/evidence/bundled-grouped-date-grid.csv'))) {
 if (!(Test-Path -LiteralPath $taskInput)) { throw "Missing historical input: $taskInput" }
}
if ((Get-Content -LiteralPath (Join-Path $BaselineSource 'DESCRIPTION') -Raw) -notmatch '(?m)^Version: 0\.1\.4\.9007\s*$') { throw 'Historical PT9007 baseline source required.' }
$taskHash=(Get-FileHash -LiteralPath (Join-Path $taskOriginal 'PortfolioTesteR_0.1.4.9008.tar.gz') -Algorithm SHA256).Hash.ToLowerInvariant()
if ($taskHash -ne 'd0cf3140f84295213418070b7c9b700b6951489baa30b0d96b9131aabcf978dc') { throw 'Historical A1/B3 PT9008 archive identity mismatch; do not substitute a current archive.' }
foreach ($taskPair in @(@('RExecutable','R'),@('Rscript','Rscript'))) {
 if ([string]::IsNullOrWhiteSpace((Get-Variable -Name $taskPair[0] -ValueOnly))) {
  $taskCommand=Get-Command $taskPair[1] -CommandType Application -ErrorAction SilentlyContinue
  if (!$taskCommand) { throw ("Missing configuration: supply -"+$taskPair[0]+" or put "+$taskPair[1]+" on PATH.") }
  Set-Variable -Name $taskPair[0] -Value $taskCommand.Source
 }
 if (!(Test-Path -LiteralPath (Get-Variable -Name $taskPair[0] -ValueOnly) -PathType Leaf)) { throw 'Missing R executable.' }
}
$env:PT_BASELINE_SOURCE=(Resolve-Path -LiteralPath $BaselineSource).Path
New-Item -ItemType Directory -Path $taskNew | Out-Null
Copy-Item -LiteralPath (Join-Path $taskOriginal 'PortfolioTesteR') -Destination $taskNew -Recurse
Copy-Item -LiteralPath (Join-Path $taskOriginal 'PortfolioTesteR_0.1.4.9008.tar.gz') -Destination $taskNew
$taskRep=Join-Path $taskNew 'thesis-reproduction'
New-Item -ItemType Directory -Path (Join-Path $taskRep 'evidence') -Force | Out-Null
Copy-Item -LiteralPath $PSScriptRoot -Destination $taskRep -Recurse
Copy-Item -LiteralPath (Join-Path $taskOriginal 'thesis-reproduction/evidence/bundled-grouped-date-grid.csv') -Destination (Join-Path $taskRep 'evidence')
New-Item -ItemType Directory -Path (Join-Path $taskNew 'library-candidate') | Out-Null
$env:R_LIBS_USER=(Join-Path $taskNew 'library-candidate')+';'+$DependencyLibrary
$env:R_LIBS=$env:R_LIBS_USER
$env:R_ENVIRON_USER='NUL';$env:R_PROFILE_USER='NUL';$env:R_LIBS_SITE=''
& $RExecutable CMD INSTALL ('--library='+(Join-Path $taskNew 'library-candidate')) (Join-Path $taskNew 'PortfolioTesteR_0.1.4.9008.tar.gz') *> (Join-Path $taskNew 'install.log')
if ($LASTEXITCODE -ne 0) { throw 'Candidate installation failed; see install.log.' }
foreach ($taskStep in @('verify_identity','synthetic_checks','run_study','validate_results','make_figures','verify_figure_data')) {
 & pwsh -NoProfile -File (Join-Path $taskRep 'scripts/run-r.ps1') -Script (Join-Path $taskRep ('scripts/'+$taskStep+'.R')) -Log (Join-Path $taskRep ('logs/'+$taskStep+'.log')) -DependencyLibrary $DependencyLibrary -Root $taskNew -Rscript $Rscript
 if ($LASTEXITCODE -ne 0) { throw "Stopped at $taskStep; retained all outputs." }
}
Write-Output "Completed a NEW 41-evaluation reproduction at $taskNew. Original evidence preserved."
