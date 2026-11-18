# Historical A1/B3 workspace only; no current-package or sample-data substitution.
param([string]$Script, [string]$Log, [string[]]$ScriptArgs=@(),
      [string]$Root=$env:PT_A1B3_ROOT,
      [string]$DependencyLibrary=$env:PT_DEPENDENCY_LIBRARY,
      [string]$Rscript=$env:R_SCRIPT)
$ErrorActionPreference='Stop'
foreach ($taskName in @('Script','Log','Root','DependencyLibrary')) {
 if ([string]::IsNullOrWhiteSpace((Get-Variable -Name $taskName -ValueOnly))) { throw "Missing configuration: supply -$taskName." }
}
$taskRoot=(Resolve-Path -LiteralPath $Root).Path
foreach ($taskInput in @($Script,$DependencyLibrary,(Join-Path $taskRoot 'library-candidate/PortfolioTesteR'))) {
 if (!(Test-Path -LiteralPath $taskInput)) { throw "Missing historical input/library: $taskInput" }
}
if ((Test-Path -LiteralPath $Log) -or (Test-Path -LiteralPath ($Log+'.exit.txt'))) { throw 'Log exists; choose a new log path.' }
if ([string]::IsNullOrWhiteSpace($Rscript)) {
 $taskCommand=Get-Command Rscript -CommandType Application -ErrorAction SilentlyContinue
 if (!$taskCommand) { throw 'Missing configuration: supply -Rscript or put Rscript on PATH.' }
 $Rscript=$taskCommand.Source
}
if (!(Test-Path -LiteralPath $Rscript -PathType Leaf)) { throw 'Missing Rscript executable.' }
$taskLibs=@((Join-Path $taskRoot 'library-candidate'),$DependencyLibrary)
$env:R_LIBS_USER = $taskLibs -join ';'
$env:R_LIBS = $env:R_LIBS_USER
$env:R_LIBS_SITE = ''
$env:R_ENVIRON_USER = 'NUL'
$env:R_PROFILE_USER = 'NUL'
$env:LC_ALL = 'C'
$env:LANG = 'C'
$env:TZ = 'Europe/London'
$env:NOT_CRAN = 'false'
$env:RUN_SEQ = 'false'
$env:RUN_KERAS_VIGNETTES = 'false'
$env:RUN_LIVE = 'false'
$env:OMP_NUM_THREADS = '1'
$env:OPENBLAS_NUM_THREADS = '1'
$env:_R_CHECK_FORCE_SUGGESTS_ = 'false'
$env:_R_CHECK_CRAN_INCOMING_REMOTE_ = 'false'
# Use an existing RSTUDIO_PANDOC setting, if needed; no machine-specific default.
if ($env:RSTUDIO_PANDOC) { $env:PATH = $env:RSTUDIO_PANDOC + ';' + $env:PATH }
$env:PT_BUILDER_ROOT = $taskRoot
New-Item -ItemType Directory -Force -Path (Split-Path -Parent $Log) | Out-Null
& $Rscript --vanilla $Script @ScriptArgs *> $Log
$taskCode = $LASTEXITCODE
Set-Content -LiteralPath ($Log + '.exit.txt') -Value $taskCode
Get-Content -LiteralPath $Log -Tail 35
exit $taskCode
