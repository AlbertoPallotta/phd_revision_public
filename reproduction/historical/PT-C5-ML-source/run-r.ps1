# Historical Chapter 5 workspace/library must be supplied explicitly.
param([string]$Script,[string]$Log,[string[]]$ScriptArgs=@(),
 [string]$HistoricalRoot=$env:PT_C5_ROOT,[string]$HistoricalLibrary=$env:PT_C5_LIBRARY,
 [string[]]$DependencyLibraries=@(),[string]$Rscript=$env:R_SCRIPT)
$ErrorActionPreference='Stop'
foreach ($taskName in @('Script','Log','HistoricalRoot','HistoricalLibrary')) {
 if ([string]::IsNullOrWhiteSpace((Get-Variable -Name $taskName -ValueOnly))) { throw "Missing configuration: supply -$taskName." }
}
foreach ($taskInput in @($Script,$HistoricalRoot,(Join-Path $HistoricalLibrary 'PortfolioTesteR'))+$DependencyLibraries) {
 if (!(Test-Path -LiteralPath $taskInput)) { throw "Missing historical input/library: $taskInput" }
}
if (Test-Path -LiteralPath $Log) { throw 'Log exists; choose a new log path.' }
if ([string]::IsNullOrWhiteSpace($Rscript)) {
 $taskCommand=Get-Command Rscript -CommandType Application -ErrorAction SilentlyContinue
 if (!$taskCommand) { throw 'Missing configuration: supply -Rscript or put Rscript on PATH.' }
 $Rscript=$taskCommand.Source
}
if (!(Test-Path -LiteralPath $Rscript -PathType Leaf)) { throw 'Missing Rscript executable.' }
$env:PT_C5_ROOT=(Resolve-Path -LiteralPath $HistoricalRoot).Path
$env:PT_C5_LIBRARY=(Resolve-Path -LiteralPath $HistoricalLibrary).Path
$taskLibraries=@($env:PT_C5_LIBRARY)+$DependencyLibraries
$env:R_LIBS_USER=$taskLibraries -join ';'
$env:R_LIBS=$env:R_LIBS_USER
$env:R_LIBS_SITE=''
$env:R_ENVIRON_USER='NUL'
$env:R_PROFILE_USER='NUL'
$env:TZ='Europe/London'
$env:LC_ALL='English_United Kingdom.utf8'
$env:OMP_NUM_THREADS='1'
$env:OPENBLAS_NUM_THREADS='1'
$env:NOT_CRAN='false'
$env:RUN_LIVE='false'
& $Rscript --vanilla $Script @ScriptArgs *> $Log
$taskExit=$LASTEXITCODE
Get-Content -LiteralPath $Log -Tail 45
exit $taskExit
