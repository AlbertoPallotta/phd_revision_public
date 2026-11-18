# Delivery support. Check selection, exact tree, then listed hashes.
# ExpectedSelection is the fixed delivery path inventory derived from
# the agreed specification, never a directory-generated inventory. Its digest is
# provided independently by the agreed specification/release record.
param([Parameter(Mandatory=$true)][string]$Delivery,
      [Parameter(Mandatory=$true)][string]$Manifest,
      [Parameter(Mandatory=$true)][string]$ExpectedSelection,
      [Parameter(Mandatory=$true)][string]$ExpectedSelectionSHA256,
      [Parameter(Mandatory=$true)][string]$NewLog)
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$taskRoot = (Resolve-Path -LiteralPath $Delivery).Path.TrimEnd('\')
$taskManifestPath = [IO.Path]::GetFullPath($Manifest)
$taskRequiredManifest = [IO.Path]::GetFullPath((Join-Path $taskRoot 'manifests/delivery-files.csv'))
if (!$taskManifestPath.Equals($taskRequiredManifest,[StringComparison]::OrdinalIgnoreCase)) {
  throw 'The only self-exception is Delivery/manifests/delivery-files.csv'
}
$taskLog = [IO.Path]::GetFullPath($NewLog)
$taskMembershipLog = $taskLog + '.membership.json'
foreach ($taskOutput in @($taskLog,$taskMembershipLog)) {
  if ($taskOutput.Equals($taskRoot,[StringComparison]::OrdinalIgnoreCase) -or
      $taskOutput.StartsWith($taskRoot + '\',[StringComparison]::OrdinalIgnoreCase)) {
    throw 'All check logs must remain outside the delivery tree'
  }
  if (Test-Path -LiteralPath $taskOutput) { throw 'Refuse to replace an existing log' }
  $taskParent = Split-Path -Parent $taskOutput
  if (!(Test-Path -LiteralPath $taskParent -PathType Container)) { throw 'Create the external log directory first' }
}
function Get-NormalizedRelativePath([string]$Value) {
  if ([string]::IsNullOrWhiteSpace($Value) -or [IO.Path]::IsPathRooted($Value) -or
      $Value.Contains(':')) { throw 'Empty, rooted or alternate-stream path' }
  $taskParts = $Value.Replace('\','/').Split('/')
  if (@($taskParts | Where-Object { $_ -in @('','.','..') -or $_ -match '[ .]$' }).Count -gt 0) {
    throw ('Noncanonical relative path: ' + $Value)
  }
  $taskFull = [IO.Path]::GetFullPath((Join-Path $taskRoot $Value))
  if (!$taskFull.StartsWith($taskRoot + '\',[StringComparison]::OrdinalIgnoreCase)) { throw 'Path outside delivery' }
  return ($taskParts -join '/').ToLowerInvariant()
}
$taskEvidence = [ordered]@{
  status='FAIL'; selection_status='NOT RUN'; exact_set_status='NOT RUN'; hash_status='NOT RUN'
  expected_selection_sha256=$ExpectedSelectionSHA256
  self_exception='manifests/delivery-files.csv'; missing=@(); extra=@(); error=''
}
$taskChecks = [Collections.Generic.List[object]]::new()
try {
  if ($ExpectedSelectionSHA256 -notmatch '^[0-9a-fA-F]{64}$') { throw 'Missing agreed selection identity' }
  $taskSelectionHash = (Get-FileHash -LiteralPath $ExpectedSelection -Algorithm SHA256).Hash.ToLowerInvariant()
  if ($taskSelectionHash -ne $ExpectedSelectionSHA256.ToLowerInvariant()) { throw 'Selection does not match independently agreed identity' }
  $taskExpected = @{}
  foreach ($taskRow in @(Import-Csv -LiteralPath $ExpectedSelection)) {
    $taskKey = Get-NormalizedRelativePath $taskRow.relative_path
    if ($taskKey -eq 'manifests/delivery-files.csv') { throw 'Self-exception must not appear among expected content rows' }
    if ($taskExpected.ContainsKey($taskKey)) { throw ('Duplicate expected path: ' + $taskKey) }
    if ([string]::IsNullOrWhiteSpace($taskRow.manifest_id)) { throw 'Expected file needs an agreed selection ID' }
    $taskExpected[$taskKey] = $taskRow
  }
  if ($taskExpected.Count -eq 0) { throw 'Empty expected selection' }
  $taskRows = @(Import-Csv -LiteralPath $Manifest)
  $taskSeen = @{}
  foreach ($taskRow in $taskRows) {
    $taskKey = Get-NormalizedRelativePath $taskRow.relative_path
    if ($taskSeen.ContainsKey($taskKey)) { throw ('Duplicate manifest path: ' + $taskKey) }
    if ($taskRow.sha256 -notmatch '^[0-9a-fA-F]{64}$' -or $taskRow.bytes -notmatch '^\d+$') { throw 'Unassigned hash/size' }
    $taskSeen[$taskKey] = $taskRow
  }
  $taskUnapproved = @($taskSeen.Keys | Where-Object { !$taskExpected.ContainsKey($_) })
  $taskOmitted = @($taskExpected.Keys | Where-Object { !$taskSeen.ContainsKey($_) })
  if ($taskUnapproved.Count -or $taskOmitted.Count) {
    $taskEvidence.extra=$taskUnapproved; $taskEvidence.missing=$taskOmitted
    throw 'Delivery manifest differs from the agreed expected selection'
  }
  $taskEvidence.selection_status='PASS'
  $taskActualFiles = @{}
  foreach ($taskItem in @(Get-ChildItem -LiteralPath $taskRoot -Force -Recurse)) {
    if ($taskItem.Attributes -band [IO.FileAttributes]::ReparsePoint) { throw 'Reparse points are not permitted in this delivery' }
    if ($taskItem.PSIsContainer) { continue }
    $taskKey = Get-NormalizedRelativePath ($taskItem.FullName.Substring($taskRoot.Length + 1))
    if ($taskActualFiles.ContainsKey($taskKey)) { throw ('Duplicate normalized actual path: ' + $taskKey) }
    $taskActualFiles[$taskKey] = $taskItem
  }
  if (!$taskActualFiles.ContainsKey('manifests/delivery-files.csv')) { throw 'Missing named self manifest' }
  $taskActualFiles.Remove('manifests/delivery-files.csv')
  $taskEvidence.missing=@($taskExpected.Keys | Where-Object { !$taskActualFiles.ContainsKey($_) } | Sort-Object)
  $taskEvidence.extra=@($taskActualFiles.Keys | Where-Object { !$taskExpected.ContainsKey($_) } | Sort-Object)
  if ($taskEvidence.missing.Count -or $taskEvidence.extra.Count) { throw 'Actual delivery has missing or unexpected files' }
  $taskEvidence.exact_set_status='PASS'
  foreach ($taskKey in @($taskExpected.Keys | Sort-Object)) {
    $taskRow = $taskSeen[$taskKey]; $taskItem = $taskActualFiles[$taskKey]
    $taskActual = (Get-FileHash -LiteralPath $taskItem.FullName -Algorithm SHA256).Hash.ToLowerInvariant()
    $taskStatus = if ($taskActual -eq $taskRow.sha256.ToLowerInvariant() -and $taskItem.Length -eq [long]$taskRow.bytes) { 'PASS' } else { 'FAIL' }
    $taskChecks.Add([pscustomobject]@{relative_path=$taskKey;sha256=$taskActual;bytes=$taskItem.Length;status=$taskStatus})
    if ($taskStatus -eq 'FAIL') { throw ('Identity mismatch: ' + $taskKey) }
  }
  $taskEvidence.hash_status='PASS'; $taskEvidence.status='PASS'
} catch {
  $taskEvidence.error=$_.Exception.Message
} finally {
  if ($taskChecks.Count -gt 0) { $taskChecks | Export-Csv -LiteralPath $taskLog -NoTypeInformation }
  else { 'relative_path,sha256,bytes,status' | Set-Content -LiteralPath $taskLog }
  $taskEvidence | ConvertTo-Json -Depth 6 | Set-Content -LiteralPath $taskMembershipLog
}
if ($taskEvidence.status -ne 'PASS') { throw $taskEvidence.error }
