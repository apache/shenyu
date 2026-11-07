# Licensed to the Apache Software Foundation (ASF) under one or more
# contributor license agreements. See the NOTICE file distributed with
# this work for additional information regarding copyright ownership.
# The ASF licenses this file to You under the Apache License, Version 2.0
# (the "License"); you may not use this file except in compliance with
# the License. You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

param(
    [ValidateRange(100, 1000000)][int]$Requests = 2000,
    [ValidateRange(1, 1000000)][int[]]$Rates = @(500),
    [ValidateRange(3, 100)][int]$Repeats = 3,
    [ValidateRange(1, 128)][int]$Conditions = 2,
    [ValidateRange(1, 128)][int]$Burst = 32,
    [ValidateRange(60, 3600)][int]$TimeoutSeconds = 180,
    [switch]$Profile,
    [switch]$CompileOnly
)
$ErrorActionPreference = 'Stop'
$workspace = (Resolve-Path (Join-Path $PSScriptRoot '../../..')).Path
$module = Join-Path $workspace 'shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-divide'
$baseline = '548bec1f9'
$java = Join-Path $env:JAVA_HOME 'bin/java.exe'
$javac = Join-Path $env:JAVA_HOME 'bin/javac.exe'
$dependenciesFile = Join-Path $module 'target/gateway-classpath.txt'
if (!(Test-Path -LiteralPath $dependenciesFile)) {
    throw 'Existing target/gateway-classpath.txt is required. This script never invokes Maven or downloads dependencies.'
}
$dependencies = (Get-Content -Raw $dependenciesFile).Trim()
foreach ($entry in $dependencies.Split(';')) {
    if (!$entry -or !(Test-Path -LiteralPath $entry)) { throw "Missing installed dependency: $entry" }
}
# Local changed classes MUST precede all installed jars, including old common/base jars.
$modules = @('shenyu-common', 'shenyu-plugin/shenyu-plugin-base', 'shenyu-plugin/shenyu-plugin-api',
    'shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-divide')
$localClasses = @($modules | ForEach-Object { Join-Path $workspace "$_/target/classes" })
foreach ($directory in $localClasses) {
    if (!(Test-Path -LiteralPath $directory)) { throw "Missing compiled classes: $directory" }
}
# Reuse local supporting modules when available; older installed gateway jars can be binary-incompatible.
foreach ($support in @('shenyu-loadbalancer', 'shenyu-web', 'shenyu-plugin/shenyu-plugin-global',
        'shenyu-plugin/shenyu-plugin-httpclient', 'shenyu-plugin/shenyu-plugin-response', 'shenyu-plugin/shenyu-plugin-uri')) {
    $directory = Join-Path $workspace "$support/target/classes"
    if (Test-Path -LiteralPath $directory) { $localClasses += $directory }
}
$classpath = ($localClasses -join ';') + ';' + $dependencies
$sourcePaths = @(
    'shenyu-common/src/main/java/org/apache/shenyu/common/dto/BaseData.java',
    'shenyu-common/src/main/java/org/apache/shenyu/common/dto/ConditionData.java',
    'shenyu-common/src/main/java/org/apache/shenyu/common/dto/PluginData.java',
    'shenyu-common/src/main/java/org/apache/shenyu/common/dto/RuleData.java',
    'shenyu-common/src/main/java/org/apache/shenyu/common/dto/SelectorData.java',
    'shenyu-plugin/shenyu-plugin-base/src/main/java/org/apache/shenyu/plugin/base/cache/BaseDataCache.java',
    'shenyu-plugin/shenyu-plugin-base/src/main/java/org/apache/shenyu/plugin/base/cache/MatchDataCache.java',
    'shenyu-plugin/shenyu-plugin-base/src/main/java/org/apache/shenyu/plugin/base/AbstractShenyuPlugin.java',
    'shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-divide/src/main/java/org/apache/shenyu/plugin/divide/DividePlugin.java'
)
# Do not silently benchmark stale target/classes. Build prerequisites separately, never here.
foreach ($relative in ($sourcePaths + 'shenyu-common/src/main/java/org/apache/shenyu/common/dto/DtoSnapshots.java')) {
    $source = Join-Path $workspace $relative
    $compiled = Join-Path $workspace ($relative.Replace('/src/main/java/', '/target/classes/').Replace('.java', '.class'))
    if (!(Test-Path -LiteralPath $compiled) -or (Get-Item $source).LastWriteTimeUtc -gt (Get-Item $compiled).LastWriteTimeUtc) {
        throw "Missing/stale changed class: $compiled"
    }
}
$run = Join-Path $module ('target/config-views-' + (Get-Date -Format 'yyyyMMdd-HHmmss') + '-' + [guid]::NewGuid().ToString('N').Substring(0, 8))
$classes = Join-Path $run 'classes'
$baselineClasses = Join-Path $run 'baseline-classes'
$baselineSource = Join-Path $run 'baseline-source'
$argumentsDirectory = Join-Path $env:TEMP ('shenyu-config-views-' + [guid]::NewGuid().ToString('N'))
New-Item -ItemType Directory -Path $classes,$baselineClasses,$baselineSource,$argumentsDirectory | Out-Null
$utf8 = [Text.UTF8Encoding]::new($false)
$processes = [Collections.Generic.List[Diagnostics.Process]]::new()
$logback = Join-Path $run 'logback.xml'
[IO.File]::WriteAllText($logback, '<configuration><root level="OFF"/></configuration>', $utf8)
function Stop-OwnedProcess([Diagnostics.Process]$Process) {
    if (!$Process.HasExited) {
        $Process.Kill()
        if (!$Process.WaitForExit(5000)) { throw "Could not stop owned process $($Process.Id)" }
    }
}
function Start-Tool([string]$Executable, [string]$Name, [string[]]$ToolArgs) {
    $argumentFile = Join-Path $argumentsDirectory ($Name + '.args')
    $lines = @($ToolArgs | ForEach-Object { '"' + $_.Replace('\', '/').Replace('"', '\"') + '"' })
    [IO.File]::WriteAllLines($argumentFile, $lines, $utf8)
    $process = Start-Process -FilePath $Executable -ArgumentList "@`"$argumentFile`"" -WindowStyle Hidden -PassThru `
        -RedirectStandardOutput (Join-Path $run ($Name + '.stdout.log')) -RedirectStandardError (Join-Path $run ($Name + '.stderr.log'))
    $processes.Add($process)
    return $process
}
function Wait-Tool([Diagnostics.Process]$Process, [string]$Name) {
    $null = $Process.Handle
    if (!$Process.WaitForExit($TimeoutSeconds * 1000)) {
        Stop-OwnedProcess $Process
        throw "$Name timed out; inspect $run"
    }
    # Refresh the process handle before reading its exit status on Windows PowerShell 5.1.
    $Process.Refresh()
    if ($Process.ExitCode -ne 0) { throw "$Name failed; inspect $run" }
}
function Start-Java([string]$Name, [string]$ClassPath, [string[]]$ModeArgs, [bool]$Record = $false) {
    $options = @('-Xms512m', '-Xmx512m', '-XX:ActiveProcessorCount=8', "-Dlogback.configurationFile=$logback")
    if ($Record) { $options += "-XX:StartFlightRecording=filename=$argumentsDirectory/$Name.jfr,settings=profile,dumponexit=true" }
    return Start-Tool $java $Name ($options + @('-cp', $ClassPath, 'org.apache.shenyu.benchmark.DivideGatewayBenchmark') + $ModeArgs)
}
function Wait-Ready([string]$Path, [Diagnostics.Process]$Process) {
    $deadline = [DateTime]::UtcNow.AddSeconds([Math]::Min(60, $TimeoutSeconds))
    while (!(Test-Path -LiteralPath $Path)) {
        if ($Process.HasExited) { throw "Process exited before readiness; inspect $run" }
        if ([DateTime]::UtcNow -gt $deadline) { throw "Readiness timed out: $Path" }
        Start-Sleep -Milliseconds 100
    }
}
try {
    # Export ONLY the nine baseline classes. DtoSnapshots did not exist at the baseline.
    $sources = @()
    foreach ($relative in $sourcePaths) {
        $name = [IO.Path]::GetFileName($relative)
        $export = Start-Process -FilePath 'git' -ArgumentList @('-C', "`"$workspace`"", 'show', "${baseline}:$relative") `
            -WindowStyle Hidden -PassThru -RedirectStandardOutput (Join-Path $baselineSource $name) `
            -RedirectStandardError (Join-Path $run ($name + '.git.stderr.log'))
        $processes.Add($export)
        Wait-Tool $export "git show $relative"
        $sources += Join-Path $baselineSource $name
    }
    $compile = Start-Tool $javac 'compile-baseline' (@('--release', '17', '-encoding', 'UTF-8', '-cp', $classpath, '-d', $baselineClasses) + $sources)
    Wait-Tool $compile 'Baseline compilation'
    # Compile the common harness against baseline APIs: no accidental linkage to readOnly/DtoSnapshots.
    $compile = Start-Tool $javac 'compile-benchmark' @('--release', '17', '-encoding', 'UTF-8', '-cp', "$baselineClasses;$classpath", '-d', $classes,
        (Join-Path $PSScriptRoot 'DivideGatewayBenchmark.java'))
    Wait-Tool $compile 'Benchmark compilation'
    [IO.File]::WriteAllText((Join-Path $run 'configuration.json'), (@{
        baseline=$baseline; requests=$Requests; offeredRates=$Rates; repeats=$Repeats; conditionsPerEntry=$Conditions;
        burst=$Burst; profile=$Profile.IsPresent; heapMiB=512; activeProcessors=8;
        dependencyClasspathSHA256=(Get-FileHash $dependenciesFile -Algorithm SHA256).Hash
    } | ConvertTo-Json), $utf8)
    if ($CompileOnly) { Write-Output "COMPILED=$run (no benchmark processes started)"; return }
    $fixtureClasspath = "$classes;$classpath"
    $upstreamFile = Join-Path $run 'upstreams.json'
    $upstream = Start-Java 'upstream' $fixtureClasspath @('upstream', $upstreamFile)
    Wait-Ready $upstreamFile $upstream
    $rows = [Collections.Generic.List[object]]::new()
    foreach ($rate in $Rates) {
        foreach ($size in @(1,128)) {
            foreach ($state in @('matched','no-match','cache-hit')) {
                foreach ($updates in @(0,$Burst)) {
                    for ($repeat = 0; $repeat -lt $Repeats; $repeat++) {
                        $variants = if ($repeat % 2 -eq 0) { @('baseline','change') } else { @('change','baseline') }
                        foreach ($variant in $variants) {
                            $name = "$variant-$size-$state-b$updates-r$rate-$repeat"
                            $ready = Join-Path $run ($name + '.port')
                            $variantClasspath = if ($variant -eq 'baseline') { "$baselineClasses;$fixtureClasspath" } else { $fixtureClasspath }
                            # Fresh, sequential gateway JVMs avoid idle sibling interference and accumulated retained state.
                            $gateway = Start-Java $name $variantClasspath @('gateway', $upstreamFile, $ready, 'config-views') $Profile.IsPresent
                            try {
                                Wait-Ready $ready $gateway
                                $port = (Get-Content -Raw $ready).Trim()
                                $setup = Invoke-RestMethod "http://127.0.0.1:$port/_bench/config-view?size=$size&conditions=$Conditions&state=$state" `
                                    -TimeoutSec $TimeoutSeconds
                                [IO.File]::WriteAllText((Join-Path $run ($name + '.setup.json')), ($setup | ConvertTo-Json -Depth 5), $utf8)
                                $resultPath = Join-Path $run ($name + '.json')
                                $load = Start-Java ($name + '-load') $fixtureClasspath @('load-config', $port, "$Requests", "$rate", $resultPath, $state, "$size", "$updates")
                                Wait-Tool $load $name
                                $data = Get-Content -Raw $resultPath | ConvertFrom-Json
                                if ($data.successes -ne $Requests -or $data.errors -ne 0) { throw "Unexpected HTTP responses: $name" }
                                if (!$data.retained_before.gc_observed -or !$data.retained_after.gc_observed) { throw "Explicit GC was not observed: $name" }
                                if ($updates -gt 0 -and !$data.update.firstRequestVisible) { throw "Publication visibility failed: $name" }
                                $rows.Add([PSCustomObject]@{
                                    variant=$variant; selectors=$size; rules_per_selector=$size; conditions=$Conditions; state=$state; updates=$updates; repeat=$repeat
                                    offered_rate=$rate; requests=$data.requests; expected_responses=$data.successes; errors=$data.errors
                                    observed_rate=[Math]::Round($data.requests * 1e9 / $data.elapsed_ns,2); upstream_successes=$data.upstream_successes
                                    allocated_bytes_per_request=[Math]::Round($data.allocated_bytes / $data.requests,2); cpu_ns_per_request=[Math]::Round($data.cpu_ns / $data.requests,2)
                                    p95_ms=$data.p95_ns / 1e6; p99_ms=$data.p99_ns / 1e6; scheduled_p95_ms=$data.scheduled_p95_ns / 1e6; scheduled_p99_ms=$data.scheduled_p99_ns / 1e6
                                    gc_count=$data.gc_count; gc_ms=$data.gc_ms; sampled_peak_heap_bytes=$data.sampled_peak_heap_bytes
                                    retained_before_bytes=$data.retained_before.heap_bytes; retained_after_bytes=$data.retained_after.heap_bytes
                                    update_ns=$data.update.updateNs; max_update_ns=$data.update.maxUpdateNs; first_visible_ns=$data.update.firstVisibleNs
                                    setup_ns=$setup.setup.elapsed_ns; setup_allocated_bytes=$setup.setup.allocated_bytes; setup_cpu_ns=$setup.setup.cpu_ns
                                    setup_gc_count=$setup.setup.gc_count; setup_gc_ms=$setup.setup.gc_ms
                                    setup_heap_before_bytes=$setup.setup.heap_before_bytes; setup_heap_after_bytes=$setup.setup.heap_after_bytes
                                })
                                $rows | Export-Csv -NoTypeInformation -Encoding utf8 (Join-Path $run 'results.csv')
                                Write-Output "$name completed (observed rate, NOT capacity)"
                                if ($Profile) {
                                    # jcmd does not accept Java @argfiles. Bound it like every other child process.
                                    $recording = Join-Path $argumentsDirectory ($name + '.jfr')
                                    $dump = Start-Process -FilePath (Join-Path $env:JAVA_HOME 'bin/jcmd.exe') `
                                        -ArgumentList @($gateway.Id, 'JFR.dump', "`"filename=$recording`"") -WindowStyle Hidden -PassThru `
                                        -RedirectStandardOutput (Join-Path $run ($name + '.jfr.log')) -RedirectStandardError (Join-Path $run ($name + '.jfr.stderr.log'))
                                    $processes.Add($dump)
                                    Wait-Tool $dump 'JFR dump'
                                    Copy-Item -LiteralPath $recording -Destination (Join-Path $run ($name + '.jfr'))
                                }
                            } finally { Stop-OwnedProcess $gateway }
                        }
                    }
                }
            }
        }
    }
    Write-Output "RESULTS=$run"
} finally {
    foreach ($process in $processes) {
        try { Stop-OwnedProcess $process } catch { Write-Warning $_ }
        $process.Dispose()
    }
    Remove-Item -LiteralPath $argumentsDirectory -Recurse -Force
}
