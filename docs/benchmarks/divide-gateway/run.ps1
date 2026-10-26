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
    [int]$Requests = 2000,
    [int]$Rate = 500,
    [int]$Samples = 3,
    [switch]$Smoke,
    [switch]$Profile,
    [switch]$SkipVerify
)
$ErrorActionPreference = 'Stop'
$workspace = (Resolve-Path (Join-Path $PSScriptRoot '../../..')).Path
$module = Join-Path $workspace 'shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-divide'
$java = Join-Path $env:JAVA_HOME 'bin/java.exe'
$javac = Join-Path $env:JAVA_HOME 'bin/javac.exe'
$run = Join-Path $module ('target/gateway-' + (Get-Date -Format 'yyyyMMdd-HHmmss'))
New-Item -ItemType Directory -Path $run -Force | Out-Null
$argumentDirectory = Join-Path $env:TEMP ('shenyu-gateway-' + [guid]::NewGuid().ToString('N'))
New-Item -ItemType Directory -Path $argumentDirectory | Out-Null
$dependencies = (Get-Content -Raw (Join-Path $module 'target/gateway-classpath.txt')).Trim()
$classes = Join-Path $run 'classes'
$baselineClasses = Join-Path $run 'baseline-classes'
New-Item -ItemType Directory -Path $classes,$baselineClasses -Force | Out-Null
$classpath = (Join-Path $module 'target/classes') + ';' + $dependencies
& $javac --release 17 -cp $classpath -d $classes (Join-Path $PSScriptRoot 'DivideGatewayBenchmark.java')
if ($LASTEXITCODE -ne 0) { throw 'Benchmark compilation failed' }
$baselineSource = Join-Path $run 'baseline-source'
New-Item -ItemType Directory -Path $baselineSource | Out-Null
$source = git -C $workspace show '581bee9f1:shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-divide/src/main/java/org/apache/shenyu/plugin/divide/DividePlugin.java'
if ($LASTEXITCODE -ne 0) { throw 'Baseline revision is unavailable' }
[IO.File]::WriteAllLines((Join-Path $baselineSource 'DividePlugin.java'), $source)
$breakerPath = Join-Path $workspace 'shenyu-plugin/shenyu-plugin-base/src/main/java/org/apache/shenyu/plugin/base/circuitbreaker/UpstreamCircuitBreaker.java'
$breaker = (Get-Content -Raw $breakerPath).Replace('return upstream.buildSelectorKey(selectorId);', 'return selectorId + ":" + upstream.buildDomain();')
[IO.File]::WriteAllText((Join-Path $baselineSource 'UpstreamCircuitBreaker.java'), $breaker)
& $javac --release 17 -cp $classpath -d $baselineClasses (Join-Path $baselineSource 'DividePlugin.java') (Join-Path $baselineSource 'UpstreamCircuitBreaker.java')
if ($LASTEXITCODE -ne 0) { throw 'Baseline compilation failed' }
$logback = Join-Path $run 'logback.xml'
[IO.File]::WriteAllText($logback, '<configuration><root level="OFF"/></configuration>')
$processes = [Collections.Generic.List[Diagnostics.Process]]::new()
function Start-BenchmarkJava([string]$Name, [string]$ClassPath, [string[]]$ModeArgs, [bool]$Record = $false) {
    $arguments = @('-Xms256m', '-Xmx256m', '-XX:ActiveProcessorCount=8', "-Dlogback.configurationFile=$logback")
    if ($Record) { $arguments += "-XX:StartFlightRecording=filename=$argumentDirectory/$Name.jfr,settings=profile,dumponexit=true" }
    $arguments += @('-cp', $ClassPath, 'org.apache.shenyu.benchmark.DivideGatewayBenchmark') + $ModeArgs
    $argumentFile = Join-Path $argumentDirectory ($Name + '.args')
    $lines = $arguments | ForEach-Object { '"' + $_.Replace('\','/').Replace('"','\"') + '"' }
    [IO.File]::WriteAllLines($argumentFile, $lines)
    $process = Start-Process -FilePath $java -ArgumentList @("@$argumentFile") -WindowStyle Hidden -PassThru `
        -RedirectStandardOutput (Join-Path $run ($Name + '.stdout.log')) -RedirectStandardError (Join-Path $run ($Name + '.stderr.log'))
    return $process
}
function Wait-Ready([string]$Path, [Diagnostics.Process]$Process) {
    $deadline = [DateTime]::UtcNow.AddSeconds(40)
    while (!(Test-Path -LiteralPath $Path)) {
        if ($Process.HasExited) { throw "Process exited before publishing $Path; inspect $run" }
        if ([DateTime]::UtcNow -gt $deadline) { throw "Timed out waiting for $Path" }
        Start-Sleep -Milliseconds 100
    }
}
try {
    $fixtureClasspath = $classes + ';' + $classpath
    $upstreamFile = Join-Path $run 'upstreams.json'
    $upstream = Start-BenchmarkJava 'upstream' $fixtureClasspath @('upstream', $upstreamFile)
    $processes.Add($upstream)
    Wait-Ready $upstreamFile $upstream
    $ports = @{}
    foreach ($variant in @('baseline','optimized')) {
        $ready = Join-Path $run ($variant + '.port')
        $variantClasspath = if ($variant -eq 'baseline') { $baselineClasses + ';' + $fixtureClasspath } else { $fixtureClasspath }
        $gateway = Start-BenchmarkJava $variant $variantClasspath @('gateway', $upstreamFile, $ready) $Profile.IsPresent
        $processes.Add($gateway)
        Wait-Ready $ready $gateway
        $ports[$variant] = (Get-Content -Raw $ready).Trim()
    }
    $rows = [Collections.Generic.List[object]]::new()
    $sizes = if ($Smoke) { @(1) } else { @(1,10,128) }
    $states = if ($Smoke) { @('healthy') } else { @('healthy','mixed','blocked') }
    if ($Smoke) { $Samples = 1; $Requests = 200; $Rate = 200 }
    foreach ($size in $sizes) {
        foreach ($state in $states) {
            for ($sample = 0; $sample -lt $Samples; $sample++) {
                $variants = if ($sample % 2 -eq 0) { @('baseline','optimized') } else { @('optimized','baseline') }
                foreach ($variant in $variants) {
                    $port = $ports[$variant]
                    Invoke-RestMethod "http://127.0.0.1:$port/_bench/config?size=$size&state=$state" | Out-Null
                    $name = "$variant-$size-$state-$sample"
                    $resultPath = Join-Path $run ($name + '.json')
                    $load = Start-BenchmarkJava $name $fixtureClasspath @('load', $port, "$Requests", "$Rate", $resultPath)
                    if (!$load.WaitForExit(60000)) { $load.Kill(); throw 'Load generator timed out' }
                    if ($load.ExitCode -ne 0) { throw "Load failed: $name; inspect $run" }
                    $data = Get-Content -Raw $resultPath | ConvertFrom-Json
                    $expected = if ($state -eq 'blocked') { 0 } else { $Requests }
                    if ($data.successes -ne $expected) { throw "Unexpected successes for ${name}: $($data.successes), expected $expected" }
                    $rows.Add([PSCustomObject]@{
                        variant=$variant; targets=$size; state=$state; sample=$sample; requests=$data.requests; successes=$data.successes; errors=$data.errors
                        request_rate=[Math]::Round($data.requests * 1e9 / $data.elapsed_ns,2)
                        successful_rate=[Math]::Round($data.successes * 1e9 / $data.elapsed_ns,2)
                        allocated_bytes_per_request=[Math]::Round($data.allocated_bytes / $data.requests,2)
                        cpu_ns_per_request=[Math]::Round($data.cpu_ns / $data.requests,2)
                        p99_ms=[Math]::Round($data.p99_ns / 1e6,3); scheduled_p99_ms=[Math]::Round($data.scheduled_p99_ns / 1e6,3)
                        gc_count=$data.gc_count; gc_ms=$data.gc_ms
                    })
                    $rows | Export-Csv -NoTypeInformation -Encoding utf8 (Join-Path $run 'results.csv')
                    Write-Output "$name completed: $($data.successes)/$Requests successful"
                }
            }
        }
    }
    if (!$SkipVerify) {
        $verify = Start-BenchmarkJava 'verify' $fixtureClasspath @('verify', $ports.optimized, $upstreamFile, (Join-Path $run 'correctness.json'))
        if (!$verify.WaitForExit(90000)) { $verify.Kill(); throw 'HTTP correctness verification timed out' }
        if ($verify.ExitCode -ne 0) { throw "HTTP correctness verification failed; inspect $run" }
    }
    [IO.File]::WriteAllText((Join-Path $run 'environment.txt'), "JDK: $(& $java -version 2>&1 | Out-String)`nRequests: $Requests; rate: $Rate; samples: $Samples; profile: $Profile`nSeparate load, gateway and upstream JVMs; 256 MiB heaps; ActiveProcessorCount=8; 128 connections; 4 gateway server and 4 gateway client event loops; 2 ms upstream delay; 1024-byte body; retry=0; gateway timeout=60 s.`n")
    Write-Output "RESULTS=$run"
} finally {
    if ($Profile) {
        foreach ($process in $processes | Select-Object -Skip 1) {
            if (!$process.HasExited) {
                $recording = Join-Path $argumentDirectory ($process.Id.ToString() + '.jfr')
                & (Join-Path $env:JAVA_HOME 'bin/jcmd.exe') $process.Id JFR.dump "filename=$recording" | Out-Null
                $label = if ($process.Id -eq $processes[1].Id) { 'baseline' } else { 'optimized' }
                if (Test-Path -LiteralPath $recording) { Copy-Item -LiteralPath $recording -Destination (Join-Path $run ($label + '.jfr')) }
            }
        }
    }
    foreach ($process in $processes) {
        if (!$process.HasExited) { $process.Kill(); $process.WaitForExit() }
    }
}
