# gRPC upstream membership microbenchmark

This standalone JMH harness compares the old and proposed bodies of
`ShenyuNameResolver.Resolve.needsToUpdateConnections`, using the production
`GrpcUpstream` and `CommonUpstream` classes. The method bodies are reproduced in
the harness; it does not invoke the private resolver method. Resolver behavior
is tested separately in `ShenyuNameResolverTest` through real cache notifications.
These are comparison costs, not gateway latency or throughput measurements.

Each input contains distinct but equal upstream objects, modeling a freshly
decoded notification. `same` preserves order; `reversed` changes order without
changing membership. Set construction and its allocations are timed. This
benchmark covers unchanged membership; size changes already return immediately.

## Reproduce (PowerShell, from repository root)

Requires a JDK 17 `java` and `javac` on PATH. No Maven project configuration or
runtime dependencies are changed. Dependencies below are fetched from Maven Central.

```powershell
$bench = 'docs/benchmarks/grpc-upstream-membership'
$out = Join-Path $env:TEMP 'shenyu-grpc-membership-benchmark'
New-Item -ItemType Directory -Force "$out/lib", "$out/classes" | Out-Null
$artifacts = @{
  'jmh-core.jar' = 'org/openjdk/jmh/jmh-core/1.37/jmh-core-1.37.jar'
  'jmh-annprocess.jar' = 'org/openjdk/jmh/jmh-generator-annprocess/1.37/jmh-generator-annprocess-1.37.jar'
  'jopt-simple.jar' = 'net/sf/jopt-simple/jopt-simple/5.0.4/jopt-simple-5.0.4.jar'
  'commons-math3.jar' = 'org/apache/commons/commons-math3/3.6.1/commons-math3-3.6.1.jar'
}
foreach ($entry in $artifacts.GetEnumerator()) {
  Invoke-WebRequest "https://repo.maven.apache.org/maven2/$($entry.Value)" -OutFile "$out/lib/$($entry.Key)"
}
$cp = "$out/classes;$out/lib/*"
$dto = 'shenyu-common/src/main/java/org/apache/shenyu/common/dto/convert/selector'
javac -cp $cp -d "$out/classes" "$dto/CommonUpstream.java" "$dto/GrpcUpstream.java" "$bench/UpstreamMembershipBenchmark.java"
if ($LASTEXITCODE -ne 0) { throw 'Benchmark compilation failed' }
java -cp $cp org.openjdk.jmh.Main UpstreamMembershipBenchmark -f 2 -wi 3 -i 5 -w 1s -r 1s -p size=8,1024,4096 -jvmArgs '-Xms256m -Xmx256m' -prof gc -rf json -rff "$out/results.json"
```

## Recorded run

November 25, 2026; Windows, Intel Core i5-12500H (12 cores / 16 logical CPUs),
Oracle HotSpot 17.0.4.1+1-LTS-2, JMH 1.37. Two forks, three 1-second warmups and
five 1-second measurements per fork, 256 MiB heap. Base revision:
`d3a3918a69b7297b3f9837c71c8f669593940daa`.

| Order | Instances | Baseline, us/op | Optimized, us/op | Optimized allocated bytes/op |
| --- | ---: | ---: | ---: | ---: |
| Same | 8 | 0.679 | 0.138 | approximately 0 |
| Same | 1024 | 6956.012 | 18.661 | approximately 0 |
| Same | 4096 | 83296.715 | 62.597 | approximately 0 |
| Reversed | 8 | 0.609 | 0.523 | 912 |
| Reversed | 1024 | 6080.040 | 80.047 | 106576 |
| Reversed | 4096 | 79393.927 | 626.701 | 426064 |

Raw measurements, JMH error estimates and allocation metrics are in
`results.json`. This was a shared development machine with dependency downloads
running concurrently. Variance is substantial, particularly for the 4096-entry
reversed optimized case (626.701 +/- 1039.937 us/op); do not treat these numbers
as stable production speedup ratios. They demonstrate the scaling issue and the
allocation tradeoff: the equal-list fast path avoids allocation, while reordered
lists use a temporary set with O(n) space and expected O(n) comparison time.

The implementation retains the size check and existing membership semantics,
including duplicate handling, and publishes addresses from the original list.
The set is not retained across notifications because upstream objects are mutable.
