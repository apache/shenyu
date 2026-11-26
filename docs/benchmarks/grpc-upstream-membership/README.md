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

## Reproduce (from repository root)

Requires JDK 17. The isolated benchmark POM resolves JMH through the
repository Maven wrapper and compiles the two production DTO source files directly.
It is not a module of the gateway reactor and adds no gateway runtime dependencies.

POSIX shell (Linux/macOS):

```sh
./mvnw -B -f docs/benchmarks/grpc-upstream-membership/pom.xml clean package &&
java -jar docs/benchmarks/grpc-upstream-membership/target/benchmarks.jar \
  UpstreamMembershipBenchmark -f 2 -wi 3 -i 5 -w 1s -r 1s \
  -jvmArgs '-Xms256m -Xmx256m' -prof gc -rf json \
  -rff docs/benchmarks/grpc-upstream-membership/target/results.json
```

PowerShell:

```powershell
.\mvnw.cmd -B -f docs/benchmarks/grpc-upstream-membership/pom.xml clean package
if ($LASTEXITCODE -ne 0) { throw 'Benchmark build failed' }
java -jar docs/benchmarks/grpc-upstream-membership/target/benchmarks.jar UpstreamMembershipBenchmark -f 2 -wi 3 -i 5 -w 1s -r 1s -jvmArgs '-Xms256m -Xmx256m' -prof gc -rf json -rff docs/benchmarks/grpc-upstream-membership/target/results.json
```

The default sizes are 8, 1024, and 4096, matching the recorded table and JSON.
For a packaging smoke test, override the measurement options with
`-f 1 -wi 0 -i 1 -r 100ms`; smoke results are not performance evidence.
The commands can be used on Linux CI, but this PR does not add a timing gate or
claim that the benchmark has run in the project's Linux CI.

## Keeping the harness aligned

The predicates are copied, not invoked through `ShenyuNameResolver`, so they can
silently drift when production changes. Before interpreting a new run, compare
`baseline()` against the pre-optimization revision below and `optimized()` against
`ShenyuNameResolver.Resolve.needsToUpdateConnections` in the checkout. Review the
size guard, equal-list fast path, membership direction, and DTO `equals/hashCode`.
Both harness variants use a primitive size comparison instead of production's
boxed `Objects.equals` size guard; the copied predicates are not byte-for-byte
resolver measurements.
The setup assertion checks the unchanged-membership fixtures only; it cannot
establish equivalence with the resolver. Keep resolver behavior covered by
`ShenyuNameResolverTest`; the large-list test checks publication behavior, not
execution time or algorithmic complexity.

`GrpcUpstream.equals` ignores `weight` and `status`. Updates limited to these
fields do not cause this predicate to publish when list size and equality fields
are unchanged. This preserves the previous comparison semantics, not a complete
snapshot comparison.

## Recorded run

November 25, 2026; Windows, Intel Core i5-12500H (12 cores / 16 logical CPUs),
Oracle HotSpot 17.0.4.1+1-LTS-2, JMH 1.37. Two forks, three 1-second warmups and
five 1-second measurements per fork, 256 MiB heap. Base revision:
`d3a3918a69b7297b3f9837c71c8f669593940daa`.

The recorded VM is `Java HotSpot(TM) 64-Bit Server VM`, version
`17.0.4.1+1-LTS-2`, using the HotSpot JIT. The JVM arguments captured in
`results.json` are `-Xms256m -Xmx256m`; no explicit JIT compiler or tiering
overrides are recorded. These measurements apply to this machine, JVM, and run
conditions only, not to other hardware, JIT implementations, or gateway workloads.

| Order | Instances | Baseline, us/op | Optimized, us/op | Optimized allocated bytes/op |
| --- | ---: | ---: | ---: | ---: |
| Same | 8 | 0.679 | 0.138 | approximately 0 |
| Same | 1024 | 6956.012 | 18.661 | approximately 0 |
| Same | 4096 | 83296.715 | 62.597 | approximately 0 |
| Reversed | 8 | 0.609 | 0.523 | 912 |
| Reversed | 1024 | 6080.040 | 80.047 | 106576 |
| Reversed | 4096 | 79393.927 | 626.701 | 426064 |

Raw measurements, JMH error estimates and allocation metrics are in
`results.json`. These historical results retain the original `benchmark` package
name; the harness now uses `org.apache.shenyu.benchmark`. The recorded data has
not been relabeled or replaced by packaging smoke-test results.

This was a shared development machine with dependency downloads running
concurrently. Variance is substantial, particularly for the 4096-entry
reversed optimized case (626.701 +/- 1039.937 us/op); do not treat these numbers
as stable production speedup ratios. They demonstrate the scaling issue and the
allocation tradeoff: the equal-list fast path avoids allocation, while reordered
lists use a temporary set with O(n) space and expected O(n) comparison time.

The implementation retains the size check and existing membership semantics,
including duplicate handling, and publishes addresses from the original list.
The set is not retained across notifications because upstream objects are mutable.
