# gRPC upstream membership benchmark

The [benchmark instructions and recorded results](benchmarks/grpc-upstream-membership/README.md)
compare the resolver's previous list-membership scan with the equal-list fast path
and temporary-set comparison. The guide includes Maven-wrapper build commands
for POSIX shells and PowerShell, allocation measurements, and the limitations of
the copied predicates. These microbenchmarks measure comparison work, not gateway
throughput. Resolver publication behavior is checked separately by
`ShenyuNameResolverTest`.
