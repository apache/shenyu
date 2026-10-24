# ShenYu example Spring Cloud

Start Eureka with `shenyu-example-eureka`, then run `ShenyuTestSpringCloudApplication`
using the configured HTTP client registration channel.

etcd data synchronization and gateway/SDK instance discovery are external integrations;
see the [plugin-store migration guide](../../docs/plugin-store-etcd.md) for artifacts,
configuration and rollback. Client HTTP API registration remains a separate channel.
The former etcd client/server registration example is archived in plugin-store's
`examples/etcd/legacy` for the pre-cutover release.
