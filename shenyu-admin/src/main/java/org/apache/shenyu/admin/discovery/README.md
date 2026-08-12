# Discovery namespace handling

This note documents how discovery data is scoped when the same upstream service is used by multiple ShenYu namespaces.

## Namespace scoped data

Discovery configuration is stored per namespace. `DiscoveryServiceImpl` binds a discovery configuration by looking up the selector with `selectorName`, `pluginName`, and `namespaceId`, and then creates or reuses a discovery record by `pluginName`, discovery level, `namespaceId`, and discovery type.

The runtime sync payload also carries namespace information:

- `DiscoverySyncData.namespaceId`
- `DiscoveryUpstreamData.namespaceId`
- `DiscoveryUpstreamDTO.namespaceId`
- `DiscoveryUpstreamDO.namespaceId`
- `CommonUpstream.namespaceId`, inherited by plugin upstream handles such as `DivideUpstream`

During a full discovery refresh, `AbstractDiscoveryProcessor.fetchAll(...)` fills missing upstream namespaces from the current proxy selector namespace:

```java
if (Objects.isNull(discoveryUpstreamData.getNamespaceId())) {
    discoveryUpstreamData.setNamespaceId(proxySelectorDTO.getNamespaceId());
}
```

This makes the initial upstream list namespace aware even when the registry payload itself does not contain a ShenYu namespace.

## Shared registry service names

External registries such as Eureka do not know about ShenYu namespaces. When two ShenYu namespaces configure the same Eureka `listenerNode`, both namespaces watch the same Eureka application name.

Current watcher caching is scoped by discovery id. Since discovery records are namespace scoped, the same Eureka service name in multiple namespaces creates one watcher per namespace discovery id. Inside one discovery id, duplicate watchers for the same listener node are avoided by `DefaultDiscoveryProcessor`.

This is expected for correctness, but it can duplicate registry polling work. Cross-namespace watcher sharing would require a broader cache key such as registry type, server list, and listener node, plus fan-out to each namespace-specific `DiscoverySyncData`.

## Incremental update caveat

Eureka incremental events currently build upstream JSON without `namespaceId`. `DiscoveryDataChangedEventSyncListener` maps a blank upstream namespace to the system default namespace before filtering by the current selector namespace. As a result, an incremental Eureka upstream event for a non-default namespace can be filtered out even though the initial full refresh inserted the same upstream with the correct namespace.

The minimum behavior-preserving fix is to fill a blank upstream namespace from the current discovery sync context instead of the system default namespace:

```java
if (StringUtils.isBlank(discoveryUpstreamData.getNamespaceId())) {
    discoveryUpstreamData.setNamespaceId(discoverySyncData.getNamespaceId());
}
```

Payloads that explicitly include a namespace keep their current behavior and are still filtered by the selector namespace.
