# Actuator raw configuration inspection (compatibility and migration)

## Background

The gateway serves a set of inspection endpoints under `/actuator` (`/actuator/pluginData`, `/actuator/selectorData`, `/actuator/ruleData`, `/actuator/selectorMatchCache`, `/actuator/ruleMatchCache`, `/actuator/metadata`, `/actuator/metadataCache`). They are plain MVC mappings under a hard-coded `/actuator` prefix that the `HealthFilter` dispatches straight to the `DispatcherHandler`, bypassing the gateway plugin chain. They are **not** Spring Boot Actuator endpoints: `management.endpoints.web.exposure.include`, `management.server.base-path` and the management port have never had any effect on them.

The payloads of these endpoints are the raw runtime configuration caches, and they routinely carry credentials: plugin `config`, selector and rule `handle` (database passwords, API keys, tokens), and metadata `rpcExt`. Because the listener is reachable by anonymous callers in a default deployment, the raw payloads are no longer served by default.

## Behavior after this change

Two views now exist:

1. **Digest view (always available)** — `ShenyuControllerEndpoint` serves `/actuator/plugins`, `/actuator/pluginDigest`, `/actuator/selectorDigest`, `/actuator/ruleDigest`, `/actuator/selectorMatchDigest`, `/actuator/ruleMatchDigest`, `/actuator/metadataDigest` and `/actuator/metadataCacheDigest`. These publish non-secret identity fields (plugin name, selector and rule names and ids, metadata path, RPC type and method) plus SHA-256 digests of the sensitive payload fields only (`config`, `handle`, `rpcExt`), reusing the diagnostics subsystem's `DiagnosticRedactor` digest policy. `/actuator/plugins` (plugin name and order, no configuration) stays visible by design, exactly as before.
2. **Raw view (opt-in)** — `ShenyuRawDataEndpoint` serves the original paths with the original raw payloads, byte-for-byte as before this change. The bean is only registered when `shenyu.endpoint.raw-data.enabled=true`.

With the property absent or `false` (the default), the raw paths have no mappings and a request to them is rejected (HTTP 404) before any cache traversal.

## Migration for operators

If your monitoring or tooling reads the raw paths, choose one of:

- **Switch to the digest view** if you only need structure and cardinality (cache sizes, selectors/rules per plugin, metadata inventory). No configuration is needed; the endpoints are always on. Note the digest view intentionally omits secret-bearing content, so consumers must not expect raw values.
- **Opt in to the raw view** if you genuinely need the raw payloads, understanding that whatever credentials live in your plugin/selector/rule/metadata configuration become readable by anyone who can reach the gateway listener:

```yaml
shenyu:
  endpoint:
    raw-data:
      enabled: true
```

See the commented example in `shenyu-bootstrap/src/main/resources/application.yml`. E2E and integration tooling that asserts on the raw payloads (for example `shenyu-e2e-client`'s `GatewayClient`) sets this property in its test environment.
