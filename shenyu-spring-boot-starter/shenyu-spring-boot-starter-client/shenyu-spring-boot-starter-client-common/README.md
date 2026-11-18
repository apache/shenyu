# Business-client registration configuration

This starter is shared by the HTTP, WebSocket, gRPC, Dubbo, SOFA, TARS and MCP
business-client starters. Each enabled starter declares its protocol's validation.
The common repository bean validates bound configuration before calling the
repository factory, including when the factory already has a cached repository.
Validation starts no publisher, retry timer, server probe or network connection.
Shared Admin/gateway configuration DTOs and the registration wire format are unchanged.

## Binding and ownership

Top-level Java properties use Spring's existing relaxed binding: for example,
`shenyu.register.register-type` and `shenyu.register.registerType` select the same
field. Entries inside `props` are **case-sensitive Properties keys**. Preserve
`appName`, `contextPath`, `ipAndPort`, `isFull`, `addPrefixed`, `shutdownWaitTime`
and `delayOtherHooksExecTime`; do not rename these entries to kebab-case.

```yaml
spring:
  application:
    name: orders
server:
  port: 8080
shenyu:
  register:
    enabled: true
    register-type: http
    server-lists: http://localhost:9095
    props:
      username: admin
      password: ${SHENYU_ADMIN_PASSWORD}
      shutdownWaitTime: 3000
      delayOtherHooksExecTime: 2000
  namespace: one;two
  client:
    http:
      props:
        appName: orders
        contextPath: /orders
        host: 127.0.0.1
        port: 8080
```

Equivalent map entries can use bracket syntax in a properties file:

```properties
shenyu.register.register-type=http
shenyu.register.server-lists=http://localhost:9095
shenyu.register.props[shutdownWaitTime]=3000
shenyu.client.http.props[appName]=orders
shenyu.client.http.props[contextPath]=/orders
shenyu.client.http.props[port]=8080
```

`shenyu.register.enabled` defaults to true. False disables these business-client
configurations, their validation and their resources, including the opt-in
`@EnableClientRegister` configuration. There is no new per-protocol enable switch:
an absent or excluded starter contributes no validator. Unused protocol entries
are not validated. The gateway beat starter is a separate owner.

## Global and transport properties

| Property | Existing default and meaning | Validation |
| --- | --- | --- |
| `shenyu.register.register-type` | No default; repository SPI name | Nonblank. The built-in business registration transport is `http`. Custom SPI names remain supported. |
| `shenyu.register.server-lists` | No default | HTTP requires a nonempty comma-separated list of absolute HTTP/HTTPS Admin URLs, with valid URL ports. Other SPI address formats remain extension-owned. |
| `shenyu.namespace` | Missing or wholly blank uses `649330b6-c2d7-4edc-be8e-8a54df9eb385` | Nonblank lists use `;` and require nonblank entries. Namespace identifiers are not rewritten. |
| `shenyu.register.props.username/password` | Existing HTTP login inputs | Authentication remains transport/Admin-owned; validation does not echo these values or invent new mandatory credentials. |
| `shenyu.register.props.shutdownWaitTime` | `3000` milliseconds | Integer `0..2147483647`; existing JVM-hook delay. |
| `shenyu.register.props.delayOtherHooksExecTime` | `2000` milliseconds | Integer `0..2147483647`; existing JVM-hook takeover window. |

Failback registration currently uses the existing fixed ten-second interval and
finite retry count. This change adds no `retryInterval` or `maxRetries` setting to
HTTP registration. Arbitrary extension properties are retained rather than rejected
because they are absent from built-in metadata. Validation does not log supplied
values or retain URL/number parser causes in its diagnostics.

Additional existing HTTP transport settings are `props[aes-secret-key]` and
`props[aes-secret-iv]` for optional password encryption, plus
`registrationFencingDomains`, `registrationFencingStartupId` and
`registrationFencingExpectedGeneration` for the opt-in fenced registration contract.
Their crypto/capability/ownership validation remains with the existing transport;
this gate does not duplicate it. See the [fencing contract](../../../shenyu-register-center/REGISTRATION_FENCING.md).
The existing JVM-hook reflection overrides are `applicationShutdownHooksClassName`
(default `java.lang.ApplicationShutdownHooks`) and `applicationShutdownHooksFieldName`
(default `hooks`); runtime/JDK availability remains owned by that hook implementation.

## Protocol properties and defaults

The following keys are under `shenyu.client.<protocol>.props`.

| Protocol | Required identity/settings in the existing listener | Advertised port |
| --- | --- | --- |
| `http` | Legacy: requires `appName` or its `spring.application.name` fallback, even with an explicit `contextPath`. Legacy blank `contextPath` keeps its existing `/<spring.application.name>` fallback. Opt-in: absent `appName` falls back to the Spring name; an explicitly blank value stays blank. Opt-in accepts an application name or nonblank `contextPath`; absent context defaults to empty. | Optional; absent, `-1` or `0` are accepted automatic inputs. Explicit positive values must be `1..65535`. |
| `websocket` | `appName` or `contextPath`; no new Spring-name fallback | Same HTTP/WebSocket automatic-input rules. |
| `grpc` | `contextPath`, `ipAndPort` and `port`; `ipAndPort` retains its existing protocol-specific identity role | Required, `1..65535`. |
| `dubbo` | `appName` or `contextPath`; Dubbo service/protocol configuration still owns actual exports | Optional; if supplied, `1..65535`. |
| `sofa` | `appName` or `contextPath`, and `port` used by its registration payloads | Required, `1..65535`. |
| `tars` | `contextPath`, `host` and `port` | Required, `1..65535`. |
| `mcp` | `appName` or `contextPath`; tool-specific request configuration remains separate | Optional; if supplied, `1..65535`. |

Optional `host` retains the existing host/interface discovery behavior; validation
does not perform DNS/interface probes. HTTP/WebSocket `protocol` retains its existing `http://` / `ws://` default, and `isFull` defaults to false.
HTTP's `addPrefixed` defaults to false; the generic opt-in registration config also
retains its existing false default. Other opaque protocol/SPI properties keep
their own consumers and are not subjected to a closed property allowlist.

`server.port` configures the web server; `props.port` advertises a registration
endpoint. `server.port=0` is a valid random-bind request, not an advertised address.
Early validation deliberately does not resolve the server or fabricate a usable
endpoint while beans are being built. Existing listeners resolve supported
automatic inputs at their existing lifecycle boundary. Actual random-bound-port
resolution is separately tracked by [#8426 / PR #8453](https://github.com/apache/shenyu/pull/8453);
this validation change does not replace that resolver or claim its unmerged fix.

## Discovery settings

`shenyu.discovery.type` defaults to `local` for the existing local-vs-registry
publication decision. `shenyu.discovery.enable=true` creates the bound discovery
configuration. Its existing `server-list` property activates discovery-configuration
publication; `register=true` activates supported client-side instance registration.
Validate only those active paths: they require `type`, supplied `server-list` must
be nonblank, and known remote providers require their server address for instance
registration. Custom provider addresses/properties retain provider-owned formats.

For active client-side ZooKeeper registration, existing `props` numeric keys are
`sessionTimeout=3000`, `connectionTimeout=3000`, `baseSleepTime=1000`,
`maxRetries=3`, `maxSleepTime=2147483647`; all must parse as nonnegative Java
integers. Durations are milliseconds. Etcd uses `etcdTimeout=3000` milliseconds
and `etcdTTL=5` seconds: timeout is a nonnegative Java long, TTL a positive Java
long, and endpoints are HTTP/HTTPS URLs. These checks are not applied when only
publishing discovery metadata, or when discovery is disabled.

This does not change discovery authority, default resolution, mixed-starter
configuration selection or registry lifecycles. Clients still communicate with
Admin over the existing registration transport; gateways never read these client
properties from the database.
