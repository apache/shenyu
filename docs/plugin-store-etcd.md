<!--
Licensed to the Apache Software Foundation (ASF) under one or more
contributor license agreements. See the NOTICE file distributed with
this work for additional information regarding copyright ownership.
The ASF licenses this file to You under the Apache License, Version 2.0
(the "License"); you may not use this file except in compliance with
the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-->
# etcd plugin-store migration

The entire etcd stack is transferred as one unit to `apache/shenyu-plugin-store`:
`shenyu-infra-etcd`, `shenyu-admin-listener-etcd`, `shenyu-sync-data-etcd`,
`shenyu-registry-etcd`, and `shenyu-spring-boot-starter-sync-data-etcd`.
Infrastructure is etcd-specific; no retained core module imports it.

## Release gates

The main removal PR can enter code review, but must not merge until dev-list consensus is recorded, an
advance deprecation notice has shipped in a core release, the full released
deprecation cycle is complete, receiving builds and real-server/full-deployment
tests pass, external artifacts are published, and a rollback window is announced.
The deprecation release retains the in-tree integration; this branch prepares a
later cutover. No removal release is scheduled. Source changes and local snapshots
do not complete these release gates.
The store currently uses `2.7.1-SNAPSHOT` and compiles against ShenYu `2.7.2-SNAPSHOT`;
select the announced compatible store release when deploying. Do not publish the
same group/artifact/version from both repositories. Retain the last in-tree release
and its configuration during the rollback window; the release announcement must
specify that window before the cutover is merged.

## Dependency selection

Admin needs `org.apache.shenyu:shenyu-admin-listener-etcd`; gateway needs
`org.apache.shenyu:shenyu-spring-boot-starter-sync-data-etcd`. Applications using etcd
instance registration, SDK discovery, or Admin etcd discovery additionally need
`org.apache.shenyu:shenyu-registry-etcd`. Use the **store** version for all five
external modules and the **core** version for retained ShenYu APIs.
Gateway registration also requires the retained core
`shenyu-spring-boot-starter-registry`; exclude its bundled providers and add the
external `shenyu-registry-etcd`. The receiving gateway example POM shows this selection.
The infrastructure and jetcd dependencies are transitive. For binary distributions,
resolve the complete runtime dependency closure into `lib/`, remove superseded etcd
JARs, and include the extension's LICENSE/NOTICE information in the custom package.
Adding only the starter JAR without its dependencies is insufficient.

The `etcd` SPI key is unchanged. The JAR still registers
`etcd=org.apache.shenyu.registry.etcd.EtcdInstanceRegisterRepository` in
`META-INF/shenyu/org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository`.
Admin's existing `ShenyuInstanceRegisterRepositoryFactory.loadRepository` loads the
external provider without a hard-coded class or dependency in core.

## Configuration and storage contract

Admin and gateway retain `shenyu.sync.etcd.url`, with comma-separated endpoint URLs:

```yaml
shenyu:
  sync:
    etcd:
      url: http://etcd-1:2379,http://etcd-2:2379
      sessionTimeout: 30000
      connectionTimeout: 30000
```

Disable the previous sync channel when selecting etcd. Admin uses
`shenyu.sync.websocket.enabled=false`; gateway must remove/disable its WebSocket
`urls` setting. Preserve the existing timeout values: the historical binding maps
`sessionTimeout` to the client's operation timeout (milliseconds) and
`connectionTimeout` to its lease TTL (seconds). This migration does not reinterpret
those values; revisit the naming/units in a separate compatible change.

Gateway instance registration keeps:

```yaml
shenyu:
  register:
    enabled: true
    registerType: etcd
    serverLists: http://etcd-1:2379,http://etcd-2:2379
    props:
      etcdTimeout: "3000"
      etcdTTL: "5"
```

Configuration keys retain their leading slash and namespace:

| Data | Key |
| --- | --- |
| Plugin | `/<namespace>/shenyu/plugin/<pluginName>` |
| Selector | `/<namespace>/shenyu/selector/<pluginName>/<selectorId>` |
| Rule | `/<namespace>/shenyu/rule/<pluginName>/<selectorId>-<ruleId>` |
| Authentication | `/<namespace>/shenyu/auth/<appKey>` |
| Metadata | `/<namespace>/shenyu/metaData/<path>` |
| Proxy selector | `/<namespace>/shenyu/proxySelectorData/<pluginName>/<selectorId>` |
| Discovery upstream | `/<namespace>/shenyu/discoveryUpstream/<pluginName>/<selectorId>` |
| Instance | `/shenyu/register/instance/<appName>/<host>:<port>` |

JSON DTOs, enum values, discovery mode, namespace IDs and existing keyspaces stay
compatible. Instance keys remain lease-backed; after client shutdown they disappear
when their lease expires. No schema conversion or data deletion is required.

## Compatibility, licensing and verification

The transferred client remains `io.etcd:jetcd-core:0.7.3`, using the core-managed
gRPC `1.65.1`. The real-server fixture targets etcd `3.5.21`; other etcd/client
versions need validation before claiming support. Server and client are Apache-2.0:
see [etcd LICENSE](https://github.com/etcd-io/etcd/blob/v3.5.21/LICENSE) and
[jetcd LICENSE](https://github.com/etcd-io/jetcd/blob/jetcd-0.7.3/LICENSE).
The receiving extension carries the former distribution's CoreOS notice.
Historical release notes and upgrade documentation remain in main.

The store README documents unit, starter, real-server and full Admin/gateway E2E
commands. Real-server tests cover initial snapshots, namespace isolation, route
create/update/delete, server restart/watch recovery, external SPI registration,
instance update/delete/lease expiry and client shutdown. CI uses Testcontainers;
a native etcd binary is an explicit local alternative. Full deployment validation
uses the real Admin API and gateway HTTP traffic with externally supplied JARs.

## Upgrade and rollback

1. Back up Admin's database and take an etcd snapshot. Record namespace, endpoints,
   timeouts, TTL, core/store versions and the deployed JAR inventory.
2. Verify compatible external artifacts and all release gates in a staging deployment.
   Add the selected artifacts and their runtime dependencies before changing images.
3. Roll Admin and gateways one at a time, preserving their namespace/endpoints and
   registry settings. Check initial synchronization, route updates and instance discovery.
4. Keep the previous in-tree images and configuration available for the announced window.
   To roll back, restore those images/configuration and remove the external etcd JARs.
   Reuse the same etcd keyspace and Admin database; do not clear either as part of rollback.

The archived compose/Kubernetes assets in `examples/etcd/legacy` belong to the last
in-tree deployment. They require the original core images; the receiving project's
new external-artifact E2E launcher is the cutover validation path.
