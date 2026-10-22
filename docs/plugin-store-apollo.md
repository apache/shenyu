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
# Apollo plugin-store migration

## Status and deprecation cycle

Apollo is currently an optional, supported in-tree integration. Issue [#7599](https://github.com/apache/shenyu/issues/7599)
proposes transferring its registry, data-sync, Admin listener and gateway starter to
[plugin-store](https://github.com/apache/shenyu-plugin-store), extending [#7306](https://github.com/apache/shenyu/issues/7306).
This guide is the advance deprecation notice: the in-tree modules remain available
for the release carrying this notice. No removal release has been selected.
The separate removal PR is a draft and must not be merged in that deprecation release.

| Stage | Required action |
| --- | --- |
| Agreement | Obtain and link the dev-list decision approving Apollo externalization. |
| Deprecation release | Ship this notice and migration guide in a core release while retaining all four in-tree modules. Record that released version publicly. |
| Receiving parity | Build/test all four modules together in plugin-store, verify external SPI loading and unchanged sync/registry configuration. |
| Publication | Announce compatible core/store versions, publish external artifacts before cutover, and reserve one publisher per Maven group/artifact/version. |
| Later core release | After the deprecation release has shipped, merge the separate removal, update the release inventory, and announce the rollback window. |

Do not treat creating PRs, merging documentation, or producing local snapshot JARs
as completing the dev-list decision, a released deprecation cycle, or artifact publication.
Keep the issue open until all stages and their release evidence are complete.

## Artifact ownership and installation

The receiving `shenyu-extension-apollo` reactor owns these artifacts together:

- `shenyu-admin-listener-apollo`
- `shenyu-sync-data-apollo`
- `shenyu-registry-apollo`
- `shenyu-spring-boot-starter-sync-data-apollo`

The preparation branch uses store `2.7.1-SNAPSHOT` against core `2.7.2-SNAPSHOT`.
These are development coordinates, not an announced release compatibility matrix.
For deployment, select the published store release declared compatible with your core release.
Use the core version for retained APIs and the store version for **all four** Apollo artifacts.
Keep exactly one provider version on each runtime classpath; the in-tree and external
implementations retain identical packages and must not coexist.

Admin needs:

```xml
<dependency>
    <groupId>org.apache.shenyu</groupId>
    <artifactId>shenyu-admin-listener-apollo</artifactId>
    <version>${store.version}</version>
</dependency>
```

Gateway data synchronization needs:

```xml
<dependency>
    <groupId>org.apache.shenyu</groupId>
    <artifactId>shenyu-spring-boot-starter-sync-data-apollo</artifactId>
    <version>${store.version}</version>
</dependency>
```

Applications using Apollo instance registration or SDK instance discovery additionally need:

```xml
<dependency>
    <groupId>org.apache.shenyu</groupId>
    <artifactId>shenyu-registry-apollo</artifactId>
    <version>${store.version}</version>
</dependency>
```

Gateway registration also requires the retained core `shenyu-spring-boot-starter-registry`;
SDK discovery requires the appropriate SDK starter. Apollo was never included in the
compatibility registry starter, so it remains an explicit provider dependency.
For binary distributions, resolve the complete runtime dependency closure into `lib/`,
replace old Apollo JARs, and retain the extension's licensing information and dependency notices.

Shading continues to package Apollo client dependencies, but excludes retained ShenYu
API artifacts and sibling extension modules. API classes come from the application's core artifacts, preserving
class identity for external SPI loading. The published POM retains runtime dependencies.

## Enable Admin and gateway synchronization

Admin keeps its existing property prefix and portal publishing settings:

```yaml
shenyu:
  sync:
    websocket:
      enabled: false
    apollo:
      appId: shenyu
      meta: http://apollo-config-service:8080
      portalUrl: http://apollo-portal:8070
      env: DEV
      clusterName: default
      namespace: application
      token: ${APOLLO_OPENAPI_TOKEN}
      operatorUser: apollo
```

Gateway keeps the same reading settings; remove or disable the previous sync channel:

```yaml
shenyu:
  namespace: 649330b6-c2d7-4edc-be8e-8a54df9eb385
  sync:
    websocket:
      urls: false
    apollo:
      appId: shenyu
      meta: http://apollo-config-service:8080
      env: DEV
      clusterName: default
      namespace: application
      accessKey: ${APOLLO_ACCESS_KEY:}
```

The portal OpenAPI token and config-service access key serve different APIs.
Ensure the publishing app, environment, cluster and Apollo namespace match the reader.
The preserved reader calls Apollo `ConfigService.getAppConfig()`, so the example uses
`application`. This migration does not add arbitrary namespace selection.
The ShenYu namespace ID is a separate tenant prefix inside Apollo item keys.
The Apollo client sets JVM-wide app/meta/cluster properties; preserve the existing
process-level configuration when using sync and registry together.

## Registry and SPI contract

The external JAR continues to publish:

```text
META-INF/shenyu/org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository
apollo=org.apache.shenyu.registry.apollo.ApolloInstanceRegisterRepository
```

`ShenyuInstanceRegisterRepositoryFactory.loadRepository("apollo")` resolves that
resource without any factory or core runtime change, and preserves the non-singleton provider.
Registry configuration remains:

```yaml
shenyu:
  register:
    enabled: true
    registerType: apollo
    serverLists: http://apollo-config-service:8080
    props:
      appId: shenyu
      portalUrl: http://apollo-portal:8070
      token: ${APOLLO_OPENAPI_TOKEN}
      env: DEV
      clusterName: default
      namespace: application
```

SDK discovery uses the same `apollo` type and registry settings via its existing SDK
configuration; it does not replace backend HTTP annotation/API registration.
Instance registration writes persistent portal items and publishes the namespace.
Existing shutdown removes listeners; it does not deregister persistent instance items.
Preserve that behavior during migration and apply your existing instance-removal procedure.

## Stored keys and shared compatibility shim

| Data | Existing Apollo item key |
| --- | --- |
| Plugin | `<shenyuNamespace>.plugin.<pluginName>` |
| Selector | `<shenyuNamespace>.selector.<pluginName>.<selectorId>` |
| Rule | `<shenyuNamespace>.rule.<pluginName>.<selectorId>.<ruleId>` |
| Authentication | `<shenyuNamespace>.auth.<appKey>` |
| Metadata | `<shenyuNamespace>.meta.<metadataId>` |
| Proxy selector | `<shenyuNamespace>.proxy.selector.<pluginName>.<selectorId>` |
| Discovery upstream | `<shenyuNamespace>.discovery.<pluginName>.<selectorId>` |
| Instance | `/shenyu/register/instance/<appName>/<host>:<port>` |

The existing `.list` index items under each category/group are retained alongside the data keys.
JSON DTOs, key prefixes, namespace IDs and the `apollo` type remain unchanged.
`org.apache.shenyu.common.constant.ApolloPathConstants` stays in core as a small
constants/path compatibility shim with no Apollo SDK dependency. Do not duplicate that
class in store JARs or migrate/delete existing keyspaces as part of this transfer.

The client versions remain `apollo-client`, `apollo-openapi` and `apollo-core` `2.1.0`.
Source and the Apollo clients are Apache-2.0; preserve licenses when assembling custom packages.
Existing unit tests, real Apollo `DefaultConfig` listener filtering tests and starter
configuration tests move with their modules. Those tests do not claim a full live Apollo
portal/config-service deployment; validate your staging deployment before production cutover.

## Upgrade and rollback

1. After publication and release gates pass, back up the Admin database and Apollo
   application/namespace configuration, recording exact core/store versions and deployed JARs.
2. Verify the external artifacts in staging using unchanged namespace, endpoints, tokens,
   keys and registry properties. Check initial sync, create/update/delete and instance updates.
3. Roll Admin and gateways with complete external dependency closures, then confirm
   subsequent changes propagate and existing instance discovery still works.
4. Retain the last in-tree release for the announced rollback window. To roll back,
   restore its images/JARs and configuration and remove the external Apollo versions.
   Reuse the same Admin database and Apollo keyspace; do not clear either during rollback.
