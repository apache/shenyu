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
# Consul Plugin Store Cutover

The Consul admin listener, gateway sync service, registry repository, and Spring Boot starter are no longer built or shipped by the main ShenYu repository. Existing deployments that use Consul should add the external plugin-store artifacts:

```xml
<dependency>
    <groupId>org.apache.shenyu</groupId>
    <artifactId>shenyu-admin-listener-consul</artifactId>
    <version>2.7.1-SNAPSHOT</version>
</dependency>
<dependency>
    <groupId>org.apache.shenyu</groupId>
    <artifactId>shenyu-spring-boot-starter-sync-data-consul</artifactId>
    <version>2.7.1-SNAPSHOT</version>
</dependency>
<dependency>
    <groupId>org.apache.shenyu</groupId>
    <artifactId>shenyu-registry-consul</artifactId>
    <version>2.7.1-SNAPSHOT</version>
</dependency>
```

The external extension keeps the existing `shenyu.sync.consul` configuration prefix, registry type name `consul`, Consul KV namespace layout, service names, TTL check behavior, and `com.ecwid.consul:consul-api` client compatibility used by the former in-tree implementation.

Consul sync configuration remains:

```yaml
shenyu:
  sync:
    consul:
      url: http://localhost:8500
      waitTime: 10000
      watchDelay: 10000
```

Registry configuration remains:

```yaml
shenyu:
  register:
    registerType: consul
    serverLists: 127.0.0.1:8500
    props:
      token: ""
      waitTime: "30"
      watchDelay: "5"
      tags: gateway,shenyu
      checkTtl: "5"
```

Configuration data stays below the namespace-scoped Consul KV prefix, for example `default/shenyu/plugin/<pluginName>`, `default/shenyu/selector/<pluginName>/<selectorId>`, and `default/shenyu/rule/<pluginName>/<selectorId>-<ruleId>`. Deployments that rely on ACL tokens, TLS, mTLS, custom CA bundles, or multi-datacenter routing should keep using the same local Consul agent or proxy pattern they used before cutover unless the external extension adds new public configuration fields.

The main repository keeps Consul enum values, SQL comments, and historical upgrade scripts as compatibility contracts for existing admin data. The main repository no longer publishes the Consul runtime artifacts or includes the Consul client dependency in admin/bootstrap distributions.

## Verification Status

The external Consul extension has a real Consul boundary test that writes through the real admin listener and initializer into Consul KV, reads through the real `ConsulSyncDataService`, dispatches to the real gateway subscribers, and verifies route create/update/delete behavior through `ShenyuWebHandler` and the gateway HTTP plugin chain. That fixture also covers namespace isolation and Consul registry register/deregister behavior.

This boundary test is not full `shenyu-admin` application or controller coverage; deployments that need controller-level assurance should still run their normal admin-to-gateway environment tests after switching to the external artifacts.

Rollback during the cutover window is a dependency-only change: remove the external Consul artifacts and return to a ShenYu build that still shipped the in-tree Consul modules. Existing Consul KV data and registered service names keep the same layout and should not be deleted during rollback unless the deployment intentionally resets all ShenYu sync state.
