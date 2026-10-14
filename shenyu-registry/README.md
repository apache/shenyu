<!--
Licensed to the Apache Software Foundation (ASF) under one or more
contributor license agreements.  See the NOTICE file distributed with
this work for additional information regarding copyright ownership.
The ASF licenses this file to You under the Apache License, Version 2.0
(the "License"); you may not use this file except in compliance with
the License.  You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-->

# Registry dependency selection

`shenyu-registry-core` supplies the SPI-backed factory and its existing per-type cache.
It depends only on `shenyu-registry-api` and `shenyu-spi`; implementation modules depend
on the API. Loading a factory no longer loads every registry client.

## Select providers explicitly

Add the implementation matching `RegisterConfig.registerType` to your application's
runtime classpath, using the same ShenYu version as the application:

```xml
<dependency>
    <groupId>org.apache.shenyu</groupId>
    <artifactId>shenyu-registry-zookeeper</artifactId>
    <version>${shenyu.version}</version>
    <scope>runtime</scope>
</dependency>
```

| SPI key | Implementation artifact |
| --- | --- |
| `zookeeper` | `shenyu-registry-zookeeper` |
| `etcd` | External `shenyu-registry-etcd` from plugin-store; see [migration](../docs/plugin-store-etcd.md) |
| `nacos` | `shenyu-registry-nacos` |
| `apollo` | `shenyu-registry-apollo` |
| `eureka` | `shenyu-registry-eureka` |
| `kubernetes` | `shenyu-registry-kubernetes` |

Multiple implementation dependencies may coexist. The configured key selects the
provider; the factory never calls `init` on unrelated providers. Missing or unknown keys
fail with the requested artifact, runtime classpath guidance, and available SPI keys.
Provider construction and initialization errors still propagate without being reported
as missing dependencies.

SDK libraries depend on the registry API. The SDK and Feign SDK starters additionally
include the factory, so add the selected implementation next to the SDK starter.
Applications calling the factory directly should declare `shenyu-registry-core` explicitly.
`shenyu.sdk.register-type=local` uses configured gateway addresses and needs no registry
implementation. The HTTP, Dubbo and Feign SDK examples select ZooKeeper, the Spring Cloud
example selects Eureka, and the gRPC example selects no registry implementation.

## Compatibility and migration

The compatibility starter retains ZooKeeper, Nacos, Eureka and Kubernetes.
etcd is now an explicit external dependency: add the compatible plugin-store
`shenyu-registry-etcd` artifact before upgrading a deployment configured with `etcd`.
Its SPI key and configuration remain unchanged; see [migration and rollback](../docs/plugin-store-etcd.md).
Apollo remains an explicit choice.

Applications depending directly on core, SDK libraries, or SDK starters should add
their configured provider before upgrading. For gateway registration with a smaller
classpath, retain the registry starter, exclude the unused implementation dependencies,
and declare the selected implementation explicitly. The SDK integration tests show
this arrangement for ZooKeeper. Admin declares its four in-tree discovery providers
explicitly and preserves independent connections for separate discovery configurations.

To roll back a dependency selection, restore the application's previous explicit
provider dependencies, or keep the compatibility starter if already used. Rolling back
the ShenYu version restores the old core aggregation. No stored configuration or SPI
key migration is required. The factory's cache and reinitialization behavior is unchanged.

## External implementations

External providers, including etcd, Consul and Polaris extensions, implement
`org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository`, carry `@Join`, and
publish this resource in their JAR:

```text
META-INF/shenyu/org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository
```

For example, `consul=com.example.ConsulInstanceRegisterRepository` preserves the existing
registration key. Add the extension JAR and its client dependencies to the runtime
classpath; no core POM change is needed. Existing ShenYu SPI classloader rules continue
to apply. Factory tests load external `consul`, `etcd` and `polaris` keys from isolated resources
without any built-in provider dependency.

## Dependency and release inventory

| Consumer | Registry implementation inventory |
| --- | --- |
| Registry API/core, SDK libraries and SDK starters | None |
| Compatibility registry starter | ZooKeeper, Nacos, Eureka, Kubernetes |
| Admin distribution | ZooKeeper, Nacos, Eureka, Kubernetes, explicitly declared |
| Bootstrap distribution | Registry API only; client libraries also arrive through sync/register/plugin modules |
| SDK HTTP/Dubbo integration gateways | ZooKeeper |
| Upload-plugin integration gateway | None; registry registration is not enabled |

etcd clients are no longer part of the default Admin/Bootstrap dependency graphs;
their jetcd LICENSE/NOTICE entries move with the external stack. The remaining
client notices stay in the distributions. Custom packages enabling etcd must
include the notices for the external artifacts and their runtime dependencies.

CI checks resolved runtime trees for the API/core, both SDK libraries/starters, all five
in-repository implementations and the compatibility starter. The checks reject registry
implementations and client libraries in lean consumers, reverse implementation-to-core
dependencies, and loss of a compatibility provider. Run locally:

```bash
python3 .github/scripts/test-registry-dependencies.py
REGISTRY_MODULES="$(python3 .github/scripts/check-registry-dependencies.py --modules)"
./mvnw -B -ntp -pl "$REGISTRY_MODULES" -am package dependency:tree \
  -DskipTests -DskipRemoteResources=true -Djacoco.skip=true -Dmaven.javadoc.skip=true \
  -Dscope=runtime -DoutputType=tgf -DoutputFile=target/registry-dependencies.tgf
python3 .github/scripts/check-registry-dependencies.py
```
