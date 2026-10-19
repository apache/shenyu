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
| `etcd` | `shenyu-registry-etcd` |
| `nacos` | `shenyu-registry-nacos` |
| `apollo` | `shenyu-registry-apollo`; in-tree deprecation and planned external ownership: [migration guide](../docs/plugin-store-apollo.md) |
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

For the 2.7.2 transition, `shenyu-spring-boot-starter-registry` retains the five providers
previously supplied by core: ZooKeeper, etcd, Nacos, Eureka and Kubernetes. Existing users
of that starter keep the same classpath and configuration keys. Apollo was not part of
the previous aggregate and remains an explicit choice. Removing the compatibility
aggregate is deferred to a separately announced change.

Applications depending directly on core, SDK libraries, or SDK starters should add
their configured provider before upgrading. For gateway registration with a smaller
classpath, retain the registry starter, exclude the unused implementation dependencies,
and declare the selected implementation explicitly. The SDK integration tests show
this arrangement for ZooKeeper. Admin declares its five supported discovery providers
explicitly and preserves independent connections for separate discovery configurations.

To roll back a dependency selection, restore the application's previous explicit
provider dependencies, or keep the compatibility starter if already used. Rolling back
the ShenYu version restores the old core aggregation. No stored configuration or SPI
key migration is required. The factory's cache and reinitialization behavior is unchanged.

## External implementations

External providers, including Consul and Polaris extensions, implement
`org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository`, carry `@Join`, and
publish this resource in their JAR:

```text
META-INF/shenyu/org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository
```

For example, `consul=com.example.ConsulInstanceRegisterRepository` preserves the existing
registration key. Add the extension JAR and its client dependencies to the runtime
classpath; no core POM change is needed. Existing ShenYu SPI classloader rules continue
to apply. Factory tests load external `consul` and `polaris` keys from isolated resources
without any built-in provider dependency.

## Dependency and release inventory

| Consumer | Registry implementation inventory |
| --- | --- |
| Registry API/core, SDK libraries and SDK starters | None |
| Compatibility registry starter | ZooKeeper, etcd, Nacos, Eureka, Kubernetes |
| Admin distribution | ZooKeeper, etcd, Nacos, Eureka, Kubernetes, explicitly declared |
| Bootstrap distribution | Registry API only; client libraries also arrive through sync/register/plugin modules |
| SDK HTTP/Dubbo integration gateways | ZooKeeper |
| Upload-plugin integration gateway | None; registry registration is not enabled |

The packaged Admin client inventory includes Curator 5.7.0, ZooKeeper 3.9.5, jetcd
0.7.3, Nacos 2.2.4, Eureka 2.0.2, and Kubernetes Java client 17.0.2. Bootstrap still
packages Curator, ZooKeeper, jetcd and Nacos through its other selected modules. Both
release archives include their LICENSE/NOTICE files. No registry client license entry
is removed by this change.

Admin retains the same client libraries, so its existing distribution LICENSE/NOTICE
entries still apply. Bootstrap distribution client libraries also come from selected
plugin, registration and synchronization starters; removing an edge from registry-core
alone does not remove those libraries. Keep the existing license and notice entries
for libraries still shipped. Custom distributions must inventory the actual packaged
`lib/` JARs and include their notices when selecting or removing providers.

CI checks resolved runtime trees for the API/core, both SDK libraries/starters, all six
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
