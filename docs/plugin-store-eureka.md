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

# Eureka plugin-store migration

Prepared cutover for apache/shenyu#7596. The receiving extension owns
shenyu-registry-eureka and shenyu-spring-boot-starter-registry-eureka.
The main-repository removal must remain a draft until compatible artifacts are
published and the complete Admin/gateway deployment gate below passes.
Source preparation, local snapshot installation and this guide are not publication.

## Compatibility and ownership

Development validation uses store 2.7.1-SNAPSHOT, core 2.7.2-SNAPSHOT at 87c22dde7,
Microsoft OpenJDK 17.0.18 and Netflix eureka-client/eureka-client-jersey3 2.0.2.
The disposable server uses ShenYu's Spring Cloud Eureka example (4.1.2).
These are development coordinates and test scope, not a released compatibility matrix.

The provider keeps its packages, non-singleton @Join annotation, eureka type key,
SPI resource, registration/discovery/lease behavior and polling-failure recovery.
No core factory dispatch or enum/SQL key changes are needed. It resolves through
ShenyuInstanceRegisterRepositoryFactory with an external provider JAR.
This transfer does not redesign Eureka instance identity, authentication or refresh policy.
In particular, current identity is derived by Eureka's configured instance builder;
do not treat the single-identity update test as certification for multiple replicas
using an identical default identity.

The registry adapter is independent of Spring Cloud backend service discovery.
The generic main-repository Eureka server example and Spring Cloud transport E2E
fixtures remain usable while their broader test ownership is reviewed. They are not
default Admin/bootstrap registry dependencies and do not justify retaining the in-tree
registry provider.

## Install

Use a published store version explicitly announced compatible with the selected
core release. Keep one owner per Maven group/artifact/version. The same provider
coordinates/packages cannot coexist from both repositories on a runtime classpath.

For gateway instance registration, add:

```xml
<dependency>
    <groupId>org.apache.shenyu</groupId>
    <artifactId>shenyu-spring-boot-starter-registry-eureka</artifactId>
    <version>${store.version}</version>
</dependency>
```

The external starter retains the core registry lifecycle and explicitly excludes
the core starter's old transitive Eureka provider. It contributes its external
provider instead. Other retained compatibility providers may remain on that
starter's classpath; exclude those explicitly if building a minimal assembly.

Admin discovery and SDK deployments that already own their registration/discovery
wiring need the provider rather than the gateway starter:

```xml
<dependency>
    <groupId>org.apache.shenyu</groupId>
    <artifactId>shenyu-registry-eureka</artifactId>
    <version>${store.version}</version>
</dependency>
```

Resolve the complete runtime dependency closure, including Eureka's Jersey 3
transport, when installing into lib/ distributions. Do not copy only the provider
JAR or replace retained core API/SPI classes with bundled duplicates.
The source remains ASF/Apache-2.0. Preserve Netflix/Jersey and resolved third-party
licenses/notices in custom binary distributions.

## Enable and verify

Gateway configuration retains its keys:

```yaml
shenyu:
  register:
    enabled: true
    registerType: eureka
    serverLists: http://eureka:8761/eureka/
    props:
      name: shenyu-gateway
      host: gateway
```

The listener uses the actually bound server port unless props.port explicitly
overrides it. Existing SDK register-type=eureka and server-list settings retain
their current meaning; HTTP annotation registration to Admin is a separate flow.
Do not change Admin's HTTP client-registration transport to eureka merely to
enable the instance/discovery adapter.

The starter tests cover disabled-by-default wiring, external SPI construction,
configuration binding and one registration at the bound port. Unit tests retain
the source module's polling and shutdown checks. The opt-in eureka-live Maven
profile launches no fake service: it requires -Deureka.test.url pointing at a
real disposable server and verifies registration, initial discovery, an instance
update/refresh, deregistration and watch shutdown.

Example verification from a checkout with the core Maven wrapper available:

```text
core/mvnw -f store/pom.xml -pl shenyu-extension/shenyu-extension-eureka/shenyu-spring-boot-starter-registry-eureka -am test
core/mvnw -f store/pom.xml -pl shenyu-extension/shenyu-extension-eureka/shenyu-registry-eureka -am verify -Peureka-live -Deureka.test.url=http://127.0.0.1:8761/eureka/
```

The live profile must fail if its server URL is missing/unreachable; ordinary unit
tests do not silently substitute for it. Its CI lane builds the server and core
API artifacts from a pinned main commit, then runs the receiving provider.

## Cutover gates

1. Merge/review the receiving code and confirm published artifact ownership and
   compatible core/store versions without duplicate coordinates.
2. Run the live provider lane and a complete disposable Eureka + Admin + bootstrap
   environment using only the external runtime provider. Verify gateway instance
   registration at its bound address, discovery, config refresh, deregistration,
   healthy sibling behavior and restart/rollback. Record image/JAR digests and logs.
3. Package main Admin/bootstrap after removal and inspect dependency trees and
   release licenses. Keep other registry implementations and SDK boundary tests green.
4. Publish external artifacts first, then merge the main removal in the coordinated
   release and announce migration/rollback versions. Do not close #7596 before the
   complete deployment and publication evidence exists.

Local evidence: six unit/SPI/starter tests pass with Checkstyle and RAT. The real
server lifecycle test also passes locally. Full Admin/bootstrap/Eureka container
E2E has not been run: the local Docker Linux-engine pipe was unavailable even
after a desktop launch attempt. That gate is outstanding, not certified by the
provider-only test.

## Upgrade and rollback

Back up the Admin database, registry configuration and exact old images/JARs.
Install the complete external closure in staging, remove the in-tree provider
version, and keep the eureka key and application/instance configuration unchanged.
Confirm discovery refresh and route/instance removal after a process dies; do not
infer success from a Maven build or an HTTP registration acknowledgement alone.

For rollback, stop/drain the new processes and restore the last core distribution
that included the provider, removing the external copy. Preserve database/type
keys and Eureka applications. Clean up or expire obsolete instance records using
the deployment's existing lease/identity procedure. Do not clear the whole registry
or run two versions with identical classes in the same JVM.

