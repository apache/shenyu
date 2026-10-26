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
# Casdoor Plugin Store Cutover

The Casdoor authentication runtime plugin and Spring Boot starter are no longer built or shipped by the main ShenYu repository. Existing deployments that use Casdoor should add the external plugin-store starter:

```xml
<dependency>
    <groupId>org.apache.shenyu</groupId>
    <artifactId>shenyu-spring-boot-starter-plugin-casdoor</artifactId>
    <version>2.7.1-SNAPSHOT</version>
</dependency>
```

The external plugin keeps the same plugin name, order, plugin-data fields, callback parameters, and downstream identity headers (`name`, `id`, and `organization`) used by the former in-tree implementation. Certificates and secrets remain deployment-owned configuration and must not be committed to source control.

The main repository keeps `PluginEnum.CASDOOR` as a stable compatibility contract for the external plugin and existing admin data during the supported cutover window. The main repository no longer publishes the Casdoor runtime artifact or includes the Casdoor SDK in the default bootstrap distribution.

Rollback during the cutover window is a dependency-only change: remove the external starter and return to a ShenYu build that still shipped the in-tree Casdoor starter. Existing plugin data keeps the same plugin name and configuration schema.
