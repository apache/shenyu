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
# Hystrix Plugin Store Cutover

The Hystrix runtime plugin and Spring Boot starter are no longer built or shipped by the main ShenYu repository. Existing deployments that still need Hystrix compatibility should add the external plugin-store starter:

```xml
<dependency>
    <groupId>org.apache.shenyu</groupId>
    <artifactId>shenyu-spring-boot-starter-plugin-hystrix</artifactId>
    <version>2.7.1-SNAPSHOT</version>
</dependency>
```

Hystrix is a deprecated legacy compatibility plugin. New deployments should use the Resilience4j plugin.

The main repository keeps `PluginEnum.HYSTRIX`, `HystrixHandle`, `HystrixIsolationModeEnum`, and related default constants as a stable compatibility contract for the external plugin and existing admin data during the supported cutover window. The main repository no longer publishes the Hystrix runtime artifact or includes Hystrix dependencies in the default bootstrap distribution.

Rollback during the cutover window is a dependency-only change: remove the external starter and return to a ShenYu build that still shipped the in-tree Hystrix starter. Existing selector and rule data keep the same plugin name and rule-handle schema.
