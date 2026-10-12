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

# Draft: Polaris Plugin Store Migration and Rollback

This draft records the root-repository handoff for the Polaris plugin-store extension. Root publishing and aggregator wiring remain separate review steps.

The plugin store owns these Polaris artifacts:

- `shenyu-admin-listener-polaris`
- `shenyu-sync-data-polaris`
- `shenyu-registry-polaris`
- `shenyu-spring-boot-starter-sync-data-polaris`

The validated store lane targets ShenYu API artifacts `2.7.2-SNAPSHOT` and Polaris SDK `1.13.0`. Real-server tests use Docker API `1.44` (selected with `-Dapi.version=1.44`) and the pinned Polaris standalone image `polarismesh/polaris-standalone@sha256:22c75382080a260e5d9fc9839b6657ae73f3154e308b8da881e1fab58653911c`.

## Migration draft

1. Review and publish the plugin-store Polaris artifacts.
2. Wire the root aggregator, dependency management, and CI paths only after store artifact ownership is approved.
3. Add the Polaris sync starter to gateways that should consume Polaris-backed sync data.
4. Configure admin listener and gateway sync settings with the same Polaris config namespace and file group.
5. Configure registry clients with the Polaris naming gRPC address when Polaris instance discovery is required.
6. Roll admin listener changes before gateway consumers so config files exist before gateways subscribe.

## Rollback draft

1. Stop Polaris listener writes and restore the previous admin sync channel.
2. Roll gateways back to the previous sync-data starter and settings.
3. Roll registry clients back to the previous registry implementation.
4. Keep Polaris config and naming state until no clients depend on it, then clean it up operationally.

## Tested boundary

The store real-server tests prove the config path from `PolarisDataChangedListener` through the real Polaris config SDK into `PolarisSyncDataService`, `BaseDataCache`, and a real `ShenyuWebHandler` test server. They cover create, update, delete, recreate, close, and fresh-subscriber recovery.

The registry real-server test proves `PolarisInstanceRegisterRepository` can register and query an instance against a real Polaris naming server, deregister that instance through the Polaris SDK boundary, observe removal through a fresh repository query, and close clients.

The tests do not start the full `shenyu-admin` Spring Boot app, do not cover dashboard or database persistence, and do not cover client annotation registration flows.
