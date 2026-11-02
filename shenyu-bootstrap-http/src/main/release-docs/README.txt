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

Apache ShenYu minimal HTTP gateway
=================================

This build-yourself, opt-in assembly is outside the standard release profile.
It is not an official ASF binary release. Build from a full source checkout
with JDK 17+ and the repository Maven wrapper:

  ./mvnw -Pminimal-http -pl shenyu-bootstrap-http -am clean install -DskipTests

Only WebSocket configuration sync and the Global, Divide, URI, HTTP Client
and Response plugins are included. HTTP upstreams are configured in Admin;
authentication, rate limiting, caching, RPC and AI plugins are not packaged.

Run from the extracted apache-shenyu-http directory, using JDK 17+.
Configure these environment variables to match your Admin namespace and policy:

  SHENYU_NAMESPACE=649330b6-c2d7-4edc-be8e-8a54df9eb385
  SHENYU_SYNC_WEBSOCKET_URLS=ws://localhost:9095/websocket
  SHENYU_SYNC_WEBSOCKET_ALLOW_ORIGIN=ws://localhost:9195
  SHENYU_SYNC_WEBSOCKET_TOKEN=<your-admin-sync-token>
  JAVA_OPTS=-Xms512m -Xmx1024m -XX:MaxDirectMemorySize=256m

On Unix, export the variables and run: sh bin/start.sh
On Windows, set the variables and run: bin\start.bat
The launcher runs in the foreground. Stop with the normal process signal.
To override the default HTTP port 9195: bin/start.sh --server.port=9295

Readiness: http://localhost:9195/actuator/health/readiness
Liveness:  http://localhost:9195/actuator/health/liveness
Readiness stays unavailable until the initial WebSocket snapshot is applied.
Use a compatible Admin from the same source revision. Initial-sync readiness
is a startup latch, not a guarantee of ongoing Admin or upstream connectivity.
For older Admin versions, disable shenyu.sync.websocket.initial-sync-readiness
and verify a real route before accepting traffic.

To add features, declare compatible starters in the application POM and rebuild.
The assembly reuses the full distribution's conservative legal-file superset;
the lib/ directory identifies the actual packaged dependencies.
Further build, smoke-test and measurement instructions are in the source tree:
  docs/performance/minimal-http.md
