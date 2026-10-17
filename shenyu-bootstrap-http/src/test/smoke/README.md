<!--
  ~ Licensed to the Apache Software Foundation (ASF) under one or more
  ~ contributor license agreements.  See the NOTICE file distributed with
  ~ this work for additional information regarding copyright ownership.
  ~ The ASF licenses this file to You under the Apache License, Version 2.0
  ~ (the "License"); you may not use this file except in compliance with
  ~ the License.  You may obtain a copy of the License at
  ~
  ~     http://www.apache.org/licenses/LICENSE-2.0
  ~
  ~ Unless required by applicable law or agreed to in writing, software
  ~ distributed under the License is distributed on an "AS IS" BASIS,
  ~ WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  ~ See the License for the specific language governing permissions and
  ~ limitations under the License.
  -->

# Smoke test for shenyu-bootstrap-http (issue #7649)

Verifies the acceptance scenarios of the minimal HTTP gateway distribution:

| Step | Action | Expected gateway behaviour |
|------|--------|----------------------------|
| T1   | create divide selector + rule (admin REST), attach the stub upstream via `POST /shenyu-client/register-uri` | route + upstream synced over websocket; `GET /smoke/hello` -> **HTTP 200**, body from backend (`smoke-backend-ok`) |
| T2   | offline the upstream (`register-uri` with `eventType=OFFLINE`) | upstream removal synced; no healthy upstream -> **HTTP 200** with `{"code":-119,...}` (`CANNOT_FIND_HEALTHY_UPSTREAM_URL`) |
| T3   | re-register the upstream (`register-uri`) | upstream addition synced; proxying succeeds again -> **HTTP 200** + marker |
| T5   | kill the stub backend process while registered | connect refused on the still-cached upstream -> **HTTP 500** (`GlobalErrorHandler`) |
| T4   | delete rule + selector (admin REST) | deletion synced; no selector matches -> **HTTP 200** with `{"code":-107,...}` (`SELECTOR_NOT_FOUND` from divide) |

The gateway behaviours asserted above are the actual semantics of
`AbstractShenyuPlugin` / `DividePlugin` / `GlobalErrorHandler`, not approximations.

**Why register-uri for upstreams:** in current master the gateway only learns
divide upstreams from `DISCOVER_UPSTREAM` sync events
(`DivideUpstreamDataHandler` -> `UpstreamCacheManager`); a selector's `handle`
JSON is never parsed gateway-side, and the admin publishes `DISCOVER_UPSTREAM`
only for client registrations (`/shenyu-client/register-uri`) and
discovery-bound selectors. A manually maintained selector handle therefore
never becomes a healthy upstream. `/shenyu-client/**` is not on the admin shiro
white-list, so the script sends the admin `X-Access-Token` on register calls.

Last verified live: 2026-10-17, 10/10 assertions pass (admin @9095 H2 profile,
gateway @9195, websocket sync).

## Files

- `smoke-test.sh` — the test driver (bash + curl only; no jq/python required)
- `HttpStubBackend.java` — stub upstream; single-file source program, runs with any JDK 11+

## Run locally

```bash
# 1. start admin (H2 profile, websocket sync enabled - both are the defaults)
java -jar shenyu-admin/target/shenyu-admin.jar

# 2. start the minimal gateway (in another terminal)
#    if admin runs with a custom websocket token, export SHENYU_SYNC_WEBSOCKET_TOKEN first
java -jar shenyu-bootstrap-http/target/shenyu-bootstrap-http-exec.jar

# 3. run the smoke test (in another terminal)
bash shenyu-bootstrap-http/src/test/smoke/smoke-test.sh
```

Exit code is `0` when every assertion passes, `1` on assertion failure,
`2` when a prerequisite process is not reachable.

### Configuration (environment variables)

| Variable          | Default                                  | Meaning                        |
|-------------------|------------------------------------------|--------------------------------|
| `ADMIN_URL`       | `http://localhost:9095`                  | shenyu-admin base URL          |
| `GATEWAY_URL`     | `http://localhost:9195`                  | minimal gateway base URL       |
| `ADMIN_USER`      | `admin`                                  | admin login user               |
| `ADMIN_PASSWORD`  | `123456`                                 | admin login password           |
| `NAMESPACE_ID`    | `649330b6-c2d7-4edc-be8e-8a54df9eb385`   | default seeded namespace       |
| `BACKEND_PORT`    | `18080`                                  | stub upstream listen port      |
| `SYNC_WAIT`       | `3`                                      | seconds to wait after each admin change for websocket sync |

The script cleans up everything it creates (rule, selector, registered
upstream, stub backend) on exit, so it is safe to run repeatedly.
