# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

Apache ShenYu — a reactive API gateway for microservices. Java 17, Maven multi-module build (root `pom.xml`, version `2.7.2-SNAPSHOT`). Use the wrapper `./mvnw`, not a local `mvn`.

## Commands

```bash
# Full build (checkstyle + RAT license check run automatically at validate phase)
./mvnw clean install -DskipTests

# Faster build of one module and its dependencies
./mvnw -pl shenyu-admin -am clean install -DskipTests -Dmaven.javadoc.skip=true -Drat.skip=true -Djacoco.skip=true

# Run all tests in a module
./mvnw test -pl shenyu-common

# Run a single test class / method (surefire)
./mvnw test -pl shenyu-common -Dtest=GsonUtilsTest
./mvnw test -pl shenyu-common -Dtest='GsonUtilsTest#testToJson'

# Checkstyle only
./mvnw checkstyle:check
```

- **Checkstyle is enforced on every build** (validate phase). Config: `script/shenyu_checkstyle.xml`. It is strict (javadoc, import order, final parameters); match the style of surrounding files.
- **Apache RAT** enforces license headers: every new source file needs the Apache 2.0 header (`script/checkstyle-header.txt`).
- Distribution packages / Docker images: `make build-admin`, `make build-bootstrap`, `make build-all-image` (see `Makefile`; builds via `shenyu-dist/*`).

## Architecture

ShenYu is split into a **control plane** and a **data plane** that communicate only through data-sync channels — the gateway never reads the database.

- **`shenyu-admin`** — control plane. Spring MVC + MyBatis app (port 9095) with a web dashboard. Persists plugins/selectors/rules/metadata (DB init scripts under `db/`; H2 by default, MySQL/PG/Oracle supported). Structure: `controller` → `service` → `mapper`, with `listener` publishing config-change events.
- **`shenyu-admin-listener`** + **`shenyu-sync-data-center`** — config sync from admin to gateways. One submodule per channel: websocket (default), http long-polling, zookeeper, nacos, etcd, consul, apollo, polaris. Gateway-side subscribers update in-memory caches; changed config takes effect without restart.
- **`shenyu-bootstrap`** — data plane. A thin Spring WebFlux launcher; its behavior is composed by which starters are on its classpath (`shenyu-spring-boot-starter-*`). Port 9195.
- **`shenyu-web`** — gateway runtime core. `ShenyuWebHandler` (`shenyu-web/.../web/handler/ShenyuWebHandler.java`) executes an ordered chain of plugins per request.
- **`shenyu-plugin`** — all gateway features are plugins.
  - `shenyu-plugin-api`: `ShenyuPlugin` (reactive `execute(exchange, chain)` + `getOrder()`), `ShenyuPluginChain`, `ShenyuContext`.
  - `shenyu-plugin-base`: `AbstractShenyuPlugin` implements selector → rule matching against cached config, then calls the concrete plugin's `doExecute`; `PluginDataHandler` implementations receive config updates from data sync into per-plugin caches.
  - Feature plugins grouped by category: `shenyu-plugin-proxy` (dubbo, grpc, spring-cloud, sofa, tars, websocket, mqtt...), `-security`, `-logging`, `-cache`, `-fault-tolerance`, `-ai`, `-mcp-server`, etc. A plugin is activated by adding its starter to the bootstrap pom.
- **`shenyu-spi`** — custom SPI extension mechanism: `@SPI` on the interface, `@Join` on implementations, registration files under `META-INF/shenyu/`. Used for load balancers, condition matchers, etc. (`shenyu-loadbalancer` plugs in this way).
- **`shenyu-client`** + **`shenyu-register-center`** — client-side API registration. Backend services annotate endpoints (e.g. `@ShenyuSpringMvcClient`) and register metadata/URIs to admin through a register channel (http, zookeeper, nacos...). Admin consumes registrations through `shenyu-disruptor` (async event pipeline).
- **`shenyu-common`** — shared model (`PluginData`, `SelectorData`, `RuleData`, `MetaData`), enums, constants, utils. Config semantics live here, so changes ripple to both admin and gateway.

Typical config flow: dashboard/API change in admin → DB write + change event → sync channel → gateway subscriber updates cache → `AbstractShenyuPlugin` matches subsequent requests against new selectors/rules.

## Testing modules

- Unit tests live per module (surefire).
- `shenyu-integrated-test` and `shenyu-e2e` are Docker-based integration/e2e suites run in CI; not part of a normal local build.
- `shenyu-examples` contains sample backend services for manually exercising the gateway.

## GitNexus code intelligence

This repo is indexed by the GitNexus MCP server (repo name `shenyu`). Prefer `query`/`context` for finding execution flows across the admin↔gateway boundary, and `impact` before refactoring widely-used symbols (e.g. anything in `shenyu-common`). Skill docs live under `.claude/skills/gitnexus/`.
