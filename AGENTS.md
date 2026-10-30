# AGENTS.md

This file provides guidance to AI Coding CLI when working with code in this repository.

## Project

Apache ShenYu — a reactive API gateway for microservices. Java 17, Maven multi-module build. Read the root `pom.xml` for the current project version and build configuration. Use the wrapper `./mvnw`, not a local `mvn`.

## Commands

```bash
# Install the default reactor without running tests (test sources are still compiled)
# Checkstyle runs at validate; RAT runs at verify
./mvnw clean install -DskipTests

# Faster local build of one module and its dependencies
# Skips test execution, Javadoc, RAT and coverage; Checkstyle still runs
./mvnw -pl shenyu-admin -am clean install -DskipTests -Dmaven.javadoc.skip=true -Drat.skip=true -Djacoco.skip=true

# Run tests in a module and its reactor dependencies
./mvnw -pl shenyu-common -am test

# Run a single test class / method, allowing dependency modules with no match
./mvnw -pl shenyu-common -am test -Dtest=GsonUtilsTest -Dsurefire.failIfNoSpecifiedTests=false
./mvnw -pl shenyu-common -am test -Dtest='GsonUtilsTest#testToJson' -Dsurefire.failIfNoSpecifiedTests=false

# Checkstyle only
./mvnw checkstyle:check

# License headers only
./mvnw apache-rat:check
```

- `-am` includes reactor dependencies such as `shenyu-register-protocol`, so tests do not depend on previously installed ShenYu artifacts. A `-Dtest` selector applies to every selected module; with `-Dsurefire.failIfNoSpecifiedTests=false`, confirm that the intended class or method actually ran in Surefire's output.
- **Checkstyle** runs at `validate` in lifecycle builds. Config: `script/shenyu_checkstyle.xml`. It is strict (javadoc, import order, final parameters); match surrounding files. `-DskipTests` does not skip it; `-Dcheckstyle.skip=true` explicitly does.
- **Apache RAT** runs at `verify`, which `test` and `package` do not reach. New source files need the Apache 2.0 header (`script/checkstyle-header.txt`). `-DskipTests` does not skip RAT; `-Drat.skip=true` explicitly does. Run required checks before submitting a PR.
- Distribution packages: `make build-admin`, `make build-bootstrap` (see `Makefile`; builds via `shenyu-dist/*`). For image targets, override the Makefile's independent `VERSION` default with the root project version:

```bash
SHENYU_VERSION="$(./mvnw -q -N -DforceStdout help:evaluate -Dexpression=project.version)" &&
make build-all-image VERSION="$SHENYU_VERSION"
```

## Working principles

- **Think before coding.** Inspect relevant source, configuration, tests and current issue/PR state. State assumptions and tradeoffs affecting behavior, compatibility or scope; investigate repository evidence and ask only when material ambiguity blocks a correct decision. Make routine reversible decisions autonomously; use a short plan with observable checks for multi-step work.
- **Simplicity first.** Reuse existing plugin, SPI, configuration and observability mechanisms. Choose the smallest complete solution by behavior and maintenance cost. Avoid speculative options; a requested extension contract may start with one implementation when justified by a concrete use case. Retain handling for invalid input, network failures, cancellation, reload races and resource ownership.
- **Surgical changes.** Follow surrounding style, remove code made unused by your change and leave unrelated cleanup out. Preserve public SPI, plugin names, configuration defaults and supported legacy behavior unless the task changes them. Trace affected plugin/selector/rule contracts through shared DTOs, Admin persistence/validation/events, sync, gateway consumers and starters; coordinate necessary changes and tests. Check maintained SQL dialects and upgrade paths when persistence or seed metadata changes.
- **Goal-driven execution.** Define observable success; prefer a reproducing regression test for bugs and preserve externally observable behavior in refactors. Start with Maven-wrapper checks of affected modules/dependencies, then broaden according to risk and repository/CI requirements. Distinguish behavior tests from compilation or `-DskipTests` builds. For reactive changes, check affected subscription, completion/error/cancellation/backpressure and buffer/resource lifetimes. Verify cancellation follows resource ownership, including the original upstream call where applicable; headers, body completion and WebSocket session termination are distinct boundaries. Use factual/link/configuration checks for documentation changes. Report checks, results and material checks not run; existing Checkstyle, RAT/license and contribution requirements still apply.

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
