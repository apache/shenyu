# ShenYu Examples

This module contains example applications that demonstrate how to integrate your services with the Apache ShenYu gateway.

## Client Dependencies

During the [Java client migration](../shenyu-register-center/CLIENT_MIGRATION.md),
these examples use the client modules still staged in the main `apache/shenyu`
repository. Build their dependencies from the same checkout. The examples do not
select artifacts from the independent `shenyu-client-java` repository.

### How It Works

The [examples parent POM](pom.xml) manages the client and client-starter dependencies
below with `${project.version}`. The examples form a separate Maven reactor from
main, so install the required main modules before building an example. Both reactors
must use the same checkout version.

The former `shenyu.client.java.version` property is no longer used by this POM.
Passing it on the command line does not select a different client. To verify an
external migration candidate, use its own examples or a separate test application
as described in the [contribution guide](../docs/split-client-contribution.md).

### Client Artifacts Used by the Examples

| Client Artifact | Used By |
|----------------|---------|
| `shenyu-client-springmvc` | `shenyu-examples-springmvc`, `shenyu-examples-springmvc-tomcat` |
| `shenyu-spring-boot-starter-client-springmvc` | `shenyu-examples-http`, `shenyu-examples-http-swagger3`, `shenyu-examples-https`, `shenyu-examples-springcloud`, `shenyu-examples-websocket` (native), `shenyu-examples-mcp`, `shenyu-examples-sdk-http`, `shenyu-examples-sdk-feign` |
| `shenyu-spring-boot-starter-client-grpc` | `shenyu-examples-grpc` |
| `shenyu-spring-boot-starter-client-apache-dubbo` | `shenyu-examples-apache-dubbo-service`, `shenyu-examples-apache-dubbo-service-annotation` |
| `shenyu-client-apache-dubbo` | `shenyu-examples-apache-dubbo-service-xml`, `shenyu-examples-sdk-apache-dubbo-provider` |
| `shenyu-spring-boot-starter-client-spring-websocket` | `shenyu-examples-websocket` (annotation, native, reactive) |
| `shenyu-spring-boot-starter-client-mcp` | `shenyu-examples-mcp` |

### Internal Example Dependencies

The following dependencies also remain main-owned (`${project.version}`):

- `shenyu-examples-common` — shared example utilities
- `shenyu-examples-dubbo-api` — Dubbo API definitions
- `shenyu-sdk-*` — ShenYu SDK modules, outside the client split
- `shenyu-spring-boot-starter-sdk-*` — ShenYu SDK starters, outside the client split

Client-dependent examples may move with the migration; that does not transfer
ownership of SDK modules. See the [ownership inventory](../docs/java-client-repository-boundary.md#inventory-and-migration-baseline).

## Testing the MCP Example from This Checkout

Use JDK 17 / Spring Boot 3 and run all commands below from the root of this
`apache/shenyu` checkout. Confirm the selected runtime with `./mvnw -version`.
Use `mvnw.cmd` on Windows. A lower compiler target in an individual example POM
does not establish Java 8 support for its Boot 3 application and dependencies.

### Build and Run the MCP Example

The MCP example needs both MCP and Spring MVC client starters. Build them and their
required main modules into the local Maven repository:

```bash
./mvnw install -pl shenyu-spring-boot-starter/shenyu-spring-boot-starter-client/shenyu-spring-boot-starter-client-mcp,shenyu-spring-boot-starter/shenyu-spring-boot-starter-client/shenyu-spring-boot-starter-client-springmvc -am -DskipTests -Dmaven.javadoc.skip=true
```

Build the example and its shared utilities using the separate examples reactor:

```bash
./mvnw -f shenyu-examples/pom.xml install -pl shenyu-examples-mcp -am -DskipTests
```

These commands prepare artifacts and skip test execution; they are not behavior
verification. Check the resolved dependencies before running the example:

```bash
./mvnw -f shenyu-examples/pom.xml dependency:tree -pl shenyu-examples-mcp '-Dincludes=org.apache.shenyu:*'
```

Confirm that both starters and shared client dependencies use this checkout's
version, as declared in the root and examples POMs, rather than a historical JDK8
client or an external migration candidate.

Configure [application.yml](shenyu-examples-mcp/src/main/resources/application.yml)
for the test Admin address, credentials, namespace and reachable service address.
Run Admin and bootstrap with compatible registration, sync and MCP gateway settings.
Then start the example:

```bash
./mvnw -f shenyu-examples/pom.xml spring-boot:run -pl shenyu-examples-mcp
```

Verify that its upstream and MCP tools register in Admin and that an actual tool
invocation through the gateway succeeds. A successful build alone does not prove
registration or routing.

## Testing with Other Unreleased Client Artifacts

For changes to in-tree clients, build the relevant main modules and examples from
the same checkout as above. For `shenyu-client-java` migration work, select the
candidate identified by its issue/PR and follow its preparation guide through the
[split-client contribution workflow](../docs/split-client-contribution.md#build-and-test-the-split-client).
Do not inject external clients into main's examples or add a version override to
bypass the one-way dependency boundary. Historical Java 8 / Boot 2 instructions
apply only to explicitly selected historical implementations, not these examples.

## Verification

### Prerequisites

For a general example, first install its required main modules. If they have not
already been built, a complete main build installs them and runs the main tests:

```bash
./mvnw clean install -Dmaven.javadoc.skip=true
```

Then build the selected example and its example dependencies. For Spring MVC:

```bash
./mvnw -f shenyu-examples/pom.xml install -pl shenyu-examples-springmvc -am -DskipTests
```

Installing only `shenyu-examples-common` does not prepare all main client, SDK or
protocol dependencies. For another example, select its actual module path and
build its dependencies before starting it.

### Run Examples

From the main repository root, run the prepared Spring MVC example:

```bash
./mvnw -f shenyu-examples/pom.xml spring-boot:run -pl shenyu-examples-springmvc
```

`spring-boot:run -pl` selects an application; it does not build and install missing
reactor dependencies. Configure that application's Admin address and credentials
and start Admin/bootstrap with matching sync settings and the required plugin first.

### Confirm Registration

After the application starts:

1. Inspect its registered metadata, URI and resulting selector/rule/upstream in Admin
   (default: `http://localhost:9095`).
2. Verify a request through the gateway (default: `http://localhost:9195`) and compare
   the response with a direct backend request. For MCP, also invoke the registered tools.
3. Exercise heartbeat, offline or discovery behavior when the change affects it.

Record commits, resolved dependency versions, JDKs, registration/sync channels and
observed results. These source checks do not certify a released client/Admin pair;
use the [compatibility matrix](../docs/java-client-compatibility.md) and
[release gates](../shenyu-register-center/CLIENT_MIGRATION.md#release-order-and-gates)
for published support claims.
