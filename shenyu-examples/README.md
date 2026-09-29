# ShenYu Examples

This module contains example applications that demonstrate how to integrate your services with the Apache ShenYu gateway.

## Client Dependencies

The example modules resolve ShenYu client artifacts through Maven dependencies. Released client artifacts are published by [shenyu-client-java](https://github.com/apache/shenyu-client-java). The current development branch defaults to `2.7.2-SNAPSHOT` to use the MCP registration fixes in this repository; build and install these artifacts locally before building the examples.

This reflects the recommended approach for real users: add the ShenYu client library as an external Maven dependency.

### How It Works

The parent POM (`shenyu-examples/pom.xml`) defines a property:

```xml
<shenyu.client.java.version>2.7.2-SNAPSHOT</shenyu.client.java.version>
```

All `shenyu-client-*` and `shenyu-spring-boot-starter-client-*` dependencies in individual example POMs are managed by this property through the `<dependencyManagement>` section. The version is inherited automatically — individual examples do not specify a `<version>` for these artifacts.

This default applies to all examples. A clean standalone build cannot assume that these unreleased artifacts are available from Maven Central. Follow the local build steps below, or override the version to `2.7.0.3` for examples that do not require the MCP fixes.

### Which Artifacts Are Consumed Externally

| Client Artifact | Used By |
|----------------|---------|
| `shenyu-client-springmvc` | `shenyu-examples-springmvc`, `shenyu-examples-springmvc-tomcat` |
| `shenyu-spring-boot-starter-client-springmvc` | `shenyu-examples-http`, `shenyu-examples-http-swagger3`, `shenyu-examples-https`, `shenyu-examples-springcloud`, `shenyu-examples-websocket` (native), `shenyu-examples-mcp`, `shenyu-examples-sdk-http`, `shenyu-examples-sdk-feign` |
| `shenyu-spring-boot-starter-client-grpc` | `shenyu-examples-grpc` |
| `shenyu-spring-boot-starter-client-apache-dubbo` | `shenyu-examples-apache-dubbo-service`, `shenyu-examples-apache-dubbo-service-annotation` |
| `shenyu-client-apache-dubbo` | `shenyu-examples-apache-dubbo-service-xml`, `shenyu-examples-sdk-apache-dubbo-provider` |
| `shenyu-spring-boot-starter-client-tars` | `shenyu-examples-tars` |
| `shenyu-spring-boot-starter-client-sofa` | `shenyu-examples-sofa-service` |
| `shenyu-spring-boot-starter-client-spring-websocket` | `shenyu-examples-websocket` (annotation, native, reactive) |
| `shenyu-spring-boot-starter-client-mcp` | `shenyu-examples-mcp` |

### Internal Example Dependencies

The following dependencies remain internal to this repository (`${project.version}`):

- `shenyu-examples-common` — shared example utilities
- `shenyu-examples-dubbo-api` — Dubbo API definitions
- `shenyu-examples-sofa-api` — Sofa API definitions
- `shenyu-sdk-*` — ShenYu SDK modules (not extracted to shenyu-client-java)
- `shenyu-spring-boot-starter-sdk-*` — ShenYu SDK starters (not extracted to shenyu-client-java)

## Building with 2.7.2-SNAPSHOT

Use JDK 17 and run the following commands from the root of this `apache/shenyu` checkout. Use a checkout containing the MCP fixes: installing an unrelated snapshot from `shenyu-client-java` does not provide the changes in this repository.

### Build and Run the MCP Example

The MCP example uses both the MCP and Spring MVC client starters. Build both starters and their required reactor modules into the local Maven repository:

```bash
mvn install -pl shenyu-spring-boot-starter/shenyu-spring-boot-starter-client/shenyu-spring-boot-starter-client-mcp,shenyu-spring-boot-starter/shenyu-spring-boot-starter-client/shenyu-spring-boot-starter-client-springmvc -am -DskipTests
```

Build the MCP example and its shared example utilities using the separate examples reactor:

```bash
mvn -f shenyu-examples/pom.xml install -pl shenyu-examples-mcp -am \
  -Dshenyu.client.java.version=2.7.2-SNAPSHOT -DskipTests
```

Before starting the example, configure `shenyu-examples/shenyu-examples-mcp/src/main/resources/application.yml` for your ShenYu Admin address, credentials, namespace, and reachable service address. Run ShenYu Admin and Bootstrap from a checkout containing the corresponding MCP gateway fixes.

```bash
mvn -f shenyu-examples/pom.xml spring-boot:run -pl shenyu-examples-mcp \
  -Dshenyu.client.java.version=2.7.2-SNAPSHOT
```

Keep the same version override for build and run commands. This selects `2.7.2-SNAPSHOT` for both starters through the parent dependency management. Check the resolved ShenYu dependencies with:

```bash
mvn -f shenyu-examples/pom.xml dependency:tree -pl shenyu-examples-mcp \
  -Dshenyu.client.java.version=2.7.2-SNAPSHOT '-Dincludes=org.apache.shenyu:*'
```

Confirm that the selected MCP and Spring MVC starters and their shared client dependencies use `2.7.2-SNAPSHOT`. Then verify that the example registers its upstream and MCP tools in ShenYu Admin and that a tool invocation through the gateway succeeds.

### Build Other Examples

To prepare client dependencies for other examples using the default snapshot version, install the client and client starter modules and their dependencies from the repository root:

```bash
mvn install -pl shenyu-client,shenyu-spring-boot-starter/shenyu-spring-boot-starter-client -am -DskipTests
```

Examples with internal SDK dependencies also require the corresponding SDK modules to be installed.

For an example that does not require the MCP fixes, you can use the released client artifacts without changing any POM. For example, from the repository root:

```bash
mvn -f shenyu-examples/pom.xml install -pl shenyu-examples-springmvc -am \
  -Dshenyu.client.java.version=2.7.0.3 -DskipTests
mvn -f shenyu-examples/pom.xml spring-boot:run -pl shenyu-examples-springmvc \
  -Dshenyu.client.java.version=2.7.0.3
```

## Testing with Other Unreleased Client Artifacts

When developing or testing changes to the `shenyu-client-java` library, you may need to build the client artifacts locally and use them in these examples.

### Option 1: Override the Version via Command Line

Build the `shenyu-client-java` project locally, install the artifacts to your local Maven repository:

```bash
git clone https://github.com/apache/shenyu-client-java.git
cd shenyu-client-java
mvn clean install -DskipTests
```

This installs the artifacts with the project's snapshot version (e.g., `2.7.0.1-jdk8-SNAPSHOT`). Then, build the examples using that version:

```bash
# Run from the apache/shenyu repository root; select the example to test.
mvn -f shenyu-examples/pom.xml clean install -pl shenyu-examples-springmvc -am \
  -Dshenyu.client.java.version=2.7.0.1-jdk8-SNAPSHOT
```

### Option 2: Modify the Property in Parent POM

For a more permanent change during development, edit the `<shenyu.client.java.version>` property in `shenyu-examples/pom.xml`:

```xml
<shenyu.client.java.version>2.7.0.1-jdk8-SNAPSHOT</shenyu.client.java.version>
```

> **Note**: Locally installed snapshots do not require a remote snapshot repository. If the required snapshot has been published, you can configure the Apache Snapshot Repository in your POM or an active profile in `settings.xml` (inside `<repositories>`):
> ```xml
> <repository>
>     <id>apache-snapshots</id>
>     <url>https://repository.apache.org/content/repositories/snapshots/</url>
>     <snapshots>
>         <enabled>true</enabled>
>     </snapshots>
> </repository>
> ```

### Option 3: Use Maven `-U` Flag for Latest Snapshots

If the snapshot has been published to the Apache snapshot repository:

```bash
mvn -f shenyu-examples/pom.xml clean install -pl shenyu-examples-springmvc -am \
  -Dshenyu.client.java.version=2.7.0.1-jdk8-SNAPSHOT -U
```

The `-U` flag forces Maven to check for updated snapshots. It does not publish missing artifacts or replace the local build required for unpublished fixes.

## Verification

After updating the client version, verify that the examples can register with a local ShenYu Admin and Bootstrap.

### Prerequisites

After preparing the client dependencies as described above, install the shared example utilities and their parent POM to your local Maven repository:

```bash
cd shenyu-examples
mvn install -pl shenyu-examples-common -am -DskipTests
```

### Run Examples

Run the example from the `shenyu-examples` reactor root using `-pl`. The run command uses the internal dependencies installed in the previous step:

```bash
cd shenyu-examples

# Spring MVC HTTP example
mvn spring-boot:run -pl shenyu-examples-springmvc

# Or other protocol examples:
# mvn spring-boot:run -pl shenyu-examples-grpc
# mvn spring-boot:run -pl shenyu-examples-dubbo/shenyu-examples-apache-dubbo-service
# mvn spring-boot:run -pl shenyu-examples-websocket/shenyu-example-spring-annotation-websocket
# mvn spring-boot:run -pl shenyu-examples-mcp
```

> **Note**: `-pl` selects an example but does not build its dependencies. Install the required internal modules before running it. If you used a client version override when building, pass the same override to `spring-boot:run`.

### Confirm Registration

After the application starts:

1. Open the ShenYu Admin console (default: `http://localhost:9095`)
2. Check that the service appears under **Divide** (for HTTP), **Dubbo**, **gRPC**, or the respective plugin list
3. Verify API calls can be proxied through the ShenYu gateway (default: `http://localhost:9195`)
