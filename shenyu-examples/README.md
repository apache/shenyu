# ShenYu Examples

This module contains example applications that demonstrate how to integrate your services with the Apache ShenYu gateway.

## Client Dependencies

The example modules now consume **independent [shenyu-client-java](https://github.com/apache/shenyu-client-java) artifacts** from Maven Central, rather than depending on in-tree source modules from the main `apache/shenyu` repository.

This reflects the recommended approach for real users: add the ShenYu client library as an external Maven dependency.

### How It Works

The parent POM (`shenyu-examples/pom.xml`) defines a property:

```xml
<shenyu.client.java.version>2.7.0.3</shenyu.client.java.version>
```

All `shenyu-client-*` and `shenyu-spring-boot-starter-client-*` dependencies in individual example POMs are managed by this property through the `<dependencyManagement>` section. The version is inherited automatically — individual examples do not specify a `<version>` for these artifacts.

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

## Testing with Unreleased Client Artifacts

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
cd shenyu
mvn clean install -pl shenyu-examples -Dshenyu.client.java.version=2.7.0.1-jdk8-SNAPSHOT
```

### Option 2: Modify the Property in Parent POM

For a more permanent change during development, edit the `<shenyu.client.java.version>` property in `shenyu-examples/pom.xml`:

```xml
<shenyu.client.java.version>2.7.0.1-jdk8-SNAPSHOT</shenyu.client.java.version>
```

> **Note**: When using `-SNAPSHOT` versions, you may need to configure the Apache Snapshot Repository in your `settings.xml` or POM:
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
mvn clean install -pl shenyu-examples -Dshenyu.client.java.version=2.7.0.1-jdk8-SNAPSHOT -U
```

The `-U` flag forces Maven to check for updated snapshots.

## Verification

After updating the client version, verify that the examples can register with a local ShenYu Admin and Bootstrap.

### Prerequisites

Before running any example, install the shared example utilities to your local Maven repository:

```bash
cd shenyu-examples
mvn install -pl shenyu-examples-common -DskipTests
```

### Run Examples

Run the example from the `shenyu-examples` reactor root using `-pl` so internal dependencies like `shenyu-examples-common` are resolved automatically:

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

> **Note**: Running `mvn spring-boot:run` directly from within an individual example directory (e.g., `cd shenyu-examples-springmvc && mvn spring-boot:run`) will fail because `shenyu-examples-common` and the parent POM cannot be resolved outside the reactor. Always use the `-pl` approach from the `shenyu-examples/` root.

### Confirm Registration

After the application starts:

1. Open the ShenYu Admin console (default: `http://localhost:9095`)
2. Check that the service appears under **Divide** (for HTTP), **Dubbo**, **gRPC**, or the respective plugin list
3. Verify API calls can be proxied through the ShenYu gateway (default: `http://localhost:9195`)
