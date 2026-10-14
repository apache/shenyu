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
# Split Java client migration and contribution guide

Use this guide with the [contributor guide](https://shenyu.apache.org/community/contributor-guide/)
and the [issue and pull request conventions](https://shenyu.apache.org/community/issue-pr/).
It describes the migration tracked by [#6373](https://github.com/apache/shenyu/issues/6373).

## Choose the repository

The split [Java client repository](https://github.com/apache/shenyu-client-java) lets
applications register with ShenYu while keeping a Java 8 and Spring Boot 2 baseline.
Admin and gateway development in this repository uses Java 17 and Spring Boot 3.
Keeping these build baselines separate lets client changes be developed and tested
without requiring a business application to adopt the gateway's JDK baseline.

| Change | Repository |
| --- | --- |
| Java 8 client annotations, registration code, auto-configuration, or client starters | `apache/shenyu-client-java` |
| Admin registration endpoints, persistence, validation, or gateway behavior | `apache/shenyu` |
| Registration payload or behavior that changes both producer and consumer | Coordinated changes in both repositories |
| Published website documentation | `apache/shenyu-website` |

The main reactor still includes `shenyu-client`, `shenyu-register-center`, and
client starters. The split is a migration, not a completed removal of those modules.
Check which artifact and version the application actually consumes before choosing
a repository. A fix to the split client does not automatically fix its in-tree
counterpart; describe whether the affected implementation also needs a corresponding
change. Do not move unrelated runtime code into the client repository.

## JDK and framework baselines

| Build | Baseline | Verification configuration |
| --- | --- | --- |
| Main repository | `java.version=17`, Spring Boot 3 | Main CI builds on JDK 17 |
| Split Java client | `java.version=1.8`, Spring Boot 2 | Client CI runs `./mvnw -B clean test` on JDK 8, 17, 21, 23, and 25 |

Read each repository's `pom.xml` and `.github/workflows/ci.yml` before updating these
values. The client matrix specifies which JDKs CI attempts to test; it is not a
claim that every job passed or that every client/admin release combination works.
Link the actual workflow results and record the tested commits in a PR.
Do not override `java.version` when running the matrix: the client must retain its
Java 8 source and bytecode baseline even when Maven runs under a newer JDK.

The split client's [migration progress](https://github.com/apache/shenyu-client-java/blob/main/progress.md)
records build evidence and remaining integration work. Use the version compatibility
matrix requested in [#6370](https://github.com/apache/shenyu/issues/6370) for
release-pair support decisions once it is available; do not infer a supported pair
from similar version numbers.

## Set up a contribution

Fork the repository you will change. For the main repository:

```sh
git clone https://github.com/YOUR_USERNAME/shenyu.git
cd shenyu
git remote add upstream https://github.com/apache/shenyu.git
git fetch upstream
git switch -c docs/split-client-contribution upstream/master
```

For a client contribution, use your fork of `shenyu-client-java`, set its upstream to
`https://github.com/apache/shenyu-client-java.git`, and branch from `upstream/main`.
Keep your fork's default branch for upstream synchronization and develop on a
separate branch. Find or open an issue describing the change and check its comments
and related PRs before duplicating work.

Both repositories provide `./mvnw`. Set `JAVA_HOME` to the JDK for that build and
put `$JAVA_HOME/bin` first in `PATH`. Confirm the selected runtime with
`./mvnw -version`; a locally installed `mvn` is not required.

## Build and test the split client

Run from the root of the split client checkout:

```sh
./mvnw -version
./mvnw -B clean test
./mvnw -B clean install -Dmaven.javadoc.skip=true
```

For a focused Spring MVC starter change, build its dependencies too:

```sh
./mvnw -B -pl shenyu-spring-boot-starter-client/shenyu-spring-boot-starter-client-springmvc -am clean test
```

The focused build does not replace the complete client CI matrix. To repeat that
matrix locally, install all five JDKs and set these variables to their JDK home
directories. Run this from the client root:

```sh
for client_jdk in "$JDK8_HOME" "$JDK17_HOME" "$JDK21_HOME" "$JDK23_HOME" "$JDK25_HOME"; do
  test -x "$client_jdk/bin/java" || { echo "Missing client matrix JDK: $client_jdk"; exit 1; }
  JAVA_HOME="$client_jdk" PATH="$client_jdk/bin:$PATH" ./mvnw -version || exit 1
  JAVA_HOME="$client_jdk" PATH="$client_jdk/bin:$PATH" ./mvnw -B clean test || exit 1
done
```

Capture each result separately so failures can be tied to a JDK. If a JDK is not
available locally, report that gap and inspect the matching CI job. A successful
`-DskipTests` build does not verify test behavior.

## Test local client artifacts with admin and the gateway

1. In the split client checkout, run `./mvnw -B clean install -Dmaven.javadoc.skip=true`.
   Record its commit and the `<version>` in its root POM. This installs that version
   in your local Maven repository; it does not change the main reactor's artifacts.
2. In a separate Spring Boot 2 test application, select the split client's starter
   and set an explicit version matching the installed artifacts. For Spring MVC,
   the artifact is `org.apache.shenyu:shenyu-spring-boot-starter-client-springmvc`.
   Use the same version for other split client artifacts in that application.
3. Inspect the application's resolved dependencies before running it:

   ```sh
   ./mvnw dependency:tree -Dincludes=org.apache.shenyu
   ```

   Confirm the selected client version and that an in-tree client or another
   transitive dependency has not replaced it. The main repository's examples use
   its own dependency management; building an unchanged example does not prove
   that the split client was exercised. Do not replace a Spring Boot 3 example's
   starter with the Spring Boot 2 client and assume framework compatibility.
4. Build the main checkout with JDK 17:

   ```sh
   ./mvnw -B clean install -Dmaven.javadoc.skip=true
   ```

   Start admin and bootstrap using the [local deployment guide](https://shenyu.apache.org/docs/deployment/deployment-local/).
   Use a test environment and record the main commit, active profiles, registration
   channel, and sync channel. Configure admin and the gateway to use the same sync
   settings and enable the plugin needed by the application.
5. Configure the test application's `shenyu.register.registerType`,
   `shenyu.register.serverLists`, and channel-specific properties for that environment.
   With HTTP registration, `serverLists` points to admin, not the gateway, and the
   client reads credentials from `shenyu.register.props.username` and
   `shenyu.register.props.password`. Use the credentials required by your test admin.
6. Start the application. Verify that metadata and URI registration succeed, inspect
   the resulting selector/rule/upstream in admin, and call the registered route through
   the gateway. Compare its response with a direct request to the application.
   Exercise deregistration or heartbeat/discovery changes when the patch affects
   those paths. For API-doc or MCP changes, also verify their corresponding registration
   and downstream behavior; a basic HTTP route does not cover those features.

Save the application dependency tree and the client/admin/gateway observations as
verification evidence. Client unit tests alone cannot establish client-to-admin
wire compatibility or successful gateway routing.

## Coordinate changes across repositories

HTTP registration crosses the repository boundary as JSON. The split client defines
its registration DTOs in `shenyu-client-core`; admin handles incoming registration
in `ShenyuClientHttpRegistryController` and uses its own DTOs. Compare serialized
field names, types, required values, authentication, and responses rather than
assuming the Java package names must match.

A client-only implementation fix that preserves the registration contract generally
needs a client PR. An admin-only fix that preserves accepted client requests generally
needs a main PR. Changes to required fields, endpoints, response semantics, or
registration lifecycle need an explicit compatibility decision and coordinated PRs
when both sides must change.

For coordinated work:

- Cross-link the issue and both PRs, and describe which side can be deployed first.
- Preserve old-client/new-admin behavior where required, and test new-client/old-admin
  behavior or clearly describe the unsupported combination.
- Add producer-side serialization or registration tests and consumer-side parsing,
  validation, and persistence tests for the changed contract.
- Run a real registration and gateway check with the intended pair of commits.
- Record any rollout constraints and update compatibility documentation when a
  supported combination changes. Track a protocol proposal separately from behavior
  already present on the default branches.

## PR checklists

For a split client PR:

- [ ] Follow the contribution guidelines and link the relevant issue.
- [ ] Keep Java 8 source compatibility and the Spring Boot 2 baseline.
- [ ] Add regression tests for changed behavior and run the affected module tests.
- [ ] Check the JDK 8/17/21/23/25 matrix and report any unverified jobs.
- [ ] Run `./mvnw clean install -Dmaven.javadoc.skip=true`, or explicitly report why
  it has not completed in the repository's PR template.
- [ ] For registration changes, record the admin/gateway commit pair and integration
  results; link a main-repository PR if the consumer must change.

For a main-repository PR affecting clients:

- [ ] Identify affected in-tree and split client artifacts and the wire contract.
- [ ] Add regression tests for the admin or gateway behavior and retained compatibility.
- [ ] Build and test with JDK 17 and complete the main repository's PR template.
- [ ] Verify the intended client/admin pair, including older clients where required.
- [ ] Link coordinated client changes and explain rollout order or version constraints.

Use the official commit format, for example
`docs (client) : document split client contribution workflow.`
A corresponding PR title is
`[ISSUE #6373] Document split client migration and contribution workflow.`
Push the branch to your fork and open the PR against the upstream default branch.
Describe the final change and the checks actually run. Leave unfinished checks
unchecked, respond to review on the same branch, and inspect CI before requesting
merge. Labels, milestones, review, and merge are handled with the project's maintainers.
