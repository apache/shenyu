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
The migration policy is tracked by [#7602](https://github.com/apache/shenyu/issues/7602).
This guide follows up on [#6373](https://github.com/apache/shenyu/issues/6373) and
[#8419](https://github.com/apache/shenyu/issues/8419).

## Choose the repository

The migration targets Java 17 / Spring Boot 3, with Java 21 verification following
main's compatibility policy. First identify the implementation that your change
or application actually uses. The historical external default branch and the
migration candidate have different requirements.

The following source state was checked on 2026-11-12:

| Implementation | Source and baseline | Contribution status |
| --- | --- | --- |
| Historical external `main` | [POM at `8b93088973bf`](https://github.com/apache/shenyu-client-java/blob/8b93088973bf9f8f50d614e708bb87f662b707d5/pom.xml): Java 8, Boot 2.7.18, `2.7.0.1-jdk8-SNAPSHOT` | Still the external default branch at this check; use only for explicitly scoped historical maintenance |
| Main repository's in-tree clients | [Main POM](../pom.xml): Java 17 / Boot 3; `2.7.3-SNAPSHOT` at `149dba492033ae215331e09096f5129a2363d6fb` | Clients, starters and client-facing examples remain in main pending the release gates |
| External migration candidate | [POM at `a37668298ede`](https://github.com/apache/shenyu-client-java/blob/a37668298edef5c501e8b56a0595ed50ef895656/pom.xml): Java 17, Boot 3.3.1, `2.7.2-migration-SNAPSHOT` | [PR #16](https://github.com/apache/shenyu-client-java/pull/16) is draft and unmerged; this preparation candidate is not a published client release |

Recheck the issue, PR and current POM before starting work. A default-branch checkout
does not contain an unmerged candidate. The [compatibility matrix](java-client-compatibility.md)
preserves dated evidence for specific commits; its historical rows do not set the
baseline for new migration contributions or certify newer commits.

| Change | Repository and scope |
| --- | --- |
| Migrating business-client annotations, registration, auto-configuration, starters or client-dependent examples | `apache/shenyu-client-java`, on the migration work identified by #7602 and the relevant PR |
| Fix to client code still staged in the main reactor | `apache/shenyu`; identify whether the external candidate also needs the fix |
| Explicit maintenance of the historical Java 8 implementation | `apache/shenyu-client-java`, on the historical branch selected by its issue |
| Admin, gateway, registration protocol or shared foundations | `apache/shenyu` |
| Registration wire changes affecting both producer and consumer | Coordinated changes in both repositories |
| Published website documentation | `apache/shenyu-website` |

In-tree clients remain part of main's build scope during preparation. They are not
a second long-term implementation to maintain after migration. A change in one
implementation does not automatically update another; describe the affected
artifacts and coordinate any required counterpart change.

## JDK and framework baselines

| Build | Baseline | Verification guidance |
| --- | --- | --- |
| Main repository and in-tree clients | Java 17 / Spring Boot 3 | JDK 17 checks and main's Java 21 compatibility lane |
| External migration candidate | Java 17 / Spring Boot 3 | Build migrated clients, starters and examples on JDK 17; verify JDK 21 under the migration policy |
| Historical external Java 8 implementation only | Java 8 / Spring Boot 2.7.18 | Its pinned CI matrix tests on JDK 8, 17, 21, 23 and 25; this is not the migration matrix |

Read the selected POM and workflow: [main CI](../.github/workflows/ci.yml),
[candidate CI](https://github.com/apache/shenyu-client-java/blob/a37668298edef5c501e8b56a0595ed50ef895656/.github/workflows/ci.yml),
or [historical CI](https://github.com/apache/shenyu-client-java/blob/8b93088973bf9f8f50d614e708bb87f662b707d5/.github/workflows/ci.yml).
Their triggers can differ: the pinned candidate runs its Java 21 jobs on pushes,
while main's workflow also selects some PRs for full compatibility checks.
Do not override `java.version` to change the selected implementation's baseline.

Record local source results, hosted CI results and released client/Admin/bootstrap
verification separately. Configured or skipped jobs are not passing results; link
actual runs for the tested commit. A successful source build does not establish a
supported release pair, and matching Maven versions do not establish compatibility.
Use the [existing matrix](java-client-compatibility.md#version-compatibility-matrix)
and [release checklist](../shenyu-dist/README.md#release-compatibility-checklist)
for support claims. Keeping legacy headerless registration compatible does not
require the new client implementation to run on Java 8.

## Set up a contribution

Fork the repository you will change. For the main repository:

```sh
git clone https://github.com/YOUR_USERNAME/shenyu.git
cd shenyu
git remote add upstream https://github.com/apache/shenyu.git
git fetch upstream
git switch -c docs/split-client-contribution upstream/master
```

For an external client contribution, use your fork of `shenyu-client-java` and set
upstream to `https://github.com/apache/shenyu-client-java.git`. Choose the starting
point according to the issue:

- Historical maintenance can branch from the applicable historical `upstream/main`
  commit after checking its POM.
- For migration work, inspect #7602 and the relevant candidate PR first. At the
  check above, #16 uses `yangjj-iso/shenyu-client-java` branch
  `fix/7602-java17-migration-preparation`, head `a37668298ede`. Fetch the PR's head
  and compare the full commit with its POM and preparation guide; branching from
  the historical `upstream/main` does not select that implementation. Agree the
  contribution target with the existing migration work before duplicating it.

Keep the fork's default branch for synchronization and work on a separate branch.
Both repositories provide `./mvnw`. Set `JAVA_HOME` to the selected JDK, put
`$JAVA_HOME/bin` first in `PATH`, and confirm with `./mvnw -version`.
Use `mvnw.cmd` on Windows; a locally installed Maven is not required.

## Build and test the split client

### Java 17 migration candidate

The commands below apply to candidate #16 at the pinned commit, from its checkout
root. Follow its [preparation guide](https://github.com/apache/shenyu-client-java/blob/a37668298edef5c501e8b56a0595ed50ef895656/docs/MIGRATION.md)
when the candidate changes. First select JDK 17 and prepare its main-owned dependencies:

```sh
./mvnw -version
python3 script/prepare-foundations.py
./mvnw -B -ntp clean verify -Pexamples -Dmaven.javadoc.skip=true -Djacoco.skip=true -Ddocker.skip=true
```

The preparation script builds only the main-owned foundations listed in the
candidate's manifest, from its pinned main commit using main's wrapper. It installs
them locally and skips their tests; this step is not main test verification or
publication. The temporary client and foundation versions are separate. Release
POMs must use real published foundation versions before deployment is enabled.

After preparing foundations, a focused Spring MVC starter check is:

```sh
./mvnw -B -pl shenyu-spring-boot-starter-client/shenyu-spring-boot-starter-client-springmvc -am test
```

The focused check does not replace the complete candidate build. To repeat the
client verification on Java 17 and 21, set `JDK17_HOME` and `JDK21_HOME` to their
installed JDK directories and run from the candidate root:

```sh
for client_jdk in "$JDK17_HOME" "$JDK21_HOME"; do
  test -x "$client_jdk/bin/java" || { echo "Missing verification JDK: $client_jdk"; exit 1; }
  JAVA_HOME="$client_jdk" PATH="$client_jdk/bin:$PATH" ./mvnw -version || exit 1
  JAVA_HOME="$client_jdk" PATH="$client_jdk/bin:$PATH" ./mvnw -B -ntp clean verify \
    -Pexamples -Dmaven.javadoc.skip=true -Djacoco.skip=true -Ddocker.skip=true || exit 1
done
```

Capture each result separately. Report unavailable JDKs and unverified CI jobs;
`-DskipTests` builds do not verify behavior. The candidate's guide also describes
its client-owned source-runtime runner. Those source checks remain separate from
verification against released Admin/bootstrap images.

### Historical Java 8 maintenance only

For an issue explicitly targeting the historical implementation, retain its Java 8
source/bytecode and Boot 2 baseline. From that checkout root, use its wrapper:

```sh
./mvnw -version
./mvnw -B clean test
./mvnw -B clean install -Dmaven.javadoc.skip=true
```

Repeat the tests under the JDKs specified by that historical commit's CI without
changing `java.version`. Its [migration record](https://github.com/apache/shenyu-client-java/blob/8b93088973bf9f8f50d614e708bb87f662b707d5/progress.md)
and historical compatibility evidence apply only to that implementation. Do not
carry its copied infrastructure or Java 8 requirement into the Java 17 migration.

## Test local client artifacts with admin and the gateway

For behavior changes, select the required registration and routing scenarios from
the [cross-repository test strategy](cross-repo-compatibility-testing.md). For the
migration candidate, use its moved examples/source-runtime runner or a separate
Java 17 / Boot 3 test application. The following manual workflow uses a separate
application; an explicitly selected historical client instead requires compatible
Boot 2 application dependencies.

1. Prepare the client dependencies as above. In the selected client checkout, run
   `./mvnw -B clean install -Dmaven.javadoc.skip=true`. Record the commit and root
   POM version. Unlike `verify`, `install` makes the client artifacts available to
   a separate local application; it does not change main's reactor dependencies.
2. Select the client's starter and its exact installed version in the application.
   For Spring MVC, use `org.apache.shenyu:shenyu-spring-boot-starter-client-springmvc`.
   Keep client artifacts from the same client release together; do not force shared
   foundations to have the same version as the independent client.
3. Inspect the application's resolved dependencies:

   ```sh
   ./mvnw dependency:tree '-Dincludes=org.apache.shenyu:*'
   ```

   Confirm that the intended client and foundations were selected. Main's
   [examples](../shenyu-examples/README.md) use its own in-tree client version;
   building them does not verify the external candidate. Do not inject an external
   client into main's examples or assume Boot 2/Boot 3 starter compatibility.
4. For a source-to-source check, build a separate main checkout with JDK 17:

   ```sh
   ./mvnw -B clean install -Dmaven.javadoc.skip=true
   ```

   Start Admin and bootstrap using the [local deployment guide](https://shenyu.apache.org/docs/deployment/deployment-local/).
   Record the main commit, profiles, registration and sync channels. Configure matching
   sync settings and the required gateway plugin. For released-pair verification,
   select the released images and versions required by the compatibility matrix instead;
   a local main build cannot certify that release pair.
5. Configure `shenyu.register.registerType`, `shenyu.register.serverLists` and the
   channel-specific properties. For HTTP registration, `serverLists` points to Admin,
   not the gateway. Configure `shenyu.register.props.username` and
   `shenyu.register.props.password` for the test Admin without committing credentials.
6. Start the application. Verify metadata and URI registration, the resulting
   selector/rule/upstream in Admin, and a proxied request against the direct backend
   response. Exercise offline, heartbeat or discovery changes when affected. Verify
   API-doc, MCP and RPC behavior separately when claimed; a basic HTTP route does
   not cover them.

Save the dependency tree and client/Admin/gateway observations. Unit tests alone
cannot establish deployment compatibility. Documentation corrections do not require
new release certification; report the checks actually relevant to the change.

## Coordinate changes across repositories

The migration's dependency direction is
`shenyu-client-java -> released apache/shenyu protocol/foundation artifacts`.
Main owns the [registration protocol](../shenyu-register-center/shenyu-register-protocol/README.md),
including wire DTOs and fixtures, and shared common/register/registry foundations.
The client consumes these artifacts; do not recreate historical client-local DTO,
common, timer or registry copies. Java package names do not determine ownership.
Compare wire fields, types, defaults, authentication and responses when changing
the contract, and retain legacy v1 receiver compatibility.

Follow the [ownership inventory](java-client-repository-boundary.md#inventory-and-migration-baseline):
`shenyu-register-client-beat` and `shenyu-spring-boot-starter-client-beat` stay in main
because they serve gateway heartbeat behavior. `shenyu-sdk` and SDK starters are
outside this split, even when a client-dependent example moves. Main-owned code,
tests, bootstrap and retained examples must not depend on independent client artifacts.

A client-only implementation fix preserving the contract generally needs a client
PR; an Admin-only fix preserving accepted requests generally needs a main PR.
Changes to required fields, endpoints, responses or registration lifecycle need an
explicit compatibility decision and coordinated PRs when both sides must change:

- Cross-link the issue and PRs, and explain deployment order.
- Preserve old-client/new-Admin behavior where required; test new-client/old-Admin
  behavior or document the unsupported combination.
- Add producer serialization/registration tests and main-owned consumer fixtures,
  validation and persistence tests without importing client classes into main.
- Run the relevant registration and gateway checks with the intended commits.
- Update compatibility evidence when supported combinations change. Breaking wire
  changes require protocol versioning and transition support, not synchronized releases.

### Release gates

Follow the [migration release order](../shenyu-register-center/CLIENT_MIGRATION.md#release-order-and-gates)
and [release checklist](../shenyu-dist/README.md#release-compatibility-checklist):
release main-owned protocol/foundations first, publish the external client only
after ownership handoff and compatibility verification, and remove staged main
sources last. Each artifact coordinate must have one release owner. Candidate
SNAPSHOTs, local source checks and closed documentation issues do not satisfy these
gates. Use the migration guide's rollback rules rather than adding a reverse
client dependency to main.

## PR checklists

For an external migration client PR:

- [ ] Link the issue and identify the candidate branch, commit and affected artifacts.
- [ ] Keep Java 17 / Boot 3 and main-owned protocol/foundation ownership.
- [ ] Preserve relevant client/starter tests and add regression tests for behavior changes.
- [ ] Verify Java 17 and Java 21, reporting local results and hosted CI outcomes separately.
- [ ] Prepare required foundations and run the applicable complete build and the
  repository's PR-template checks; report missing or failed checks explicitly.
- [ ] For registration changes, record the Admin/gateway pair and integration results;
  link coordinated main changes and distinguish source checks from released-pair evidence.

For historical maintenance, explicitly identify the historical branch, retain its
Java 8 / Boot 2 baseline and check its pinned JDK matrix instead of applying the
migration candidate's build instructions. Do not describe that work as completing
#7602's migration.

For a main-repository PR affecting clients:

- [ ] Identify affected in-tree and external candidate artifacts and the wire contract.
- [ ] Preserve main's ownership and one-way dependency boundary, including beat and SDK modules.
- [ ] Add tests for changed behavior and retained compatibility; for documentation,
  check factual claims, links and commands without claiming unrun runtime verification.
- [ ] Build with JDK 17, check the applicable Java 21 lane and complete the current
  PR template, including `./mvnw clean install -Dmaven.javadoc.skip=true`.
- [ ] Verify affected client/Admin scenarios and explain coordinated changes and rollout order.

For example, a documentation commit can use
`docs(client): correct split client migration guidance.` with PR title
`[ISSUE #8419] Correct split client migration guidance.`
Push the branch to your fork and open the PR against the target selected for the
issue. Describe the final change and checks actually run. Leave unfinished checks
unchecked, respond to review on the same branch, and inspect CI before requesting
merge. Labels, milestones, review and merge remain with the project's maintainers.
