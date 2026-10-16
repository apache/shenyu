# Java client repository migration

Tracked by [#7602](https://github.com/apache/shenyu/issues/7602). The dependency
rule is `shenyu-client-java -> released apache/shenyu artifacts`. Admin, bootstrap,
main-owned tests and retained examples must never resolve the independent client
release. Java 17 / Spring Boot 3 is the shared baseline; use the main repository's
Java 21 compatibility lane. Java 8/Spring Boot 2 and restored Motan support are
outside this migration.

## Current stage

This change supplies the **main-repository prerequisites**: the v1 wire artifact
and fixtures, shared MCP generation, removal of Admin and heartbeat dependencies
on the business SDK, and a staged dependency guard. The client code and examples
remain in their original reactor pending the release gates below. This is not a
claim that the external repository has been migrated or independently released.

The examples previously selected an independent `2.7.0.3` client version through
`shenyu.client.java.version`. During this preparation stage they use the current
reactor version, so development does not depend on a separately published client
or accidentally validate the older JDK8-adapted implementation.

## Ownership and inventory

| Main-owned | Client-owned after migration |
| --- | --- |
| `shenyu-common`, `shenyu-disruptor`, `shenyu-spi` | `shenyu-client/**`, with all tests |
| `shenyu-register-protocol`, `shenyu-register-common`, `shenyu-register-mcp` | `shenyu-register-client-api`, `shenyu-register-client-http` |
| `shenyu-registry-api` and implementations, `shenyu-protocol-*` | All business-service client starters and their tests |
| Admin controllers, publishers, subscribers, services and fixture tests | Client registration/lifecycle/retry/shutdown/annotation tests |
| `shenyu-register-client-beat` and `shenyu-spring-boot-starter-client-beat` | Client-dependent examples and cross-repository E2E workflows |
| Gateway/plugin/server examples and SDK consumer implementations | Java 17 / Java 21 client verification |

The machine-readable inventory is
[`.github/client-migration-inventory.json`](../.github/client-migration-inventory.json).
It lists eight business-service starter roots and nineteen example roots, including
SDK gRPC and Spring Cloud providers whose dependencies on client examples are
transitive. Remaining example modules stay in main. Shared `shenyu-examples-common`
and Dubbo API helpers are main-owned; when assembling the external example reactor,
consume a released main-owned helper or give any extracted client-only helper a
new coordinate. Never publish the same coordinate from both repositories.

Client examples covered: HTTP, HTTPS, Swagger3 HTTP, Spring MVC, Tomcat MVC,
Spring Cloud, gRPC, MCP, three Dubbo service variants, three WebSocket variants,
SDK HTTP/Feign and SDK Dubbo/gRPC/Spring Cloud providers. Retained SDK consumers
may use released main-owned SDK/protocol/API artifacts, never client artifacts.

`python3 .github/scripts/check-client-boundary.py` checks every source/test and POM
(including inactive profiles and dependency management). Only the exact staged
source roots and examples are exempt. Beat and its starter are not exempt. The
examples parent exception covers dependency management only. Remove exemptions
as source is migrated; do not broaden them to hide reverse dependencies.

## Release order and gates

1. Review and approve the ownership inventory and v1 contract. Merge/release main's
   protocol, common, registry, MCP and required foundation artifacts first. Keep
   business-client artifact deployment disabled for the handoff release once their
   coordinate ownership transfers; main must not resume publishing those artifacts.
2. Rebaseline `apache/shenyu-client-java` from the current main client source,
   preserving tests and packages. Remove its copied JDK8 common/register/timer/DTO
   and registry implementations; depend on the released main-owned artifacts.
   Its release POM must contain real published versions, not local SNAPSHOTs.
3. Move the inventoried client starters and examples. Keep heartbeat and heartbeat
   starter in main; move their aggregating POM placement when removing the business
   starter reactor. Update Dockerfiles, integration/CI matrices and contributor
   paths that reference moved examples. External E2E belongs in the client repo.
4. Pass the compatibility matrix below, record exact main/client commits, JDKs,
   image versions/digests, test commands, observed routes and proxy results, then
   publish the external client artifacts under a single designated release owner.
5. Only then remove migrated source/module entries from main, remove the staged
   exemptions and example dependency management, and verify that no retained
   POM, test, workflow or bootstrap configuration resolves independent clients.

Do not mark #7602 complete after the prerequisite PR. Main foundation publication,
external migration/releases and cross-repository E2E are release/community gates;
a successful local fixture test does not satisfy them. This ordering supersedes
older suggestions in #6367 to import independent clients into the main reactor.

## Compatibility CI

Main PRs run raw v1 fixture/controller tests, registration service tests, real
Admin/ZooKeeper discovery synchronization, MCP import tests and heartbeat tests.
These run under the normal Java 17 build; the existing push-only Java 21 lane runs
the same tests. Main CI never imports an independent client artifact. The boundary
check and its regression tests run with the existing dependency-boundary CI job.
That job also replays the protocol/ZooKeeper/beat-starter tests and checks resolved
Admin and beat-starter dependency trees, including test-scope transitive dependencies.
This adds a small in-process fixture suite and uses the existing embedded
ZooKeeper integration test, without a cross-repository Docker job in main.

Client PRs must build all migrated modules/starter tests on Java 17 and validate
emitted requests against the published protocol fixtures (including absent optional
fields, namespaces, MCP nesting and string/integer port differences). Report
`X-Shenyu-Register-Protocol-Version: 1`; optional `X-Shenyu-Client-Version` should
identify the client release, not the version of a shared foundation JAR. Do not
require new Admin response headers from older supported releases.

Client nightly and pre-release jobs should start released Admin and bootstrap
images, plus an example built from the candidate client. Pin the latest stable
Admin and the oldest explicitly supported Admin release in a reviewed matrix;
replace placeholders with verified released image digests before enabling the job.
For each pair verify Spring MVC metadata/URI registration, heartbeat renewal,
discovery instance add/update/removal, route visibility, configuration sync and an
actual proxied HTTP request. Include MCP and the migrated RPC clients before
claiming support for them. Run the Java 21 lane with the same policy as main.
Budget these Docker suites separately from PR fixture tests and retain logs on
failure. No released Admin/client pair is certified by this prerequisite change.

Local main verification (from the repository root):

```bash
python3 .github/scripts/test-client-boundary.py
python3 .github/scripts/check-client-boundary.py
./mvnw -pl shenyu-admin,shenyu-register-center/shenyu-register-client-beat -am test \
  -Dtest=RegistrationProtocolCompatibilityTest,ShenyuHttpRegistryControllerTest,ZookeeperClientDiscoveryIntegrationTest,SwaggerImportServiceImplTest,RegisterUtilsTest,OkHttpToolsTest,HeartbeatListenerTest \
  -Dsurefire.failIfNoSpecifiedTests=false -Dmaven.javadoc.skip=true -Djacoco.skip=true
```

The protocol [README](shenyu-register-protocol/README.md) is the field/endpoint
contract for #6365. The client-side matrix and recorded verification above cover
follow-up work in #6366/#6367; the ownership and contribution rules cover #6373.

## Rollback and contribution rules

Before external publication, revert a prerequisite implementation in main and
keep the existing in-tree client release owner. After publication, roll back
business services to a previously tested client release and keep Admin accepting
v1; do not add an independent client dependency to Admin/bootstrap. If source
removal must be reverted, restore the archived source under disabled deployment
until coordinate ownership is explicitly reassigned. Never solve rollback by
publishing the same coordinate/version from both repositories.

For main PRs changing DTOs/endpoints/enums/defaults: update the contract, add fixtures
without rewriting historic ones, replay older fixtures, and check dependency and
heartbeat boundaries. For client PRs: retain lifecycle/retry/shutdown tests, consume
released foundations, test supported released Admin versions, and move affected
examples/workflows together. A breaking wire change requires coordinated PRs and
dual-version Admin support, not synchronized releases of every business service.
