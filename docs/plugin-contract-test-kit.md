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

# Reactive plugin contract test kit

The `shenyu-web` test-jar supplies test-scope fixtures for third-party `ShenyuPlugin`
implementations against the real LEGACY `ShenyuWebHandler`. It adds no mandatory plugin
interface, descriptor, production dependency, `cache()` operator or execution-path change.
Profile selection is explicit and works with existing descriptor-free plugins.

## Consume the packaged kit

Build the producer before running the independent consumer (Java 17):

```bash
./mvnw -pl shenyu-web -am install -DskipTests -Djacoco.skip=true
./mvnw -f shenyu-examples/shenyu-examples-plugin-contract/pom.xml test
```

`-DskipTests` compiles test sources and packages the filtered `tests` classifier;
`-Dmaven.test.skip=true` does not. The example is outside the main source reactor and resolves
installed artifacts rather than reactor test classes. The same artifacts may be installed in a
fixed staging repository with `-Dmaven.repo.local=/path/to/staging` on both commands.

The `plugin_contract` CI job runs these producer and consumer phases as separate Maven
invocations on Java 17 for code changes. It also checks the consumer's license headers and
fails if no consumer tests run. Its result is required by the stable `build` gate.

For another repository, pin the same core version on both dependencies and provide the test
libraries explicitly: Maven does not propagate a producer's test dependencies from a test-jar.
This dependency list uses Spring Boot 3.3.1 dependency management, as does the example; import
`org.springframework.boot:spring-boot-dependencies:3.3.1` as a BOM if your project does not
already manage compatible Spring, Reactor and JUnit versions. Use a JUnit 5 capable Surefire
version (the example inherits 3.2.5). Set `shenyu.version` to the producer artifact version
(`2.7.3-SNAPSHOT` for this development checkout).

```xml
<dependency>
    <groupId>org.apache.shenyu</groupId>
    <artifactId>shenyu-web</artifactId>
    <version>${shenyu.version}</version>
    <scope>test</scope>
</dependency>
<dependency>
    <groupId>org.apache.shenyu</groupId>
    <artifactId>shenyu-web</artifactId>
    <version>${shenyu.version}</version>
    <type>test-jar</type>
    <classifier>tests</classifier>
    <scope>test</scope>
</dependency>
<dependency>
    <groupId>org.springframework.boot</groupId>
    <artifactId>spring-boot-starter-test</artifactId>
    <scope>test</scope>
</dependency>
<dependency>
    <groupId>io.projectreactor</groupId>
    <artifactId>reactor-test</artifactId>
    <scope>test</scope>
</dependency>
```

The starter supplies JUnit Jupiter, Spring Test (including `MockServerWebExchange`) and its
supported assertion libraries; `reactor-test` supplies `StepVerifier`. The main web artifact
supplies the runtime, plugin API/base and common dependencies. A plugin normally declares its
own API/base dependency for production; keep the kit and web runtime test-scoped. See the
[complete consumer POM](../shenyu-examples/shenyu-examples-plugin-contract/pom.xml).

## Choose a role and observe the plugin's own behavior

Select the expected role from the plugin's behavior, independently of its numeric order.

| Profile | Expected continuation per root execution |
| --- | --- |
| `PASS_THROUGH` | Invoke and subscribe downstream exactly once |
| `TRANSFORM` | Invoke and subscribe downstream exactly once; test the actual transformation separately |
| `SHORT_CIRCUIT` | Never invoke downstream; test the owned response separately |
| `DOWNSTREAM_WRAPPER` | Invoke and subscribe downstream at least once; decorations/resubscriptions are allowed |
| `SELECTOR_RULE` | Opt-in continuation expectation for an `AbstractShenyuPlugin` configured to delegate |

Selector/rule matching does not require every plugin to delegate after a match. A terminal
`AbstractShenyuPlugin` can use the matching fixture with `SHORT_CIRCUIT` on the matched path
and separate assertions for misses. Profiles describe expectations, not new runtime contracts.

```java
final MyTransformPlugin myPlugin = new MyTransformPlugin();
final ContractProbe probe = ContractProbe.around(
        myPlugin, "my-transform", 100, PluginContractProfile.TRANSFORM);
final LegacyPluginRuntime runtime = new LegacyPluginRuntime(probe);
final ServerWebExchange exchange = LegacyPluginRuntime.newExchange();

PluginContractAssertions.assertRootSubscriptionBoundary(
        runtime, exchange, myPlugin::isEffectObserved, "my-transform [transform]");
PluginContractAssertions.assertContinuation(probe);
```

`ContractProbe` observes calls, subscriptions and terminal signals without deferring the
wrapped `execute()` invocation. `before → execute → after` remains publisher-construction
order inside the handler's root subscription. `after` is **not** asynchronous completion;
synchronous effects inside a direct `execute()` are allowed. The boundary check rejects
observable work before the root handler subscription, not direct-execute assembly work.

`assertProbeRepeatedSubscriptions` checks downstream invocation and subscription deltas for
each of two root subscriptions. Use plugin-owned counters to check repeated side effects as
well. `assertContinuation` checks aggregate counts after execution; it cannot by itself
identify which subscription produced an effect. Build fresh fixture/probe instances per test.

`assertProbeCancellation` checks cancellation at the publisher boundary only. To prove resource
ownership, use `assertCancellationCleanup(runtime, exchange, probe, plugin::isResourceReleased)`
with a synchronously acquired controlled source. It rejects a detached upstream whose cleanup
never runs. For asynchronous acquisition or cleanup, use a custom `StepVerifier` with controlled
signals before cancellation and an explicit timeout. The supplied helpers use a five-second
verification limit, no sleeps. Test completion/error cleanup, response content and decorator
propagation against the plugin's actual state, not only the probe's counters.

The [external consumer tests](../shenyu-examples/shenyu-examples-plugin-contract/src/test/java/org/apache/shenyu/examples/plugin/contract)
show transformations, response ownership, nested decorators, error/cancellation cleanup,
repeated subscriptions, bad continuation and lost-cleanup fixtures, and optional isolated fork.
A legacy continuation contains a mutable cursor: downstream resubscription can continue from
the consumed position. A wrapper profile permits this behavior without claiming that retry
replays an already-consumed plugin. Fork starts an independent chain at index zero, so an
internal dispatcher must guard against recursive dispatch itself.

## Selector/rule and configuration fixtures

```java
try (SelectorRuleFixture fixture = new SelectorRuleFixture()) {
    fixture.cachePluginData(pluginData); // claim a unique, previously unused plugin name
    fixture.cacheSelectorData(selectorData);
    fixture.cacheRuleData(ruleData);
    // Run the matching path, then update/remove data and verify behavior again.
    fixture.removeRuleData(ruleData);
}
```

The fixture installs a minimal `ShenyuConfig` context through `SpringBeanUtils`, invalidates
matching caches for updates/removals, and removes only its own definitions on close. Existing
plugin names and selector IDs are rejected to avoid overwriting caller data. Fixture lifetimes
are serialized, nested fixtures must close in reverse order on their creating thread, and the
previous context is restored. Other tests using ShenYu's global caches/context must also run
serially; the fixture cannot synchronize unrelated code that bypasses it.

This fixture exercises core matching data, not a plugin-specific handler. If a plugin declares
a `PluginDataHandler`, drive its real `handlerRule`/`removeRule` (or corresponding plugin/selector)
callbacks with controlled configuration and check plugin-owned state after update/removal.
The advanced consumer exercises a handler's update/removal on subsequent requests. There is no
invented provider/lease contract; resource checks apply only to resources the plugin owns.

## Diagnostics and scope

```java
final PluginContractMetadata metadata = new PluginContractMetadata(
        "my-plugin",
        EnumSet.of(PluginContractProfile.TRANSFORM),
        Set.of("fork", "real-gateway-e2e"));
System.out.println(metadata.describe());
```

The stable description includes kit version, plugin API implementation version, selected
profiles, and intentionally skipped capabilities. Unpackaged development classes report
`core=development`; packaged consumption reports the declared core artifact version. Include
this description in test reports and label plugin-specific assertions with plugin/profile and
expected state. A skipped capability is not a passing test.

Ordinary contract tests need no Docker, database or shared credentials. The runtime fixture
passes a `null` loader because `handle()` does not use it; it does not validate bootstrap,
classloader uploads, external SDK/protocol correctness, data-sync transport or graph parity.
The default exchange is HTTP with a `ShenyuContext`; supply your own for other protocols.
Plugin-store publication/build matrices remain owned by #7294, migration by #7306, and future
GRAPH/LEGACY parity by #7996/#8001.

## Opt-in real-gateway smoke

Reuse the existing `shenyu-integrated-test-upload-plugin` case: its custom plugin is packaged
as a JAR, uploaded into the running gateway, and checked by `AdminPluginUploadTest`. This is
a separate boundary from unit contracts. On a disposable Docker-enabled Linux environment,
follow the existing [integrated-test workflow](../.github/workflows/integrated-test.yml) build
steps, selecting `shenyu-integrated-test-upload-plugin`. The equivalent commands from the root
are:

```bash
./mvnw -B clean install -Prelease,docker -Dmaven.javadoc.skip=true -Dmaven.test.skip=true
./mvnw -B -f shenyu-integrated-test/pom.xml -pl :shenyu-integrated-test-upload-plugin -am clean install -Pit -Dmaven.test.skip=true
docker compose -f shenyu-integrated-test/shenyu-integrated-test-upload-plugin/docker-compose.yml up -d --wait
./mvnw -B -f shenyu-integrated-test/shenyu-integrated-test-upload-plugin/pom.xml test -Pit -DfailIfNoTests=true
docker compose -f shenyu-integrated-test/shenyu-integrated-test-upload-plugin/docker-compose.yml down
```

Run teardown even if the test fails. This smoke uses the existing upload fixture and test-only
Docker configuration; it does not replace third-party protocol E2E or plugin-store CI. It is
opt-in and is not run by the standalone consumer command.
