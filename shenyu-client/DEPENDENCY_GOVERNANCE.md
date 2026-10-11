# Dependency Governance Rules for shenyu-client-java

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

## 1. Overview

This document defines the dependency governance rules for the `shenyu-client` module family. These modules are published independently as **[shenyu-client-java](https://github.com/apache/shenyu-client-java)** artifacts on Maven Central. The main ShenYu repository builds with JDK 17, but the published client artifacts **must preserve JDK 8 runtime compatibility**.

### 1.1 Scope

This governs `shenyu-client/` and all its submodules (core, http, dubbo, sofa, tars, grpc, motan, websocket, mcp, api-docs-annotations, autoconfig).

### 1.2 Key Principle

> **A JDK 8 user adding a published `shenyu-client-*` artifact to their classpath must be able to compile and run without errors.**

All runtime (compile/runtime scope) dependencies must be JDK 8–compatible. Test dependencies are not shipped and may use newer JDKs.

### 1.3 Relationship to the Independent shenyu-client-java Repository

The independent `apache/shenyu-client-java` repo applies these same rules with a JDK 8 compile target. The versions documented below are the current ones in that repository, which is the authoritative place for JDK 8 enforcement.

---

## 2. Runtime Dependency Inventory

### 2.1 Direct Runtime Dependencies (compile scope)

Each dependency was verified for JDK 8 compatibility by checking (a) class file major version in the published JAR, (b) official project documentation / release notes, and (c) Maven Central POM metadata.

| GroupId | ArtifactId | Typical Version | Class Ver | JDK 8? | Verified How | Notes |
|---------|-----------|----------------|-----------|--------|-------------|-------|
| `com.squareup.okhttp3` | `okhttp` | 4.9.3 | 52 | ✅ | JAR class version + OkHttp changelog | **🔒 CAPPED ≤ 4.x.** OkHttp 5.x requires JDK 11. [Source](https://square.github.io/okhttp/changelogs/changelog_5x/) |
| `com.google.code.gson` | `gson` | 2.9.0 | 51 | ✅ | JAR class version (51 = Java 7) | Gson 2.x supports JDK 7+. No Gson 3.x exists yet. |
| `org.javatuples` | `javatuples` | 1.2 | 49 | ✅ | JAR class version (49 = Java 5) | Very stable; final release. |
| `javax.xml.bind` | `jaxb-api` | 2.3.1 | 52 | ✅ | JAR class version | Must stay on `javax.xml.bind`. Switching to `jakarta.xml.bind:jakarta.xml.bind-api` 3.x+ requires JDK 11. |
| `com.github.ben-manes.caffeine` | `caffeine` | BOM (2.9.x) | 52 | ✅ | Spring Boot BOM 2.7.x manages this | **🔒 CAPPED ≤ 2.9.x.** Caffeine 3.x requires JDK 11 ([release notes](https://github.com/ben-manes/caffeine/releases/tag/v3.0.0)). OK while BOM is 2.7.x. |
| `com.fasterxml.jackson.core` | `jackson-annotations` | BOM (2.14.x) | 52 | ✅ | Spring Boot BOM 2.7.x manages this | Jackson 2.x supports JDK 8. |
| `org.bouncycastle` | `bcprov-jdk18on` | 1.78 | 52 (base) | ✅ | JAR class version + POM description | "jdk18on" = "JDK 1.8 and on". Multi-Release JAR: base classes = v52, JDK 11-specific classes in `META-INF/versions/11/`. |
| `org.slf4j` | `slf4j-api` | 1.7.36 | 49 | ✅ | JAR class version (49 = Java 5) | SLF4J 2.x also supports JDK 8 ([SLF4J news](https://www.slf4j.org/news.html)). |
| `org.slf4j` | `jcl-over-slf4j` | 1.7.36 | — | ✅ | Same project as slf4j-api | |
| `com.google.guava` | `guava` | 32.0.0-jre | 52 | ✅ | JAR class version + [Guava releases](https://github.com/google/guava/releases) | ⚠️ Guava 33.0.0 still supports JDK 8 runtime ([issue #6549](https://github.com/google/guava/issues/6549) about dropping JDK 8 is still open). Future versions may drop JDK 8 — monitor Guava releases. |
| `org.apache.commons` | `commons-lang3` | BOM | — | ✅ | Apache Commons project baseline | JDK 8+. |
| `org.apache.commons` | `commons-collections4` | 4.4 | 52 | ✅ | JAR class version | JDK 8+. |

### 2.2 Protocol-Specific Runtime Dependencies

| GroupId | ArtifactId | Typical Version | Class Ver | JDK 8? | Verified How | Notes |
|---------|-----------|----------------|-----------|--------|-------------|-------|
| `com.google.protobuf` | `protobuf-java` | 3.25.5 | 52 | ✅ | JAR class version | Protobuf 3.x supports JDK 8. |
| `com.google.protobuf` | `protobuf-java-util` | 3.21.12 | — | ✅ | Companion to protobuf-java | Same JDK baseline as protobuf-java. |
| `io.swagger.core.v3` | `swagger-annotations` | 2.2.21 | — | ✅ | [Swagger Core README](https://github.com/swagger-api/swagger-core) | Swagger Core 2.2.x runtime = JDK 8. No 3.x release exists. Jakarta namespace available as `-jakarta` suffix artifacts within 2.2.x. |

### 2.3 Provided-Scope Dependencies (User Supplies at Runtime)

Not transitively pulled. Listed for completeness.

| GroupId | ArtifactId | Typical Version | JDK 8? | Module |
|---------|-----------|----------------|--------|--------|
| `org.apache.dubbo` | `dubbo` | 3.2.14 | ✅ | dubbo |
| `com.alipay.sofa` | `sofa-rpc-all` | 5.7.6 | ✅ | sofa |
| `com.alipay.sofa` | `runtime-sofa-boot-starter` | 3.1.4 | ✅ | sofa |
| `com.tencent.tars` | `tars-spring-boot-starter` | 1.7.2 | ✅ | tars |
| `com.weibo` | `motan-springsupport` | 1.2.1 | ✅ | motan |
| `javax.websocket` | `javax.websocket-api` | BOM | ✅ | websocket |
| `javax.servlet` | `javax.servlet-api` | BOM | ✅ | websocket |
| `javax.validation` | `validation-api` | BOM | ✅ | dubbo |

### 2.4 Spring Framework / Spring Boot (managed by BOM)

| GroupId | ArtifactId | Implied Version | JDK 8? | Verified How | Notes |
|---------|-----------|----------------|--------|-------------|-------|
| `org.springframework` | `spring-core` | 5.3.x | ✅ | [Spring 6.0 release notes](https://github.com/spring-projects/spring-framework/wiki/Spring-Framework-6.0-Release-Notes) | **🔒 CAPPED ≤ 5.3.x.** Spring 6.x = JDK 17. |
| `org.springframework.boot` | `spring-boot-dependencies` | 2.7.18 | ✅ | Spring Boot release notes | **🔒 CAPPED ≤ 2.7.x.** Spring Boot 3.x = JDK 17. |

---

## 3. Test & Tooling Dependency Inventory

Test dependencies are NOT shipped in published artifacts.

### 3.1 Test Dependencies (test scope)

| GroupId | ArtifactId | Typical Version | JDK 8? | Verified How | Notes |
|---------|-----------|----------------|--------|-------------|-------|
| `org.junit.jupiter` | `junit-jupiter` | BOM (5.8.x) | ✅ | JUnit 5 documentation | JUnit 5.8.x from Spring Boot 2.7.x BOM. |
| `org.junit.vintage` | `junit-vintage-engine` | BOM | ✅ | JUnit Vintage docs | |
| `org.hamcrest` | `hamcrest-library` | BOM | ✅ | Hamcrest docs | |
| `org.mockito` | `mockito-core` | 3.5.15 | ✅ | [Mockito README](https://github.com/mockito/mockito) | **🔒 CAPPED ≤ 4.11.0.** Mockito 5.x = JDK 11. Can upgrade within 3.x or 4.x. |
| `org.mockito` | `mockito-junit-jupiter` | 3.5.15 | ✅ | Same as mockito-core | |
| `org.mockito` | `mockito-inline` | 3.5.15 | ✅ | Same as mockito-core | Required for mocking final classes on JDK 8. |
| `org.awaitility` | `awaitility` | 4.0.3 | ✅ | [Awaitility changelog](https://raw.githubusercontent.com/awaitility/awaitility/master/changelog.txt) | Awaitility 4.x = JDK 8+. |
| `org.springframework.boot` | `spring-boot-test` | BOM | ✅ | Spring Boot 2.7.x | |
| `org.springframework.boot` | `spring-boot-starter-tomcat` | BOM | ✅ | Embedded Tomcat from Spring Boot 2.7.x BOM | |
| `net.bytebuddy` | `byte-buddy` | 1.18.10 | ✅ | [Byte Buddy docs](http://bytebuddy.net/) | Byte Buddy 1.18.x min JVM = 8. Managed as override to support newer JDKs in CI. See §7.2. |
| `org.hibernate` | `hibernate-validator` | 5.2.5.Final | ✅ | Hibernate Validator docs | 5.x for JDK 8. 7+ requires Jakarta namespace. |
| `com.github.tomakehurst` | `wiremock` | 2.18.0 | ✅ | [WireMock 3.0 release](https://github.com/wiremock/wiremock/releases/tag/3.0.0) | **🔒 CAPPED ≤ 2.x for client tests.** WireMock 3.x = JDK 11. |

### 3.2 Build Plugins

| GroupId | ArtifactId | Typical Version | JDK 8? | Notes |
|---------|-----------|----------------|--------|-------|
| `org.apache.maven.plugins` | `maven-compiler-plugin` | 3.7.0+ | ✅ | |
| `org.apache.maven.plugins` | `maven-surefire-plugin` | 3.0.0-M4+ | ✅ | |
| `org.apache.maven.plugins` | `maven-source-plugin` | 3.0.1+ | ✅ | |
| `org.apache.maven.plugins` | `maven-javadoc-plugin` | 3.6.0+ | ✅ | |
| `org.apache.maven.plugins` | `maven-release-plugin` | 2.5.3+ | ✅ | |
| `org.apache.maven.plugins` | `maven-checkstyle-plugin` | 3.4.0+ | ✅ | |
| `org.jacoco` | `jacoco-maven-plugin` | 0.8.x | ⚠️ | **Known exception.** 0.8.9+ requires JDK 11+ to run. 0.8.8 is the last version executable on JDK 8. See §7.1. |
| `org.apache.rat` | `apache-rat-plugin` | 0.15+ | ✅ | License header checking. |

---

## 4. Risky Dependencies — JDK 8 Incompatibility Watchlist

### 4.1 🔴 Confirmed: Already Dropped JDK 8 in Newer Major Versions

Each claim verified against the project's official release notes.

| Dependency | Current Safe Version | Version That Drops JDK 8 | Drops To | Evidence |
|------------|---------------------|--------------------------|----------|----------|
| **OkHttp** | 4.x (latest: 4.12.0) | 5.0 | JDK 11 | [OkHttp 5.x changelog](https://square.github.io/okhttp/changelogs/changelog_5x/) |
| **Mockito** | 4.x (latest: 4.11.0) | 5.0 | JDK 11 | [Mockito README](https://github.com/mockito/mockito): "Mockito 5 now requires Java 11" |
| **Caffeine** | 2.x (latest: 2.9.3) | 3.0 | JDK 11 | [Caffeine 3.0 release](https://github.com/ben-manes/caffeine/releases/tag/v3.0.0) |
| **Spring Framework** | 5.3.x | 6.0 | JDK 17 | [Spring 6.0 release notes](https://github.com/spring-projects/spring-framework/wiki/Spring-Framework-6.0-Release-Notes) |
| **WireMock** | 2.x (latest: 2.35.0) | 3.0 | JDK 11 | [WireMock 3.0 release](https://github.com/wiremock/wiremock/releases/tag/3.0.0) |
| **JAXB API (namespace)** | `javax.xml.bind` 2.3.x | `jakarta.xml.bind` 3.x | JDK 11 | Jakarta EE migration |

### 4.2 🟡 Monitor: Potential Future Risk

No official release has dropped JDK 8 yet, but project discussions or roadmaps indicate it may happen.

| Dependency | Current Version | Status |
|------------|----------------|--------|
| **Guava** | 32.0.0-jre (33.0.0 still JDK 8) | [Issue #6549](https://github.com/google/guava/issues/6549) proposes requiring JDK 11+ to BUILD but retaining JDK 8 runtime. Still OPEN. No version has dropped JDK 8 yet. |
| **Gson** | 2.11.0 (latest) | No Gson 3.x release exists. Gson 2.x = JDK 7+. Monitor for 3.0 announcements. |
| **Swagger Core** | 2.2.x (latest: 2.2.50) | No 3.x release exists. Jakarta namespace is `-jakarta` suffix artifacts within 2.2.x. Risk is using wrong artifact name, not version. |
| **JaCoCo** | 0.8.x (latest: 0.8.12) | No 0.9.x release exists. Real risk: 0.8.9+ requires JDK 11+ runtime. |
| **SLF4J** | 2.0.x (JDK 8) | 2.x is fine for JDK 8. Monitor for 3.x announcements. |
| **protobuf-java** | 3.x (JDK 8) | Monitor for 4.x JDK requirements. |

### 4.3 🟢 Stable: Very Low Risk

| Dependency | Reason |
|------------|--------|
| `javatuples` 1.2 | Final release; no further development. |
| `commons-collections4` 4.4 | Stable Apache Commons; JDK 8 bytecode. |
| `commons-lang3` | Stable; managed by BOM. |
| `hamcrest-library` | Stable; managed by BOM. |
| `awaitility` 4.x | JDK 8 minimum; well-established. |
| `slf4j-api` / `jcl-over-slf4j` 1.7.x | JDK 5 bytecode. |
| `bcprov-jdk18on` | JDK 1.8 and on. Multi-release JAR. |
| `sofa-rpc-all`, `tars-spring-boot-starter`, `motan-springsupport` | `provided` scope; user controls version. |

---

## 5. Runtime Dependency Upgrade Rules

### 5.1 Golden Rule

> **A runtime dependency upgrade that raises the class file major version above 52 (Java 8) is rejected.**

### 5.2 Rules

1. **Version ceiling enforcement** — Dependencies in §4.1 🔴 have hard version ceilings. No PR may upgrade past them.

2. **Within-major-version upgrades only** for critical dependencies (e.g., Guava 32.0.0 → 32.1.3 is OK; 32.x → 33.x requires explicit JDK 8 verification).

3. **Transitive dependency audit** — Every runtime dependency upgrade PR must include `./mvnw dependency:tree` output showing no new transitive dependency with class version > 52.

4. **class file major version check** — After build, verify compiled classes have major version ≤ 52:
   ```bash
   javap -verbose <path-to-class> | grep 'major version'
   ```

5. **API compatibility** — Upgraded dependency must not introduce API calls requiring JDK 9+ (e.g., `List.of()`, `Optional.stream()`, `ProcessHandle`).

6. **Documentation** — PR description must list all dependency changes with old → new versions and JDK 8 compatibility verification.

---

## 6. Test & Tooling Dependency Upgrade Rules

### 6.1 Rules

1. **Test dependencies are not shipped** — They don't affect end users. Can be more flexible than runtime deps.

2. **Build plugins must execute on JDK 8** — The `shenyu-client-java` CI runs on JDK 8. Plugins must work on JDK 8 as execution platform. Exception: JaCoCo (§7.1).

3. **Mockito ceiling** — Stay ≤ 4.11.0 for the shenyu-client-java repo. Mockito 5.x = JDK 11.

4. **WireMock ceiling for client tests** — Stay ≤ 2.35.0. WireMock 3.x = JDK 11.

5. **Byte Buddy overrides** — May be upgraded independently of Mockito (see §7.2).

---

## 7. Known Exceptions

### 7.1 JaCoCo Maven Plugin

**Why an exception:** JaCoCo instruments bytecode at build time. Each version supports a specific range of JDK class file formats. **Versions 0.8.9+ require JDK 11+ as the execution platform**, while earlier 0.8.x versions run on JDK 8. At the same time, older versions may not instrument newer JDK class files.

**Current situation:**
- JaCoCo 0.8.8 — last version that runs on JDK 8
- JaCoCo 0.8.12 — latest 0.8.x; requires JDK 11+ to run
- JaCoCo 0.8.15 — used in shenyu-client-java for JDK 25 class file support

**Rule:**
- For the shenyu-client-java CI: prefer 0.8.15 with JDK 11+ runner, or 0.8.8 for JDK 8 runner
- The CI matrix runs on multiple JDKs — JaCoCo can execute on a newer JDK job even if other jobs use JDK 8
- Upgrade within 0.8.x to support newer JDK class formats; verify coverage report still generates

**Verification:** `progress.md` confirms 0.8.15 was chosen to recognize JDK 25 class files while JDK 8 tests still pass.

### 7.2 Byte Buddy (Mockito Transitive Dependency)

**Why an exception:** Mockito 3.5.15 bundles an older Byte Buddy that can't handle JDK 21+ bytecode. The repo manages Byte Buddy as a top-level `<dependencyManagement>` override.

**Current versions:**
- shenyu-client-java: Byte Buddy 1.18.10
- Main shenyu repo: Byte Buddy 1.14.11

**Verification:** Byte Buddy 1.18.x minimum JVM = 8. Both 1.14.x and 1.18.x support JDK 8.

**Rule:**
- Byte Buddy may be upgraded independently to support newer JDK class file formats
- Must be explicitly declared in `<dependencyManagement>`
- Must pass tests on both JDK 8 and newest JDK in CI matrix

### 7.3 SOFA Boot Starter (provided scope)

**Current:** runtime-sofa-boot-starter 3.1.4 in shenyu-client-java (JDK 8 compatible). Main repo uses 4.3.0 (may require JDK 11+).

**Rule:**
- `provided` scope — the user brings their own version at runtime
- Compile-time version must be JDK 8–compatible
- Do not introduce `jakarta.*` imports in SOFA client code

### 7.4 WireMock Version Divergence

**Main repo:** `wiremock.version` = 3.0.1 (JDK 11+, used by main repo tests — OK since main repo builds on JDK 17)
**shenyu-client-java:** `wiremock.version` = 2.18.0 (JDK 8 compatible, used if client modules ever need WireMock tests)

**Rule:** shenyu-client-java must keep WireMock ≤ 2.x. The main repo may use 3.x for its own tests.

---

## 8. PR Verification Requirements

### 8.1 Required Checks Before Merging a Client Dependency Upgrade PR

#### Local Verification (Contributor)

| # | Check | Command | Pass Criteria |
|---|-------|---------|---------------|
| 1 | **class file version** | `javap -verbose <path> \| grep 'major version'` | All runtime classes ≤ 52 |
| 2 | **dependency tree** | `./mvnw -pl <module> dependency:tree` | No transitive dep past JDK 8 ceiling |
| 3 | **Cross-reference with this doc** | Read §4.1 | No 🔴 ceiling exceeded |

#### CI Verification (for shenyu-client-java repo)

| # | Check | Pass Criteria |
|---|-------|---------------|
| 4 | CI matrix (JDK 8, 17, 21, 23, 25) | All green |
| 5 | License header check | Pass |
| 6 | CodeQL analysis | No new alerts |

### 8.2 PR Description Template (for Dependency Upgrade PRs)

```markdown
## Dependency Changes

| Artifact | Old Version | New Version | JDK 8 Compatible? | Evidence |
|----------|-------------|-------------|-------------------|----------|
| group:artifact | x.y.z | a.b.c | ✅ / ❌ | class version / release notes link |

## Verification

- [ ] No 🔴 ceiling exceeded (DEPENDENCY_GOVERNANCE.md §4.1)
- [ ] class file major version ≤ 52 for all runtime classes
- [ ] `./mvnw dependency:tree` — no new JDK 11+ transitive deps

## Rationale
```

### 8.3 Automatic Rejection Criteria

1. Upgrades a 🔴 Critical dependency past its documented ceiling (§4.1)
2. `./mvnw dependency:tree` reveals a new transitive dependency requiring JDK 11+
3. PR description does not include the dependency changes table
4. Any runtime class file major version > 52

---

## 9. Dependency Upgrade Checklist

Copy into the PR description:

```markdown
## Dependency Upgrade Checklist

### Pre-Upgrade
- [ ] Identified dependency's minimum JDK from official docs or POM metadata
- [ ] Checked against ceilings in DEPENDENCY_GOVERNANCE.md §4.1
- [ ] Classified as runtime or test/build-only

### Runtime Dependency (if applicable)
- [ ] Verified the new version compiles to class file major version ≤ 52
- [ ] No `jakarta.*` namespace migration in the new version
- [ ] No new API usage requiring JDK 9+ APIs (List.of, Optional.stream, etc.)
- [ ] `./mvnw dependency:tree` shows no new JDK 11+ transitive deps

### Test / Build Dependency (if applicable)
- [ ] Plugin/library runs on JDK 8 as execution platform (or has exception per §7)
- [ ] Byte Buddy override compatible (if applicable)

### Post-Upgrade
- [ ] `javap -verbose` confirms class file major version ≤ 52
- [ ] All existing tests pass
```

---

## 10. Policy Review Cadence

| Trigger | Action |
|---------|--------|
| **Quarterly** | Review watchlist (§4) for new releases that dropped JDK 8 |
| **New JDK LTS release** | Add to CI matrix; verify tooling compatibility |
| **Each shenyu-client-java release** | Re-verify all version ceilings |
| **Guava #6549 resolution** | Update §4.2 — may become 🔴 if JDK 8 runtime dropped |

---

## Appendix A: Quick Reference — Verified Version Ceilings

| Dependency | Max JDK 8 Version | Newer Version Drops To | Status |
|------------|-------------------|----------------------|--------|
| OkHttp | 4.12.0 (4.x) | JDK 11 (5.x) | 🔴 Confirmed |
| Mockito | 4.11.0 (4.x) | JDK 11 (5.x) | 🔴 Confirmed |
| Caffeine | 2.9.3 (2.x) | JDK 11 (3.x) | 🔴 Confirmed |
| Spring Framework | 5.3.x | JDK 17 (6.x) | 🔴 Confirmed |
| WireMock | 2.35.0 (2.x) | JDK 11 (3.x) | 🔴 Confirmed |
| JAXB API | `javax.xml.bind` 2.3.x | JDK 11 (jakarta.xml.bind 3.x) | 🔴 Confirmed |
| Guava | No ceiling yet | JDK 11 (future, issue #6549 open) | 🟡 Monitor |
| Gson | No ceiling yet | No 3.0 release exists | 🟡 Monitor |
| Swagger Core | No ceiling yet | No 3.x release exists | 🟡 Monitor |
| JaCoCo | 0.8.8 (JDK 8 runtime) | 0.8.9+ = JDK 11 runner | 🟡 Monitor |
| SLF4J | 2.0.x (JDK 8) | No 3.x | 🟡 Monitor |
| protobuf-java | 3.x (JDK 8) | No 4.x | 🟡 Monitor |

## Appendix B: Build Verification Commands

```bash
# Check class file version of compiled classes
javap -verbose shenyu-client-core/target/classes/org/apache/shenyu/client/core/*.class 2>/dev/null | grep 'major version'
# Expected: major version: 52 (Java 8)

# Check dependency tree for a module
./mvnw -pl shenyu-client/shenyu-client-core dependency:tree -Dverbose

# Find non-JDK-8 class files in build output
find shenyu-client -path "*/target/classes/*.class" -exec file {} \; | grep -v "version 52.0"
# (should return nothing)

# Check a specific JAR for class version
CLASS=$(unzip -l <jar-path> 2>/dev/null | grep "\.class$" | grep -v "META-INF/versions" | head -1 | awk '{print $NF}')
unzip -p <jar-path> "$CLASS" 2>/dev/null | od -A n -t d1 -j 7 -N 1 | tr -d ' '
# Output: 52 = Java 8, 55 = Java 11, 61 = Java 17
```
