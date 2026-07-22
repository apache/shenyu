# Upstream Manual Status Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add persisted manual upstream offline control and make Admin, sync payloads, and Gateway selection honor it end to end.

**Architecture:** Introduce a shared `UpstreamManualStatusEnum`, persist it on `discovery_upstream`, update Admin service/controller flows to publish sync events after manual changes, and carry the new field through sync DTOs into Gateway cache objects where load-balancer selection filters forced-offline upstreams.

**Tech Stack:** Java, Spring MVC, MyBatis, Maven, JUnit 5, Mockito

---

### Task 1: Add Failing Admin Tests

**Files:**
- Modify: `shenyu-admin/src/test/java/org/apache/shenyu/admin/service/DiscoveryUpstreamServiceTest.java`
- Modify: `shenyu-admin/src/test/java/org/apache/shenyu/admin/service/SyncDataServiceTest.java`
- Test: `shenyu-admin/src/test/java/org/apache/shenyu/admin/service/DiscoveryUpstreamServiceTest.java`

- [ ] **Step 1: Write failing tests for manual status update and status short-circuit**
- [ ] **Step 2: Write failing assertions that sync payload exposes `manualStatus`**
- [ ] **Step 3: Run admin tests to verify they fail for missing field and behavior**
- [ ] **Step 4: Keep failures focused on the new contract**

### Task 2: Add Failing Gateway Tests

**Files:**
- Modify: `shenyu-loadbalancer/src/test/java/org/apache/shenyu/loadbalancer/factory/LoadBalancerFactoryTest.java`
- Modify: `shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-divide/src/test/java/org/apache/shenyu/plugin/divide/handler/DivideUpstreamDataHandlerTest.java`
- Test: `shenyu-loadbalancer/src/test/java/org/apache/shenyu/loadbalancer/factory/LoadBalancerFactoryTest.java`

- [ ] **Step 1: Add a failing load-balancer test that excludes `FORCE_OFFLINE` upstreams**
- [ ] **Step 2: Add a failing divide handler test that maps sync payload `manualStatus` into cached upstreams**
- [ ] **Step 3: Run targeted gateway tests to verify red state**

### Task 3: Implement Shared Enum And DTO Changes

**Files:**
- Create: `shenyu-common/src/main/java/org/apache/shenyu/common/enums/UpstreamManualStatusEnum.java`
- Modify: `shenyu-common/src/main/java/org/apache/shenyu/common/dto/DiscoveryUpstreamData.java`
- Modify: `shenyu-loadbalancer/src/main/java/org/apache/shenyu/loadbalancer/entity/Upstream.java`

- [ ] **Step 1: Add the shared enum with `NONE` and `FORCE_OFFLINE`**
- [ ] **Step 2: Extend sync DTO and cached upstream entity with `manualStatus`**
- [ ] **Step 3: Keep defaults backward compatible with `NONE`**

### Task 4: Implement Admin Persistence And API

**Files:**
- Modify: `shenyu-admin/src/main/resources/sql-script/h2/schema.sql`
- Modify: `shenyu-admin/src/main/java/org/apache/shenyu/admin/model/entity/DiscoveryUpstreamDO.java`
- Modify: `shenyu-admin/src/main/java/org/apache/shenyu/admin/model/dto/DiscoveryUpstreamDTO.java`
- Modify: `shenyu-admin/src/main/java/org/apache/shenyu/admin/model/vo/DiscoveryUpstreamVO.java`
- Modify: `shenyu-admin/src/main/java/org/apache/shenyu/admin/mapper/DiscoveryUpstreamMapper.java`
- Modify: `shenyu-admin/src/main/resources/mappers/discovery-upstream-sqlmap.xml`
- Modify: `shenyu-admin/src/main/java/org/apache/shenyu/admin/transfer/DiscoveryTransfer.java`
- Modify: `shenyu-admin/src/main/java/org/apache/shenyu/admin/service/DiscoveryUpstreamService.java`
- Modify: `shenyu-admin/src/main/java/org/apache/shenyu/admin/service/impl/DiscoveryUpstreamServiceImpl.java`
- Create: `shenyu-admin/src/main/java/org/apache/shenyu/admin/model/dto/UpstreamManualStatusDTO.java`
- Create: `shenyu-admin/src/main/java/org/apache/shenyu/admin/controller/UpstreamController.java`

- [ ] **Step 1: Persist `manual_status` and map it through DO/DTO/VO/Mapper**
- [ ] **Step 2: Add service methods to change manual status and publish fresh discovery events**
- [ ] **Step 3: Add `/upstream/offline` and `/upstream/online` controller endpoints**

### Task 5: Implement Heartbeat Short-Circuit And Gateway Filtering

**Files:**
- Modify: `shenyu-admin/src/main/java/org/apache/shenyu/admin/service/register/AbstractShenyuClientRegisterServiceImpl.java`
- Modify: `shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-divide/src/main/java/org/apache/shenyu/plugin/divide/handler/DivideUpstreamDataHandler.java`
- Modify: `shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-websocket/src/main/java/org/apache/shenyu/plugin/websocket/handler/WebSocketUpstreamDataHandler.java`
- Modify: `shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-rpc/shenyu-plugin-grpc/src/main/java/org/apache/shenyu/plugin/grpc/handler/GrpcDiscoveryUpstreamDataHandler.java`
- Modify: `shenyu-loadbalancer/src/main/java/org/apache/shenyu/loadbalancer/factory/LoadBalancerFactory.java`

- [ ] **Step 1: Prevent alive/status recovery when the DB record is `FORCE_OFFLINE`**
- [ ] **Step 2: Map synced `manualStatus` into plugin-specific upstream cache objects**
- [ ] **Step 3: Filter forced-offline upstreams before selection**

### Task 6: Verify Green State

**Files:**
- Modify: `docs/superpowers/specs/2026-04-01-upstream-manual-status-design.md`
- Modify: `docs/superpowers/plans/2026-04-01-upstream-manual-status.md`

- [ ] **Step 1: Run targeted Maven tests for admin, loadbalancer, and divide modules**
- [ ] **Step 2: Run a focused compile if any cross-module breakage appears**
- [ ] **Step 3: Review git diff for unintended changes**
- [ ] **Step 4: Commit with one feature commit**
