/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.plugin.agent.gateway.governance;

import org.apache.shenyu.common.audit.AgentInvocationAuditEvent;
import org.apache.shenyu.common.dto.AgentRequestContext;
import org.apache.shenyu.common.governance.AgentGovernanceDecision;
import org.apache.shenyu.common.metrics.AgentRuntimeMetrics;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.AfterEach;
import org.springframework.data.redis.connection.RedisStandaloneConfiguration;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import org.springframework.data.redis.serializer.RedisSerializationContext;
import org.springframework.data.redis.serializer.StringRedisSerializer;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.UUID;
import java.util.ArrayList;
import java.util.Base64;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

final class AgentMcpQuotaRedisStoreTest {

    private static final String HOST = System.getProperty("shenyu.agent.mcp-quota.redis.host", "127.0.0.1");

    private static final int PORT = Integer.getInteger("shenyu.agent.mcp-quota.redis.port", 6379);

    private final List<LettuceConnectionFactory> factories = new ArrayList<>();

    private static boolean isRedisAvailable() {
        try (Socket socket = new Socket()) {
            socket.connect(new InetSocketAddress(HOST, PORT), 500);
            return true;
        } catch (IOException error) {
            return false;
        }
    }

    @BeforeAll
    static void requiredRedisMustBeAvailable() {
        if (Boolean.getBoolean("shenyu.agent.mcp-quota.redis.required")) {
            assertTrue(isRedisAvailable(), "Required real Redis service is unavailable");
        }
    }

    @AfterEach
    void closeConnections() {
        factories.forEach(LettuceConnectionFactory::destroy);
    }

    private ReactiveRedisTemplate<String, String> template() {
        LettuceConnectionFactory factory = new LettuceConnectionFactory(new RedisStandaloneConfiguration(HOST, PORT));
        factory.afterPropertiesSet();
        factories.add(factory);
        RedisSerializationContext<String, String> serializationContext =
                RedisSerializationContext.<String, String>newSerializationContext()
                        .key(StringRedisSerializer.UTF_8)
                        .value(StringRedisSerializer.UTF_8)
                        .hashKey(StringRedisSerializer.UTF_8)
                        .hashValue(StringRedisSerializer.UTF_8)
                        .build();
        return new ReactiveRedisTemplate<>(factory, serializationContext);
    }

    @Test
    void twoStoreInstancesCannotExceedSharedInvocationLimit() {
        Assumptions.assumeTrue(isRedisAvailable(), "Start Redis to run distributed quota tests");
        ReactiveRedisTemplate<String, String> template = template();
        String tenant = tenant();
        AgentMcpQuotaConfig policy = policy(AgentMcpQuotaConfig.Mode.ENFORCE, 5, 100);
        RedisAgentMcpQuotaStore first = new RedisAgentMcpQuotaStore(template);
        RedisAgentMcpQuotaStore second = new RedisAgentMcpQuotaStore(template());
        long allowed = Flux.range(0, 10)
                .flatMap(index -> (index % 2 == 0 ? first : second)
                        .admit(policy, tenant, "subject", UUID.randomUUID().toString(), Instant.now().plusSeconds(index % 2 == 0 ? 3600 : -3600)))
                .filter(AgentMcpQuotaStore.Admission::allowed)
                .count()
                .block(Duration.ofSeconds(10));
        assertEquals(5, allowed);
    }

    @Test
    void concurrencyAdmissionReleaseAndIdempotenceAreAtomic() {
        Assumptions.assumeTrue(isRedisAvailable(), "Start Redis to run distributed quota tests");
        ReactiveRedisTemplate<String, String> template = template();
        String tenant = tenant();
        String subject = subject();
        AgentMcpQuotaConfig policy = policy(AgentMcpQuotaConfig.Mode.ENFORCE, 100, 2);
        RedisAgentMcpQuotaStore store = new RedisAgentMcpQuotaStore(template);
        List<AgentMcpQuotaStore.Admission> admissions = Flux.range(0, 3)
                .flatMap(index -> store.admit(policy, tenant, subject, UUID.randomUUID().toString(), Instant.now()))
                .collectList()
                .block(Duration.ofSeconds(10));
        assertEquals(2, admissions.stream().filter(AgentMcpQuotaStore.Admission::allowed).count());
        assertEquals(1, admissions.stream().filter(AgentMcpQuotaStore.Admission::concurrencyLimited).count());
        admissions = admissions.stream().filter(AgentMcpQuotaStore.Admission::allowed).toList();
        AgentMcpQuotaStore.Admission first = admissions.get(0);
        AgentMcpQuotaStore.Admission second = admissions.get(1);
        assertTrue(store.release(policy, tenant, subject, "request-1", first.lease()).block(Duration.ofSeconds(5)));
        assertFalse(store.release(policy, tenant, subject, "request-2", first.lease()).block(Duration.ofSeconds(5)));
        assertTrue(store.release(policy, tenant, subject, "request-2", second.lease()).block(Duration.ofSeconds(5)));
        assertFalse(store.release(policy, tenant, subject, "request-2", second.lease()).block(Duration.ofSeconds(5)));
        AgentMcpQuotaStore.Admission recovered = store.admit(policy, tenant, subject,
                UUID.randomUUID().toString(), Instant.now()).block(Duration.ofSeconds(5));
        assertTrue(recovered.allowed());
    }

    @Test
    void aLaterArrivalCannotReclaimAnUnexpiredLease() {
        Assumptions.assumeTrue(isRedisAvailable(), "Start Redis to run distributed quota tests");
        AgentMcpQuotaConfig policy = AgentMcpQuotaConfig.of(AgentMcpQuotaConfig.Mode.ENFORCE,
                100, Duration.ofSeconds(60), 1, Duration.ofSeconds(30), false);
        RedisAgentMcpQuotaStore store = new RedisAgentMcpQuotaStore(template());
        String tenant = tenant();
        String subject = subject();
        Instant now = Instant.now();
        assertTrue(store.admit(policy, tenant, subject, "first", now).block(Duration.ofSeconds(5)).allowed());
        AgentMcpQuotaStore.Admission second = store.admit(policy, tenant, subject, "second", now.plusSeconds(1))
                .block(Duration.ofSeconds(5));
        assertFalse(second.allowed());
        assertTrue(second.concurrencyLimited());
        assertFalse(store.admit(policy, tenant, subject, "skewed", now.plusSeconds(3600)).block(Duration.ofSeconds(5)).allowed());
    }

    @Test
    void expiredLeaseRecoversWithinConfiguredBound() {
        Assumptions.assumeTrue(isRedisAvailable(), "Start Redis to run distributed quota tests");
        ReactiveRedisTemplate<String, String> template = template();
        String tenant = tenant();
        String subject = subject();
        AgentMcpQuotaConfig policy = AgentMcpQuotaConfig.of(AgentMcpQuotaConfig.Mode.ENFORCE,
                100, Duration.ofSeconds(60), 1, Duration.ofSeconds(1), false);
        RedisAgentMcpQuotaStore store = new RedisAgentMcpQuotaStore(template);
        assertTrue(store.admit(policy, tenant, subject, UUID.randomUUID().toString(), Instant.now())
                .block(Duration.ofSeconds(5)).allowed());
        AgentMcpQuotaStore.Admission denied = store.admit(policy, tenant, subject,
                UUID.randomUUID().toString(), Instant.now()).block(Duration.ofSeconds(5));
        assertFalse(denied.allowed());
        assertTrue(denied.concurrencyLimited());
        await().atMost(Duration.ofSeconds(5)).until(() -> store.admit(policy, tenant, subject,
                UUID.randomUUID().toString(), Instant.now().minusSeconds(3600)).block(Duration.ofSeconds(2)).allowed());
    }

    @Test
    void realRedisPolicyFailureRejectsBeforeToolExecutionAndRecordsAudit() {
        Assumptions.assumeTrue(isRedisAvailable(), "Start Redis to run distributed quota tests");
        ReactiveRedisTemplate<String, String> template = template();
        String tenant = tenant();
        String subject = subject();
        String tag = "{enforce:" + Base64.getUrlEncoder().withoutPadding().encodeToString(tenant.getBytes(StandardCharsets.UTF_8)) + "}";
        String tenantKey = "shenyu:agent:mcp-quota:invocation:" + tag;
        String concurrencyKey = "shenyu:agent:mcp-quota:concurrency:" + tag + ":"
                + Base64.getUrlEncoder().withoutPadding().encodeToString(subject.getBytes(StandardCharsets.UTF_8));
        List<AgentGovernanceDecision> decisions = new ArrayList<>();
        List<AgentInvocationAuditEvent> audits = new ArrayList<>();
        AgentRuntimeMetrics metrics = new AgentRuntimeMetrics() {
            @Override
            public void governanceDecision(final AgentGovernanceDecision decision) {
                decisions.add(decision);
            }
        };
        AgentMcpQuotaGovernor governor = new AgentMcpQuotaGovernor(policy(AgentMcpQuotaConfig.Mode.ENFORCE, 100, 1),
                new RedisAgentMcpQuotaStore(template), metrics, "test", audits::add);
        AtomicBoolean executed = new AtomicBoolean();
        try {
            assertTrue(template.opsForValue().set(tenantKey, "wrong-redis-type", Duration.ofSeconds(10)).block(Duration.ofSeconds(5)));
            StepVerifier.create(governor.withQuota(tenant, subject, "policy-error", 1, Mono.fromSupplier(() -> {
                executed.set(true);
                return "unexpected";
            })).contextWrite(context -> context.put(AgentRequestContext.REACTOR_KEY,
                    new AgentRequestContext("policy-error", "mcp", "selector", "rule").withIdentity(tenant, subject))))
                    .expectErrorSatisfies(error -> {
                        assertTrue(error instanceof AgentMcpQuotaException);
                        AgentGovernanceDecision decision = ((AgentMcpQuotaException) error).getDecision();
                        assertEquals(AgentGovernanceDecision.Result.DENY, decision.getResult());
                        assertEquals(AgentGovernanceDecision.Reason.POLICY_ERROR, decision.getReason());
                    }).verify(Duration.ofSeconds(5));
            assertFalse(executed.get());
            assertEquals(1, decisions.size());
            assertEquals(1, audits.size());
            assertEquals(AgentInvocationAuditEvent.Decision.REJECTED, audits.get(0).getDecision());
            assertEquals("POLICY_ERROR", audits.get(0).getReason());
            assertFalse(template.hasKey(concurrencyKey).block(Duration.ofSeconds(5)));
        } finally {
            template.delete(tenantKey).block(Duration.ofSeconds(5));
        }
    }

    @Test
    void governorRecoversAfterAnAbandonedRedisLeaseExpires() {
        Assumptions.assumeTrue(isRedisAvailable(), "Start Redis to run distributed quota tests");
        ReactiveRedisTemplate<String, String> template = template();
        RedisAgentMcpQuotaStore store = new RedisAgentMcpQuotaStore(template);
        AgentMcpQuotaConfig policy = AgentMcpQuotaConfig.of(AgentMcpQuotaConfig.Mode.ENFORCE,
                100, Duration.ofSeconds(60), 1, Duration.ofSeconds(1), false);
        AgentMcpQuotaGovernor governor = new AgentMcpQuotaGovernor(policy, store, AgentRuntimeMetrics.NOOP, "test");
        String tenant = tenant();
        String subject = subject();
        assertTrue(store.admit(policy, tenant, subject, "abandoned", Instant.now()).block(Duration.ofSeconds(5)).allowed());
        StepVerifier.create(governor.withQuota(tenant, subject, "before-expiry", 1, Mono.just("unexpected")))
                .expectErrorSatisfies(error -> assertEquals(AgentGovernanceDecision.Reason.MCP_CONCURRENCY_LIMIT,
                        ((AgentMcpQuotaException) error).getDecision().getReason()))
                .verify(Duration.ofSeconds(5));
        await().atMost(Duration.ofSeconds(5)).until(() -> governor.withQuota(tenant, subject, "after-expiry", 1, Mono.just(true))
                .onErrorResume(AgentMcpQuotaException.class, error -> Mono.just(false)).block(Duration.ofSeconds(2)));
        assertTrue(store.admit(policy, tenant, subject, "after-release", Instant.now()).block(Duration.ofSeconds(5)).allowed());
    }

    @Test
    void shadowStateDoesNotDebitEnforceState() {
        Assumptions.assumeTrue(isRedisAvailable(), "Start Redis to run distributed quota tests");
        ReactiveRedisTemplate<String, String> template = template();
        String tenant = tenant();
        String subject = subject();
        AgentMcpQuotaConfig shadow = policy(AgentMcpQuotaConfig.Mode.SHADOW, 1, 100);
        AgentMcpQuotaConfig enforce = policy(AgentMcpQuotaConfig.Mode.ENFORCE, 1, 100);
        RedisAgentMcpQuotaStore store = new RedisAgentMcpQuotaStore(template);
        AgentMcpQuotaStore.Admission firstShadow = store.admit(shadow, tenant, subject,
                UUID.randomUUID().toString(), Instant.now()).block(Duration.ofSeconds(5));
        AgentMcpQuotaStore.Admission secondShadow = store.admit(shadow, tenant, subject,
                UUID.randomUUID().toString(), Instant.now()).block(Duration.ofSeconds(5));
        assertTrue(firstShadow.allowed());
        assertFalse(secondShadow.allowed());
        AgentMcpQuotaStore.Admission enforceAdmission = store.admit(enforce, tenant, subject,
                UUID.randomUUID().toString(), Instant.now()).block(Duration.ofSeconds(5));
        assertTrue(enforceAdmission.allowed());
    }

    @Test
    void invocationQuotaUsesDeterministicFixedWindows() {
        Assumptions.assumeTrue(isRedisAvailable(), "Start Redis to run distributed quota tests");
        ReactiveRedisTemplate<String, String> template = template();
        String tenant = tenant();
        String subject = subject();
        AgentMcpQuotaConfig policy = AgentMcpQuotaConfig.of(AgentMcpQuotaConfig.Mode.ENFORCE,
                1, Duration.ofSeconds(1), 100, Duration.ofSeconds(60), false);
        RedisAgentMcpQuotaStore store = new RedisAgentMcpQuotaStore(template);
        long stableWindow = Math.floorDiv(Instant.now().plusSeconds(60).getEpochSecond(), 60) * 60L;
        assertTrue(store.admit(policy, tenant, subject, UUID.randomUUID().toString(), Instant.ofEpochSecond(stableWindow))
                .block(Duration.ofSeconds(5)).allowed());
        assertFalse(store.admit(policy, tenant, subject, UUID.randomUUID().toString(), Instant.ofEpochSecond(stableWindow + 1))
                .block(Duration.ofSeconds(5)).allowed());
        await().atMost(Duration.ofSeconds(5)).until(() -> store.admit(policy, tenant, subject, UUID.randomUUID().toString(),
                Instant.ofEpochSecond(stableWindow + 3600)).block(Duration.ofSeconds(2)).allowed());
    }

    @Test
    void tenantAggregateCannotBeBypassedWithAnotherSubject() {
        Assumptions.assumeTrue(isRedisAvailable(), "Start Redis to run distributed quota tests");
        RedisAgentMcpQuotaStore store = new RedisAgentMcpQuotaStore(template());
        String tenant = tenant();
        AgentMcpQuotaConfig policy = policy(AgentMcpQuotaConfig.Mode.ENFORCE, 1, 100);
        assertTrue(store.admit(policy, tenant, "first", "one", Instant.now()).block(Duration.ofSeconds(5)).allowed());
        assertTrue(store.admit(policy, tenant, "second", "two", Instant.now().plusSeconds(3600))
                .block(Duration.ofSeconds(5)).invocationLimited());
    }

    @Test
    void sameSubjectInDifferentTenantsUsesIsolatedInvocationCounters() {
        Assumptions.assumeTrue(isRedisAvailable(), "Start Redis to run distributed quota tests");
        ReactiveRedisTemplate<String, String> template = template();
        String subject = subject();
        AgentMcpQuotaConfig policy = AgentMcpQuotaConfig.of(AgentMcpQuotaConfig.Mode.ENFORCE,
                1, Duration.ofSeconds(60), 100, Duration.ofSeconds(60), false);
        RedisAgentMcpQuotaStore store = new RedisAgentMcpQuotaStore(template);
        assertTrue(store.admit(policy, tenant(), subject, UUID.randomUUID().toString(), Instant.now())
                .block(Duration.ofSeconds(5)).allowed());
        assertTrue(store.admit(policy, tenant(), subject, UUID.randomUUID().toString(), Instant.now())
                .block(Duration.ofSeconds(5)).allowed());
    }

    @Test
    void encodedDimensionsCannotCollideAcrossTenantAndSubjectBoundaries() {
        Assumptions.assumeTrue(isRedisAvailable(), "Start Redis to run distributed quota tests");
        ReactiveRedisTemplate<String, String> template = template();
        AgentMcpQuotaConfig policy = policy(AgentMcpQuotaConfig.Mode.ENFORCE, 1, 100);
        RedisAgentMcpQuotaStore store = new RedisAgentMcpQuotaStore(template);
        String prefix = tenant();
        assertTrue(store.admit(policy, prefix, "subject:tool", UUID.randomUUID().toString(), Instant.now())
                .block(Duration.ofSeconds(5)).allowed());
        assertTrue(store.admit(policy, prefix + ":subject", "tool", UUID.randomUUID().toString(), Instant.now())
                .block(Duration.ofSeconds(5)).allowed());
    }

    private static String tenant() {
        return "quota-" + UUID.randomUUID();
    }

    private static String subject() {
        return "subject-" + UUID.randomUUID();
    }

    private static AgentMcpQuotaConfig policy(final AgentMcpQuotaConfig.Mode mode,
                                              final long invocationLimit, final int concurrencyLimit) {
        return AgentMcpQuotaConfig.of(mode, invocationLimit, Duration.ofSeconds(60),
                concurrencyLimit, Duration.ofSeconds(60), false);
    }
}
