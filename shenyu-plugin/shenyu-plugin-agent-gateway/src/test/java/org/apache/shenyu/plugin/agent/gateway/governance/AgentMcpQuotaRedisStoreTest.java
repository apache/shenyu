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

import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.springframework.data.redis.connection.RedisStandaloneConfiguration;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import org.springframework.data.redis.serializer.RedisSerializationContext;
import org.springframework.data.redis.serializer.StringRedisSerializer;
import reactor.core.publisher.Flux;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

final class AgentMcpQuotaRedisStoreTest {

    private static final String HOST = System.getProperty("shenyu.agent.mcp-quota.redis.host", "127.0.0.1");

    private static final int PORT = Integer.getInteger("shenyu.agent.mcp-quota.redis.port", 6379);

    private static boolean isRedisAvailable() {
        try (Socket socket = new Socket()) {
            socket.connect(new InetSocketAddress(HOST, PORT), 500);
            return true;
        } catch (IOException error) {
            return false;
        }
    }

    private static ReactiveRedisTemplate<String, String> template() {
        LettuceConnectionFactory factory = new LettuceConnectionFactory(new RedisStandaloneConfiguration(HOST, PORT));
        factory.afterPropertiesSet();
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
        RedisAgentMcpQuotaStore second = new RedisAgentMcpQuotaStore(template);
        long allowed = Flux.range(0, 10)
                .flatMap(index -> (index % 2 == 0 ? first : second)
                        .admit(policy, tenant, "subject", UUID.randomUUID().toString(), Instant.now()))
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
        assertTrue(store.admit(policy, tenant, subject, "expired", now.plusSeconds(30)).block(Duration.ofSeconds(5)).allowed());
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
        AgentMcpQuotaStore.Admission afterExpiry = store.admit(policy, tenant, subject,
                UUID.randomUUID().toString(), Instant.now().plusSeconds(2)).block(Duration.ofSeconds(5));
        assertTrue(afterExpiry.allowed());
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
                1, Duration.ofSeconds(60), 100, Duration.ofSeconds(60), false);
        RedisAgentMcpQuotaStore store = new RedisAgentMcpQuotaStore(template);
        long stableWindow = Math.floorDiv(Instant.now().plusSeconds(60).getEpochSecond(), 60) * 60L;
        assertTrue(store.admit(policy, tenant, subject, UUID.randomUUID().toString(), Instant.ofEpochSecond(stableWindow))
                .block(Duration.ofSeconds(5)).allowed());
        assertFalse(store.admit(policy, tenant, subject, UUID.randomUUID().toString(), Instant.ofEpochSecond(stableWindow + 1))
                .block(Duration.ofSeconds(5)).allowed());
        assertTrue(store.admit(policy, tenant, subject, UUID.randomUUID().toString(), Instant.ofEpochSecond(stableWindow + 60))
                .block(Duration.ofSeconds(5)).allowed());
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
        assertTrue(store.admit(policy, "tenant", "subject:tool", UUID.randomUUID().toString(), Instant.now())
                .block(Duration.ofSeconds(5)).allowed());
        assertTrue(store.admit(policy, "tenant:subject", "tool", UUID.randomUUID().toString(), Instant.now())
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
