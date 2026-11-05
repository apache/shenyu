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

import org.springframework.core.io.ClassPathResource;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import org.springframework.data.redis.core.script.RedisScript;
import reactor.core.publisher.Mono;

import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.Base64;
import java.util.List;
import java.util.Objects;
import java.util.UUID;

/** Redis implementation using one atomic admission script and one idempotent release script. */
public final class RedisAgentMcpQuotaStore implements AgentMcpQuotaStore {

    private static final RedisScript<List> ADMIT = RedisScript.of(new ClassPathResource("mcp-quota.lua"), List.class);

    private static final RedisScript<Long> RELEASE = RedisScript.of(new ClassPathResource("mcp-quota-release.lua"), Long.class);

    private final ReactiveRedisTemplate<String, String> redis;

    public RedisAgentMcpQuotaStore(final ReactiveRedisTemplate<String, String> redis) {
        this.redis = Objects.requireNonNull(redis, "redis");
    }

    @Override
    public Mono<Admission> admit(final AgentMcpQuotaConfig policy, final String tenant, final String subject,
                                 final String correlationId, final Instant now) {
        Objects.requireNonNull(now, "now");
        long windowSeconds = policy.getInvocationWindow().toSeconds();
        long windowStart = Math.floorDiv(now.getEpochSecond(), windowSeconds)
                * policy.getInvocationWindow().toSeconds();
        long invocationTtl = windowStart + windowSeconds - now.getEpochSecond() + 1;
        long leaseExpiry = now.plus(policy.getLeaseTtl()).getEpochSecond();
        String lease = UUID.randomUUID().toString();
        String scope = (policy.getMode() == AgentMcpQuotaConfig.Mode.SHADOW ? "shadow" : "enforce")
                + ":" + scopePart(tenant) + ":" + scopePart(subject);
        String invocationKey = "shenyu:agent:mcp-quota:invocation:" + scope + ":" + windowStart;
        String concurrencyKey = "shenyu:agent:mcp-quota:concurrency:" + scope;
        return redis.execute(ADMIT, List.of(invocationKey, concurrencyKey),
                        List.of(Long.toString(policy.getInvocationLimit()), Long.toString(windowStart),
                                Long.toString(invocationTtl), Integer.toString(policy.getConcurrencyLimit()),
                                lease, Long.toString(leaseExpiry), Long.toString(policy.getLeaseTtl().toSeconds() + 1), Long.toString(now.getEpochSecond())))
                .next()
                .map(values -> new Admission(flag(values, 0),
                        flag(values, 1), flag(values, 2),
                        Objects.toString(values.get(3), "")));
    }

    @Override
    public Mono<Boolean> release(final AgentMcpQuotaConfig policy, final String tenant, final String subject,
                                 final String correlationId, final String lease) {
        Objects.requireNonNull(lease, "lease");
        String scope = (policy.getMode() == AgentMcpQuotaConfig.Mode.SHADOW ? "shadow" : "enforce")
                + ":" + scopePart(tenant) + ":" + scopePart(subject);
        String concurrencyKey = "shenyu:agent:mcp-quota:concurrency:" + scope;
        return redis.execute(RELEASE, List.of(concurrencyKey), List.of(lease))
                .next()
                .map(value -> Objects.equals("1", Objects.toString(value, null)));
    }

    private static String scopePart(final String value) {
        return Base64.getUrlEncoder().withoutPadding()
                .encodeToString(value.getBytes(StandardCharsets.UTF_8));
    }

    private static boolean flag(final List<?> values, final int index) {
        return Objects.equals("1", Objects.toString(values.get(index), null));
    }
}
