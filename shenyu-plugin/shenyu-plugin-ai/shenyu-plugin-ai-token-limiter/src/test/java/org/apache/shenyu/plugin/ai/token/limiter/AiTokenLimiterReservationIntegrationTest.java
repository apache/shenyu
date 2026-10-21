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

package org.apache.shenyu.plugin.ai.token.limiter;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.rule.AiTokenLimiterHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.infra.redis.ShenyuReactiveRedisTemplate;
import org.apache.shenyu.infra.redis.serializer.ShenyuRedisSerializationContext;
import org.apache.shenyu.plugin.ai.token.limiter.budget.RedisTokenBudgetStore;
import org.apache.shenyu.plugin.ai.token.limiter.budget.RedisTokenBudgetStore.ReservationResult;
import org.apache.shenyu.plugin.ai.token.limiter.budget.TokenBudgetWindow;
import org.apache.shenyu.plugin.ai.token.limiter.handler.AiTokenLimiterPluginHandler;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfSystemProperty;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.test.StepVerifier;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.zip.GZIPOutputStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Plugin-to-Redis lifecycle tests using real scripts and two independent gateway Redis clients.
 */
@EnabledIfSystemProperty(named = "shenyu.test.redis.port", matches = "\\d+")
class AiTokenLimiterReservationIntegrationTest {

    private static final Duration TIMEOUT = Duration.ofSeconds(5);

    private static final long WINDOW_SECONDS = 9_007_199_254_740L;

    private static LettuceConnectionFactory firstFactory;

    private static LettuceConnectionFactory secondFactory;

    private static ReactiveRedisTemplate<String, String> firstTemplate;

    private static ReactiveRedisTemplate<String, String> secondTemplate;

    private final List<String> keys = new ArrayList<>();

    private final RuleData rule = RuleData.builder().selectorId("reservation-integration").id(UUID.randomUUID().toString()).build();

    private ReactiveRedisTemplate<?, ?> previousRedis;

    private ApplicationContext previousContext;

    private String scope;

    private String key;

    @BeforeAll
    static void createRedisClients() {
        final int port = Integer.parseInt(System.getProperty("shenyu.test.redis.port"));
        firstFactory = new LettuceConnectionFactory("127.0.0.1", port);
        firstFactory.afterPropertiesSet();
        secondFactory = new LettuceConnectionFactory("127.0.0.1", port);
        secondFactory.afterPropertiesSet();
        firstTemplate = new ShenyuReactiveRedisTemplate<>(firstFactory, ShenyuRedisSerializationContext.stringSerializationContext());
        secondTemplate = new ShenyuReactiveRedisTemplate<>(secondFactory, ShenyuRedisSerializationContext.stringSerializationContext());
    }

    @BeforeEach
    void configureOwnedRule() {
        previousContext = SpringBeanUtils.getInstance().getApplicationContext();
        final ApplicationContext context = mock(ApplicationContext.class);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils.getInstance().setApplicationContext(context);
        final AiTokenLimiterHandle handle = AiTokenLimiterHandle.newDefaultInstance();
        handle.setAiTokenLimitType("uri");
        handle.setTokenLimit(20L);
        handle.setReservationEnabled(true);
        handle.setReservationTokens(10L);
        handle.setTimeWindowSeconds(WINDOW_SECONDS);
        AiTokenLimiterPluginHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(rule), handle);
        previousRedis = AiTokenLimiterPluginHandler.REDIS_CACHED_HANDLE.get().obtainHandle(PluginEnum.AI_TOKEN_LIMITER.getName());
        scope = "SHENYU:AI:TOKENLIMIT:" + CacheKeyUtils.INST.getKey(rule) + ":/ai/chat";
        final TokenBudgetWindow window = new RedisTokenBudgetStore(firstTemplate).currentWindow(scope, Duration.ofSeconds(WINDOW_SECONDS)).block(TIMEOUT);
        assertNotNull(window);
        key = window.getKey();
        keys.add(key);
    }

    @AfterEach
    void restoreCachesAndDeleteOwnedKeys() {
        SpringBeanUtils.getInstance().setApplicationContext(previousContext);
        AiTokenLimiterPluginHandler.CACHED_HANDLE.get().removeHandle(CacheKeyUtils.INST.getKey(rule));
        if (Objects.nonNull(previousRedis)) {
            AiTokenLimiterPluginHandler.REDIS_CACHED_HANDLE.get().cachedHandle(PluginEnum.AI_TOKEN_LIMITER.getName(), previousRedis);
        } else {
            AiTokenLimiterPluginHandler.REDIS_CACHED_HANDLE.get().removeHandle(PluginEnum.AI_TOKEN_LIMITER.getName());
        }
        firstTemplate.delete(Flux.fromIterable(keys)).block(TIMEOUT);
    }

    @AfterAll
    static void closeRedisClients() {
        if (Objects.nonNull(firstFactory)) {
            firstFactory.destroy();
        }
        if (Objects.nonNull(secondFactory)) {
            secondFactory.destroy();
        }
    }

    @Test
    void testConcurrentGatewaysReserveBeforeForwardingAndReconcileActualUsage() {
        final Sinks.Empty<Void> responses = Sinks.empty();
        final Sinks.Empty<Void> allDecided = Sinks.empty();
        final AtomicInteger decisions = new AtomicInteger();
        final AtomicInteger admitted = new AtomicInteger();
        final AtomicInteger rejected = new AtomicInteger();
        final Runnable decided = () -> {
            if (decisions.incrementAndGet() == 3) {
                allDecided.tryEmitEmpty();
            }
        };
        final ShenyuPluginChain chain = exchange -> {
            admitted.incrementAndGet();
            decided.run();
            return responses.asMono().then(Mono.defer(() -> writeUsage(exchange, false)));
        };
        final List<Mono<Void>> executions = new ArrayList<>();
        for (int index = 0; index < 3; index++) {
            final MockServerWebExchange exchange = exchange();
            executions.add(execute(index % 2 == 0 ? firstTemplate : secondTemplate, exchange, chain).doOnSuccess(ignored -> {
                if (exchange.getResponse().getStatusCode() == HttpStatus.TOO_MANY_REQUESTS) {
                    rejected.incrementAndGet();
                    decided.run();
                }
            }));
        }
        final Mono<Void> releaseResponses = allDecided.asMono()
                .then(firstTemplate.<String, String>opsForHash().entries(key).collectMap(Map.Entry::getKey, Map.Entry::getValue)).doOnNext(state -> {
                    assertEquals(2, admitted.get());
                    assertEquals(1, rejected.get());
                    assertEquals("20", state.get("reserved"));
                    assertEquals("0", state.get("used"));
                    assertEquals(Sinks.EmitResult.OK, responses.tryEmitEmpty());
                }).then();

        StepVerifier.create(Mono.when(Mono.when(executions), releaseResponses)).expectComplete().verify(TIMEOUT);

        assertEquals("12", field(key, "used"));
        assertEquals("0", field(key, "reserved"));
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testJsonAndGzipResponsesSettleThroughRealRedis(final boolean gzip) {
        StepVerifier.create(execute(firstTemplate, exchange(), forwarded -> writeUsage(forwarded, gzip))).expectComplete().verify(TIMEOUT);

        assertEquals("6", field(key, "used"));
        assertEquals("0", field(key, "reserved"));
    }

    @Test
    void testDownstreamErrorRetainsUnknownReservation() {
        final IllegalStateException failure = new IllegalStateException("upstream failure");
        StepVerifier.create(execute(firstTemplate, exchange(), ignored -> Mono.error(failure))).expectErrorMatches(error -> error == failure).verify(TIMEOUT);

        assertEquals("0", field(key, "used"));
        assertEquals("10", field(key, "reserved"));
        assertTrue(entries(key).containsValue("USAGE_UNKNOWN"));
    }

    @Test
    void testCancelledRequestRetainsUnknownReservationAndFinishesAsyncCleanup() {
        final Sinks.Empty<Void> forwarded = Sinks.empty();
        StepVerifier.create(execute(firstTemplate, exchange(), ignored -> {
            forwarded.tryEmitEmpty();
            return Mono.never();
        })).then(() -> forwarded.asMono().block(TIMEOUT)).thenCancel().verify(TIMEOUT);

        final Map<String, String> state = awaitFieldPrefix("f:");
        assertEquals("0", state.get("used"));
        assertEquals("10", state.get("reserved"));
        assertTrue(state.containsValue("USAGE_UNKNOWN"));
    }

    @Test
    void testExpiredOriginalWindowIsNotRecreatedWhenResponseFinishes() {
        final Sinks.Empty<Void> forwarded = Sinks.empty();
        final Sinks.Empty<Void> response = Sinks.empty();
        final RedisTokenBudgetStore store = new RedisTokenBudgetStore(secondTemplate);
        final TokenBudgetWindow newer = store.currentWindow(scope, Duration.ofDays(36_500)).block(TIMEOUT);
        assertNotNull(newer);
        keys.add(newer.getKey());
        StepVerifier.create(execute(firstTemplate, exchange(), exchange -> {
            forwarded.tryEmitEmpty();
            return response.asMono().then(Mono.defer(() -> writeUsage(exchange, false)));
        })).then(() -> {
            forwarded.asMono().block(TIMEOUT);
            assertTrue(Boolean.TRUE.equals(firstTemplate.expire(key, Duration.ZERO).block(TIMEOUT)));
            assertEquals(ReservationResult.RESERVED, store.reserve(newer, "new-request", 20L, 10L).block(TIMEOUT));
            assertEquals(Sinks.EmitResult.OK, response.tryEmitEmpty());
        }).expectComplete().verify(TIMEOUT);

        assertFalse(Boolean.TRUE.equals(firstTemplate.hasKey(key).block(TIMEOUT)));
        assertEquals("0", field(newer.getKey(), "used"));
        assertEquals("10", field(newer.getKey(), "reserved"));
    }

    private Mono<Void> execute(final ReactiveRedisTemplate<String, String> template, final MockServerWebExchange exchange, final ShenyuPluginChain chain) {
        AiTokenLimiterPluginHandler.REDIS_CACHED_HANDLE.get().cachedHandle(PluginEnum.AI_TOKEN_LIMITER.getName(), template);
        return new AiTokenLimiterPlugin().doExecute(exchange, chain, null, rule);
    }

    private Mono<Void> writeUsage(final org.springframework.web.server.ServerWebExchange exchange, final boolean gzip) {
        final byte[] body = "{\"usage\":{\"completion_tokens\":6}}".getBytes(StandardCharsets.UTF_8);
        exchange.getResponse().getHeaders().setContentType(MediaType.APPLICATION_JSON);
        if (!gzip) {
            return exchange.getResponse().writeWith(Mono.just(exchange.getResponse().bufferFactory().wrap(body)));
        }
        final ByteArrayOutputStream bytes = new ByteArrayOutputStream();
        try (GZIPOutputStream output = new GZIPOutputStream(bytes)) {
            output.write(body);
        } catch (IOException exception) {
            return Mono.error(exception);
        }
        exchange.getResponse().getHeaders().set("Content-Encoding", "gzip");
        final byte[] compressed = bytes.toByteArray();
        return exchange.getResponse().writeWith(Flux.just(
                exchange.getResponse().bufferFactory().wrap(ByteBuffer.wrap(compressed, 0, 8)),
                exchange.getResponse().bufferFactory().wrap(ByteBuffer.wrap(compressed, 8, compressed.length - 8))));
    }

    private MockServerWebExchange exchange() {
        return MockServerWebExchange.from(MockServerHttpRequest.post("/ai/chat").build());
    }

    private String field(final String windowKey, final String field) {
        return firstTemplate.<String, String>opsForHash().get(windowKey, field).block(TIMEOUT);
    }

    private Map<String, String> entries(final String windowKey) {
        return firstTemplate.<String, String>opsForHash().entries(windowKey).collectMap(Map.Entry::getKey, Map.Entry::getValue).block(TIMEOUT);
    }

    private Map<String, String> awaitFieldPrefix(final String prefix) {
        final Map<String, String> result = Mono.defer(() -> firstTemplate.<String, String>opsForHash().entries(key).collectMap(Map.Entry::getKey, Map.Entry::getValue))
                .filter(state -> state.keySet().stream().anyMatch(field -> field.startsWith(prefix)))
                .repeatWhenEmpty(100, repeats -> repeats.delayElements(Duration.ofMillis(10))).block(TIMEOUT);
        assertNotNull(result);
        return result;
    }
}
