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
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.convert.rule.AiTokenLimiterHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.metrics.AiTokenBudgetObserver;
import org.apache.shenyu.plugin.ai.token.limiter.handler.AiTokenLimiterPluginHandler;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.context.ApplicationContext;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import org.springframework.data.redis.core.script.RedisScript;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.TimeoutException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Request lifecycle tests with controlled Redis replies, without detached accounting subscriptions.
 */
class AiTokenLimiterReservationTest {

    private static final Duration TIMEOUT = Duration.ofSeconds(5);

    private final RuleData rule = RuleData.builder().selectorId("reservation-lifecycle-selector").id("reservation-lifecycle-rule").build();

    private final AiTokenLimiterPlugin plugin = new AiTokenLimiterPlugin();

    private final AtomicLong clock = new AtomicLong(61_234L);

    private final List<List<String>> reservations = new CopyOnWriteArrayList<>();

    private final List<List<String>> settlements = new CopyOnWriteArrayList<>();

    private final List<List<String>> completions = new CopyOnWriteArrayList<>();

    private final List<String> reservationKeys = new CopyOnWriteArrayList<>();

    private final List<String> settlementKeys = new CopyOnWriteArrayList<>();

    private final List<String> observations = new CopyOnWriteArrayList<>();

    private Flux<Long> reserveReply = Flux.just(1L);

    private Flux<Long> settleReply = Flux.just(1L);

    private Flux<Long> completionReply = Flux.just(2L);

    private ReactiveRedisTemplate<String, String> template;

    private ReactiveRedisTemplate<?, ?> previousRedis;

    private ApplicationContext previousContext;

    @BeforeEach
    @SuppressWarnings("unchecked")
    void setUp() {
        previousContext = SpringBeanUtils.getInstance().getApplicationContext();
        final ApplicationContext context = mock(ApplicationContext.class);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils.getInstance().setApplicationContext(context);
        template = mock(ReactiveRedisTemplate.class);
        when(template.execute(org.mockito.ArgumentMatchers.<RedisScript<Long>>any(), anyList(), anyList())).thenAnswer(invocation -> {
            final RedisScript<Long> script = invocation.getArgument(0);
            final List<String> keys = invocation.getArgument(1);
            final List<String> args = invocation.getArgument(2);
            return Flux.defer(() -> {
                final String source = script.getScriptAsString();
                if (source.contains("-- Select fixed windows")) {
                    return Flux.just(clock.get());
                }
                if (source.contains("-- Atomically reserve")) {
                    reservations.add(List.copyOf(args));
                    reservationKeys.add(keys.get(0));
                    return reserveReply;
                }
                if (source.contains("-- Reconcile one reservation")) {
                    settlements.add(List.copyOf(args));
                    settlementKeys.add(keys.get(0));
                    return settleReply;
                }
                completions.add(List.copyOf(args));
                return completionReply;
            });
        });
        final AiTokenLimiterHandle handle = AiTokenLimiterHandle.newDefaultInstance();
        handle.setAiTokenLimitType("uri");
        handle.setReservationEnabled(true);
        handle.setReservationTokens(10L);
        AiTokenLimiterPluginHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(rule), handle);
        previousRedis = AiTokenLimiterPluginHandler.REDIS_CACHED_HANDLE.get().obtainHandle(PluginEnum.AI_TOKEN_LIMITER.getName());
        AiTokenLimiterPluginHandler.REDIS_CACHED_HANDLE.get().cachedHandle(PluginEnum.AI_TOKEN_LIMITER.getName(), template);
    }

    @AfterEach
    void restoreOwnedCaches() {
        SpringBeanUtils.getInstance().setApplicationContext(previousContext);
        AiTokenLimiterPluginHandler.CACHED_HANDLE.get().removeHandle(CacheKeyUtils.INST.getKey(rule));
        if (Objects.nonNull(previousRedis)) {
            AiTokenLimiterPluginHandler.REDIS_CACHED_HANDLE.get().cachedHandle(PluginEnum.AI_TOKEN_LIMITER.getName(), previousRedis);
        } else {
            AiTokenLimiterPluginHandler.REDIS_CACHED_HANDLE.get().removeHandle(PluginEnum.AI_TOKEN_LIMITER.getName());
        }
    }

    @ParameterizedTest
    @ValueSource(longs = {0L, 6L, 30L})
    void testSuccessfulJsonSettlesReportedUsageBeforeRequestCompletion(final long tokens) {
        final MockServerWebExchange exchange = exchange();
        StepVerifier.create(execute(exchange, response("{\"usage\":{\"completion_tokens\":" + tokens + "}}"))).expectComplete().verify(TIMEOUT);

        assertEquals(List.of(Long.toString(tokens), requestId()), settlements.get(0));
        assertEquals(reservationKeys, settlementKeys);
        assertTrue(completions.isEmpty());
        verify(template, never()).opsForValue();
        assertEquals("{\"usage\":{\"completion_tokens\":" + tokens + "}}", exchange.getResponse().getBodyAsString().block(TIMEOUT));
        assertEquals(List.of("ADMITTED:10", "USAGE_REPORTED:" + tokens, "SETTLED:0"), observations);
    }

    @ParameterizedTest
    @ValueSource(strings = {"{}", "not-json", "{\"usage\":{\"completion_tokens\":null}}", "{\"usage\":{\"completion_tokens\":\"0\"}}",
        "{\"usage\":{\"completion_tokens\":-1}}", "{\"usage\":{\"completion_tokens\":1.5}}", "{\"usage\":{\"completion_tokens\":9007199254740992}}",
        "{\"usage\":{\"completion_tokens\":0}}garbage"})
    void testMissingOrInvalidUsageRetainsReservation(final String body) {
        StepVerifier.create(execute(exchange(), response(body))).expectComplete().verify(TIMEOUT);

        assertEquals(List.of(requestId(), "USAGE_UNKNOWN"), completions.get(0));
        assertTrue(settlements.isEmpty());
        assertEquals(List.of("ADMITTED:10", "USAGE_HELD:0"), observations);
    }

    @Test
    void testRejectedAdmissionDoesNotForwardOrCleanUpAnotherReservation() {
        reserveReply = Flux.just(0L);
        final MockServerWebExchange exchange = exchange();
        final AtomicInteger forwarded = new AtomicInteger();

        StepVerifier.create(execute(exchange, ignored -> {
            forwarded.incrementAndGet();
            return Mono.empty();
        })).expectComplete().verify(TIMEOUT);

        assertEquals(HttpStatus.TOO_MANY_REQUESTS, exchange.getResponse().getStatusCode());
        assertEquals(0, forwarded.get());
        assertTrue(completions.isEmpty());
        assertTrue(settlements.isEmpty());
        assertEquals(List.of("REJECTED:0"), observations);
    }

    @Test
    void testDownstreamFailureRetainsUnknownUsageAndPreservesOriginalFailure() {
        final IllegalStateException failure = new IllegalStateException("upstream failed");

        StepVerifier.create(execute(exchange(), ignored -> Mono.error(failure))).expectErrorMatches(error -> error == failure).verify();

        assertEquals(List.of(requestId(), "USAGE_UNKNOWN"), completions.get(0));
    }

    @Test
    void testSynchronousDownstreamFailureIsAlsoPotentialForwarding() {
        final IllegalStateException failure = new IllegalStateException("chain creation failed");

        StepVerifier.create(execute(exchange(), ignored -> {
            throw failure;
        })).expectErrorMatches(error -> error == failure).verify();

        assertEquals(List.of(requestId(), "USAGE_UNKNOWN"), completions.get(0));
    }

    @Test
    void testCancellationAfterForwardingRetainsUnknownUsage() {
        final AtomicInteger forwarded = new AtomicInteger();
        StepVerifier.create(execute(exchange(), ignored -> {
            forwarded.incrementAndGet();
            return Mono.never();
        })).then(() -> assertEquals(1, forwarded.get())).thenCancel().verify(TIMEOUT);

        assertEquals(List.of(requestId(), "USAGE_UNKNOWN"), completions.get(0));
        assertTrue(settlements.isEmpty());
    }

    @Test
    void testCancellationDuringAdmissionNeverForwardsAndAttemptsConfirmedRelease() {
        reserveReply = Flux.never();
        completionReply = Flux.just(1L);
        final AtomicInteger forwarded = new AtomicInteger();
        StepVerifier.create(execute(exchange(), ignored -> {
            forwarded.incrementAndGet();
            return Mono.empty();
        })).then(() -> assertEquals(1, reservations.size())).thenCancel().verify(TIMEOUT);

        assertEquals(0, forwarded.get());
        assertEquals(List.of(requestId(), "NOT_FORWARDED"), completions.get(0));
        assertEquals(List.of("RELEASED:0"), observations);
    }

    @Test
    void testRedisAdmissionFailureNeverFallsBackToLegacyOrForwards() {
        final IllegalStateException failure = new IllegalStateException("Redis unavailable");
        reserveReply = Flux.error(failure);
        final AtomicInteger forwarded = new AtomicInteger();
        StepVerifier.create(execute(exchange(), ignored -> {
            forwarded.incrementAndGet();
            return Mono.empty();
        })).expectErrorMatches(error -> error == failure).verify();

        assertEquals(0, forwarded.get());
        assertEquals(List.of(requestId(), "NOT_FORWARDED"), completions.get(0));
        verify(template, never()).opsForValue();
        assertTrue(observations.contains("ADMISSION_ERROR:0"));
        assertTrue(observations.stream().noneMatch(value -> value.startsWith("ADMITTED:")));
    }

    @Test
    void testRequestCompletionWaitsForSettlement() {
        final Sinks.One<Long> settled = Sinks.one();
        settleReply = settled.asMono().flux();
        StepVerifier.create(execute(exchange(), response("{\"usage\":{\"completion_tokens\":6}}")))
                .then(() -> {
                    assertEquals(1, settlements.size());
                    assertEquals(Sinks.EmitResult.OK, settled.tryEmitValue(1L));
                }).expectComplete().verify(TIMEOUT);
    }

    @Test
    void testCleanupFailureDoesNotHideOriginalDownstreamError() {
        final IllegalStateException original = new IllegalStateException("upstream failed");
        final IllegalStateException cleanup = new IllegalStateException("Redis failed");
        completionReply = Flux.error(cleanup);
        StepVerifier.create(execute(exchange(), ignored -> Mono.error(original))).expectErrorMatches(error -> error == original).verify();

        assertEquals(1, original.getSuppressed().length);
        assertSame(cleanup, original.getSuppressed()[0]);
    }

    @Test
    void testResponseAfterWindowBoundarySettlesOriginalWindow() {
        StepVerifier.create(execute(exchange(), forwarded -> {
            clock.set(121_234L);
            return response("{\"usage\":{\"completion_tokens\":6}}").execute(forwarded);
        })).expectComplete().verify(TIMEOUT);

        assertEquals(1, reservations.size());
        assertEquals(reservationKeys, settlementKeys);
        assertTrue(reservationKeys.get(0).endsWith(":60000:60000"));
    }

    @Test
    void testEachSubscriptionOwnsADifferentRequestId() {
        final Mono<Void> execution = execute(exchange(), ignored -> Mono.empty());
        StepVerifier.create(execution).expectComplete().verify(TIMEOUT);
        StepVerifier.create(execution).expectComplete().verify(TIMEOUT);

        assertEquals(2, reservations.size());
        assertNotEquals(reservations.get(0).get(3), reservations.get(1).get(3));
    }

    @Test
    void testStaleAdmissionReselectsWindowOnce() {
        final AtomicInteger attempts = new AtomicInteger();
        reserveReply = Flux.defer(() -> {
            if (attempts.getAndIncrement() == 0) {
                clock.set(121_234L);
                return Flux.just(6L);
            }
            return Flux.just(1L);
        });
        StepVerifier.create(execute(exchange(), response("{\"usage\":{\"completion_tokens\":6}}"))).expectComplete().verify(TIMEOUT);

        assertEquals(2, reservations.size());
        assertEquals(reservations.get(0).get(3), reservations.get(1).get(3));
        assertNotEquals(reservationKeys.get(0), reservationKeys.get(1));
        assertEquals(reservationKeys.get(1), settlementKeys.get(0));
    }

    @Test
    void testRepeatedlyStaleAdmissionStopsWithoutForwarding() {
        reserveReply = Flux.just(6L);
        final AtomicInteger forwarded = new AtomicInteger();
        StepVerifier.create(execute(exchange(), ignored -> {
            forwarded.incrementAndGet();
            return Mono.empty();
        })).expectError(IllegalStateException.class).verify();

        assertEquals(2, reservations.size());
        assertEquals(0, forwarded.get());
        assertTrue(completions.isEmpty());
    }

    @Test
    void testStreamingReportsAreReconciledOnceWithLatestUsage() {
        StepVerifier.create(execute(exchange(), response("data: {\"usage\":{\"completion_tokens\":2}}\n\n"
                + "data: {\"usage\":{\"completion_tokens\":6}}\n\ndata: [DONE]\n\n"))).expectComplete().verify(TIMEOUT);

        assertEquals(List.of(List.of("6", requestId())), settlements);
        assertTrue(completions.isEmpty());
    }

    @Test
    void testReportedStreamingUsageIsSettledEvenWhenClientCancels() {
        final Mono<Void> execution = execute(exchange(), forwarded -> {
            forwarded.getResponse().getHeaders().setContentType(MediaType.TEXT_EVENT_STREAM);
            return forwarded.getResponse().writeAndFlushWith(Flux.just(Flux.concat(
                    Mono.just(forwarded.getResponse().bufferFactory().wrap("data: {\"usage\":{\"completion_tokens\":6}}\n\n".getBytes(StandardCharsets.UTF_8))),
                    Mono.never())));
        });
        StepVerifier.create(execution).then(() -> assertEquals(1, reservations.size())).thenCancel().verify(TIMEOUT);

        assertEquals(List.of(List.of("6", requestId())), settlements);
        assertTrue(completions.isEmpty());
    }

    @Test
    void testSettlementFailureAfterDownstreamSuccessIsReported() {
        final IllegalStateException failure = new IllegalStateException("settlement failed");
        settleReply = Flux.error(failure);

        StepVerifier.create(execute(exchange(), response("{\"usage\":{\"completion_tokens\":6}}")))
                .expectErrorMatches(error -> error.getCause() == failure).verify(TIMEOUT);
        assertEquals(1, settlements.size());
        assertTrue(completions.isEmpty());
        assertEquals(List.of("ADMITTED:10", "USAGE_REPORTED:6", "ACCOUNTING_ERROR:0"), observations);
    }

    @Test
    void testSettlementTimeoutDoesNotHoldRequestCompletionForever() {
        settleReply = Flux.never();

        StepVerifier.withVirtualTime(() -> execute(exchange(), response("{\"usage\":{\"completion_tokens\":6}}")))
                .thenAwait(Duration.ofSeconds(5))
                .expectErrorMatches(error -> error.getCause() instanceof TimeoutException).verify(TIMEOUT);
    }

    @Test
    void testRuleSnapshotIsRetainedWhenConfigurationChangesBeforeSubscription() {
        final Mono<Void> execution = execute(exchange(), response("{\"usage\":{\"completion_tokens\":6}}"));
        final AiTokenLimiterHandle updated = AiTokenLimiterPluginHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        updated.setTokenLimit(200L);
        updated.setReservationTokens(20L);
        updated.setTimeWindowSeconds(120L);

        StepVerifier.create(execution).expectComplete().verify(TIMEOUT);
        assertEquals(List.of("100", "10", "60000"), reservations.get(0).subList(0, 3));
    }

    @Test
    void testJsonUsageSplitAcrossResponseBuffersIsCollectedBeforeCleanup() {
        StepVerifier.create(execute(exchange(), forwarded -> forwarded.getResponse().writeWith(Flux.just(
                forwarded.getResponse().bufferFactory().wrap("{\"usage\":{\"completion_".getBytes(StandardCharsets.UTF_8)),
                forwarded.getResponse().bufferFactory().wrap("tokens\":6}}".getBytes(StandardCharsets.UTF_8))))))
                .expectComplete().verify(TIMEOUT);

        assertEquals(List.of(List.of("6", requestId())), settlements);
    }

    @Test
    void testExpiredWindowStillReportsActualUsageSeparatelyFromSettlement() {
        settleReply = Flux.just(3L);
        StepVerifier.create(execute(exchange(), response("{\"usage\":{\"completion_tokens\":6}}"))).expectComplete().verify(TIMEOUT);

        assertEquals(List.of("ADMITTED:10", "USAGE_REPORTED:6", "WINDOW_EXPIRED:0"), observations);
    }

    @Test
    void testBrokenObserverCannotChangeAdmissionOrAccounting() {
        final MockServerWebExchange exchange = exchange();
        exchange.getAttributes().put(Constants.METRICS_AI_TOKEN_BUDGET, (AiTokenBudgetObserver) (event, tokens) -> {
            throw new IllegalStateException("metrics unavailable");
        });
        StepVerifier.create(execute(exchange, response("{\"usage\":{\"completion_tokens\":6}}"))).expectComplete().verify(TIMEOUT);

        assertEquals(List.of(List.of("6", requestId())), settlements);
        assertTrue(completions.isEmpty());
    }

    private MockServerWebExchange exchange() {
        final MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/ai/chat").build());
        exchange.getAttributes().put(Constants.METRICS_AI_TOKEN_BUDGET, (AiTokenBudgetObserver) (event, tokens) -> observations.add(event.name() + ":" + tokens));
        return exchange;
    }

    private Mono<Void> execute(final MockServerWebExchange exchange, final ShenyuPluginChain chain) {
        return plugin.doExecute(exchange, chain, null, rule);
    }

    private ShenyuPluginChain response(final String body) {
        return exchange -> {
            exchange.getResponse().getHeaders().setContentType(MediaType.APPLICATION_JSON);
            return exchange.getResponse().writeWith(Mono.just(exchange.getResponse().bufferFactory().wrap(body.getBytes(StandardCharsets.UTF_8))));
        };
    }

    private String requestId() {
        return reservations.get(0).get(3);
    }
}
