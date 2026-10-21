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

package org.apache.shenyu.plugin.ai.token.limiter.budget;

import org.apache.shenyu.plugin.ai.token.limiter.budget.RedisTokenBudgetStore.CompletionResult;
import org.apache.shenyu.plugin.ai.token.limiter.budget.RedisTokenBudgetStore.ReservationResult;
import org.apache.shenyu.plugin.ai.token.limiter.budget.RedisTokenBudgetStore.SettlementResult;
import org.apache.shenyu.plugin.ai.token.limiter.budget.RedisTokenBudgetStore.UsageOutcome;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import org.springframework.data.redis.core.script.RedisScript;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.time.Duration;
import java.util.List;
import java.util.NoSuchElementException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * Tests Java validation, serialization arguments and script outcome handling.
 */
class RedisTokenBudgetStoreTest {

    private ReactiveRedisTemplate<String, String> redisTemplate;

    private RedisTokenBudgetStore store;

    @BeforeEach
    @SuppressWarnings("unchecked")
    void setUp() {
        redisTemplate = mock(ReactiveRedisTemplate.class);
        store = new RedisTokenBudgetStore(redisTemplate);
    }

    @ParameterizedTest
    @CsvSource({"0,REJECTED", "1,RESERVED", "2,ALREADY_RESERVED", "3,ALREADY_SETTLED", "4,ALREADY_RELEASED", "5,USAGE_UNKNOWN", "6,WINDOW_NOT_CURRENT"})
    void testMapsScriptResultsAndUsesStringArguments(final long code, final ReservationResult expected) {
        when(redisTemplate.execute(anyScript(), eq(List.of("window")), eq(List.of("100", "10", "60000", "request"))))
                .thenReturn(Flux.just(code));

        final Mono<ReservationResult> result = store.reserve("window", "request", 100L, 10L, Duration.ofMinutes(1));
        verifyNoInteractions(redisTemplate);

        StepVerifier.create(result).expectNext(expected).verifyComplete();
        verify(redisTemplate).execute(anyScript(), eq(List.of("window")), eq(List.of("100", "10", "60000", "request")));
    }

    @ParameterizedTest
    @CsvSource({"0,10,60000", "100,0,60000", "100,10,0", "-1,10,60000", "100,-1,60000", "100,10,-1",
        "9007199254740992,10,60000", "100,9007199254740992,60000", "100,10,9007199254740992"})
    void testRejectsInvalidNumbersBeforeRedis(final long limit, final long amount, final long ttlMillis) {
        StepVerifier.create(store.reserve("window", "request", limit, amount, Duration.ofMillis(ttlMillis)))
                .expectError(IllegalArgumentException.class).verify();

        verifyNoInteractions(redisTemplate);
    }

    @ParameterizedTest
    @ValueSource(strings = {"", " "})
    void testRejectsBlankIdentitiesBeforeRedis(final String identity) {
        StepVerifier.create(store.reserve(identity, "request", 100L, 10L, Duration.ofMinutes(1)))
                .expectError(IllegalArgumentException.class).verify();
        StepVerifier.create(store.reserve("window", identity, 100L, 10L, Duration.ofMinutes(1)))
                .expectError(IllegalArgumentException.class).verify();
        StepVerifier.create(store.settle(identity, "request", 6L)).expectError(IllegalArgumentException.class).verify();
        StepVerifier.create(store.settle("window", identity, 6L)).expectError(IllegalArgumentException.class).verify();
        StepVerifier.create(store.completeWithoutUsage(identity, "request", UsageOutcome.USAGE_UNKNOWN)).expectError(IllegalArgumentException.class).verify();
        StepVerifier.create(store.completeWithoutUsage("window", identity, UsageOutcome.USAGE_UNKNOWN)).expectError(IllegalArgumentException.class).verify();

        verifyNoInteractions(redisTemplate);
    }

    @Test
    void testRejectsSubMillisecondAndOverflowingTtl() {
        StepVerifier.create(store.reserve("window", "request", 100L, 10L, Duration.ofNanos(1)))
                .expectError(IllegalArgumentException.class).verify();
        StepVerifier.create(store.reserve("window", "request", 100L, 10L, Duration.ofSeconds(Long.MAX_VALUE)))
                .expectError(IllegalArgumentException.class).verify();

        verifyNoInteractions(redisTemplate);
    }

    @Test
    void testRejectsUnknownScriptOutcome() {
        when(redisTemplate.execute(anyScript(), anyList(), anyList())).thenReturn(Flux.just(7L));

        StepVerifier.create(store.reserve("window", "request", 100L, 10L, Duration.ofMinutes(1)))
                .expectError(IllegalStateException.class).verify();
        StepVerifier.create(store.settle("window", "request", 6L)).expectError(IllegalStateException.class).verify();
        StepVerifier.create(store.completeWithoutUsage("window", "request", UsageOutcome.USAGE_UNKNOWN)).expectError(IllegalStateException.class).verify();
    }

    @Test
    void testMissingScriptReplyIsAnError() {
        when(redisTemplate.execute(anyScript(), anyList(), anyList())).thenReturn(Flux.empty());

        StepVerifier.create(store.reserve("window", "request", 100L, 10L, Duration.ofMinutes(1)))
                .expectError(NoSuchElementException.class).verify();
        StepVerifier.create(store.settle("window", "request", 6L)).expectError(NoSuchElementException.class).verify();
        StepVerifier.create(store.completeWithoutUsage("window", "request", UsageOutcome.USAGE_UNKNOWN)).expectError(NoSuchElementException.class).verify();
        StepVerifier.create(store.currentWindow("scope", Duration.ofMinutes(1))).expectError(NoSuchElementException.class).verify();
    }

    @Test
    void testRedisFailureIsPropagated() {
        final IllegalStateException failure = new IllegalStateException("Redis unavailable");
        when(redisTemplate.execute(anyScript(), anyList(), anyList())).thenReturn(Flux.error(failure));

        StepVerifier.create(store.reserve("window", "request", 100L, 10L, Duration.ofMinutes(1)))
                .expectErrorMatches(error -> error == failure).verify();
        StepVerifier.create(store.settle("window", "request", 6L)).expectErrorMatches(error -> error == failure).verify();
        StepVerifier.create(store.completeWithoutUsage("window", "request", UsageOutcome.USAGE_UNKNOWN)).expectErrorMatches(error -> error == failure).verify();
        StepVerifier.create(store.currentWindow("scope", Duration.ofMinutes(1))).expectErrorMatches(error -> error == failure).verify();
    }

    @ParameterizedTest
    @CsvSource({"0,RESERVATION_NOT_FOUND", "1,SETTLED", "2,ALREADY_SETTLED", "3,WINDOW_NOT_FOUND"})
    void testMapsSettlementResultsAndSerializesZeroUsage(final long code, final SettlementResult expected) {
        when(redisTemplate.execute(anyScript(), eq(List.of("window")), eq(List.of("0", "request")))).thenReturn(Flux.just(code));

        final Mono<SettlementResult> result = store.settle("window", "request", 0L);
        verifyNoInteractions(redisTemplate);

        StepVerifier.create(result).expectNext(expected).verifyComplete();
        verify(redisTemplate).execute(anyScript(), eq(List.of("window")), eq(List.of("0", "request")));
    }

    @ParameterizedTest
    @ValueSource(longs = {-1L, 9_007_199_254_740_992L})
    void testRejectsInvalidActualUsageBeforeRedis(final long actual) {
        StepVerifier.create(store.settle("window", "request", actual)).expectError(IllegalArgumentException.class).verify();

        verifyNoInteractions(redisTemplate);
    }

    @Test
    void testRejectsNullInputsBeforeRedis() {
        StepVerifier.create(store.reserve(null, "request", 100L, 10L, Duration.ofMinutes(1)))
                .expectError(IllegalArgumentException.class).verify();
        StepVerifier.create(store.reserve("window", null, 100L, 10L, Duration.ofMinutes(1)))
                .expectError(IllegalArgumentException.class).verify();
        StepVerifier.create(store.reserve("window", "request", 100L, 10L, null))
                .expectError(IllegalArgumentException.class).verify();
        StepVerifier.create(store.settle(null, "request", 6L)).expectError(IllegalArgumentException.class).verify();
        StepVerifier.create(store.settle("window", null, 6L)).expectError(IllegalArgumentException.class).verify();
        StepVerifier.create(store.completeWithoutUsage(null, "request", UsageOutcome.USAGE_UNKNOWN)).expectError(IllegalArgumentException.class).verify();
        StepVerifier.create(store.completeWithoutUsage("window", null, UsageOutcome.USAGE_UNKNOWN)).expectError(IllegalArgumentException.class).verify();
        StepVerifier.create(store.completeWithoutUsage("window", "request", null)).expectError(IllegalArgumentException.class).verify();

        verifyNoInteractions(redisTemplate);
    }

    @ParameterizedTest
    @CsvSource({"0,RESERVATION_NOT_FOUND,NOT_FORWARDED", "1,RELEASED,NOT_FORWARDED", "2,USAGE_HELD,USAGE_UNKNOWN",
        "3,ALREADY_SETTLED,USAGE_UNKNOWN", "4,WINDOW_NOT_FOUND,USAGE_UNKNOWN"})
    void testMapsCompletionResultsAndSerializesOutcome(final long code, final CompletionResult expected, final UsageOutcome outcome) {
        when(redisTemplate.execute(anyScript(), eq(List.of("window")), eq(List.of("request", outcome.name())))).thenReturn(Flux.just(code));

        final Mono<CompletionResult> result = store.completeWithoutUsage("window", "request", outcome);
        verifyNoInteractions(redisTemplate);

        StepVerifier.create(result).expectNext(expected).verifyComplete();
        verify(redisTemplate).execute(anyScript(), eq(List.of("window")), eq(List.of("request", outcome.name())));
    }

    @Test
    void testWindowSelectionUsesRedisClockAndDoesNotWriteBudgetState() {
        final List<String> keys = List.of(TokenBudgetWindow.scopePrefix("scope"));
        when(redisTemplate.execute(anyScript(), eq(keys), eq(List.of()))).thenReturn(Flux.just(61_234L));

        final Mono<TokenBudgetWindow> result = store.currentWindow("scope", Duration.ofMinutes(1));
        verifyNoInteractions(redisTemplate);

        StepVerifier.create(result).assertNext(window -> {
            assertEquals(60_000L, window.getStartMillis());
            assertEquals(120_000L, window.getEndMillis());
        }).verifyComplete();
        verify(redisTemplate).execute(anyScript(), eq(keys), eq(List.of()));
    }

    @Test
    void testFixedWindowReservationSendsBoundariesAndReportsStaleWindow() {
        final TokenBudgetWindow window = TokenBudgetWindow.at("scope", 61_234L, 60_000L);
        final List<String> args = List.of("100", "10", "60000", "request", "60000", "120000");
        when(redisTemplate.execute(anyScript(), eq(List.of(window.getKey())), eq(args))).thenReturn(Flux.just(6L));

        final Mono<ReservationResult> result = store.reserve(window, "request", 100L, 10L);
        verifyNoInteractions(redisTemplate);

        StepVerifier.create(result).expectNext(ReservationResult.WINDOW_NOT_CURRENT).verifyComplete();
        verify(redisTemplate).execute(anyScript(), eq(List.of(window.getKey())), eq(args));
    }

    @Test
    void testInvalidWindowSelectionAndReservationFailBeforeRedis() {
        StepVerifier.create(store.currentWindow(null, Duration.ofMinutes(1))).expectError(IllegalArgumentException.class).verify();
        StepVerifier.create(store.currentWindow(" ", Duration.ofMinutes(1))).expectError(IllegalArgumentException.class).verify();
        StepVerifier.create(store.currentWindow("scope", null)).expectError(IllegalArgumentException.class).verify();
        StepVerifier.create(store.currentWindow("scope", Duration.ZERO)).expectError(IllegalArgumentException.class).verify();
        StepVerifier.create(store.reserve(null, "request", 100L, 10L)).expectError(IllegalArgumentException.class).verify();
        final TokenBudgetWindow window = TokenBudgetWindow.at("scope", 61_234L, 60_000L);
        StepVerifier.create(store.reserve(window, " ", 100L, 10L)).expectError(IllegalArgumentException.class).verify();
        StepVerifier.create(store.reserve(window, "request", 0L, 10L)).expectError(IllegalArgumentException.class).verify();
        StepVerifier.create(store.reserve(window, "request", 100L, 9_007_199_254_740_992L)).expectError(IllegalArgumentException.class).verify();

        verifyNoInteractions(redisTemplate);
    }

    private RedisScript<Long> anyScript() {
        return any();
    }
}
