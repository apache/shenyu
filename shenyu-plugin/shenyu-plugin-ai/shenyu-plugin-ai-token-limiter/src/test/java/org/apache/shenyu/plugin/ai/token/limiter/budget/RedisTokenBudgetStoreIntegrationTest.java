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

import org.apache.shenyu.infra.redis.ShenyuReactiveRedisTemplate;
import org.apache.shenyu.infra.redis.serializer.ShenyuRedisSerializationContext;
import org.apache.shenyu.plugin.ai.token.limiter.budget.RedisTokenBudgetStore.CompletionResult;
import org.apache.shenyu.plugin.ai.token.limiter.budget.RedisTokenBudgetStore.ReservationResult;
import org.apache.shenyu.plugin.ai.token.limiter.budget.RedisTokenBudgetStore.SettlementResult;
import org.apache.shenyu.plugin.ai.token.limiter.budget.RedisTokenBudgetStore.UsageOutcome;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.junit.jupiter.api.condition.EnabledIfSystemProperty;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.time.Duration;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Real Redis reservation and settlement tests, enabled with -Dshenyu.test.redis.port.
 * Uses two independent connection factories and deletes only its own UUID keys.
 */
@EnabledIfSystemProperty(named = "shenyu.test.redis.port", matches = "\\d+")
class RedisTokenBudgetStoreIntegrationTest {

    private static final Duration TIMEOUT = Duration.ofSeconds(5);

    private static final Duration WINDOW_TTL = Duration.ofMinutes(1);

    private static LettuceConnectionFactory firstFactory;

    private static LettuceConnectionFactory secondFactory;

    private static ReactiveRedisTemplate<String, String> firstTemplate;

    private static ReactiveRedisTemplate<String, String> secondTemplate;

    private static RedisTokenBudgetStore firstStore;

    private static RedisTokenBudgetStore secondStore;

    private final List<String> keys = new ArrayList<>();

    private final String scopeId = UUID.randomUUID().toString();

    @BeforeAll
    static void setUpRedisClients() {
        final int port = Integer.parseInt(System.getProperty("shenyu.test.redis.port"));
        firstFactory = new LettuceConnectionFactory("127.0.0.1", port);
        firstFactory.afterPropertiesSet();
        secondFactory = new LettuceConnectionFactory("127.0.0.1", port);
        secondFactory.afterPropertiesSet();
        firstTemplate = new ShenyuReactiveRedisTemplate<>(firstFactory, ShenyuRedisSerializationContext.stringSerializationContext());
        secondTemplate = new ShenyuReactiveRedisTemplate<>(secondFactory, ShenyuRedisSerializationContext.stringSerializationContext());
        firstStore = new RedisTokenBudgetStore(firstTemplate);
        secondStore = new RedisTokenBudgetStore(secondTemplate);
    }

    @AfterEach
    void deleteOwnedKeys() {
        if (!keys.isEmpty()) {
            firstTemplate.delete(Flux.fromIterable(keys)).block(TIMEOUT);
        }
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
    void testConcurrentClientsCannotReserveTheSameRemainingBudget() {
        final String key = newKey();
        seed(key, 80L);

        final List<ReservationResult> results = Flux.range(0, 3).flatMap(index -> {
            final RedisTokenBudgetStore store = index % 2 == 0 ? firstStore : secondStore;
            return store.reserve(key, "request-" + index, 100L, 10L, WINDOW_TTL);
        }, 3).collectList().block(TIMEOUT);

        assertNotNull(results);
        assertEquals(2, Collections.frequency(results, ReservationResult.RESERVED));
        assertEquals(1, Collections.frequency(results, ReservationResult.REJECTED));
        assertEquals("80", field(key, "used"));
        assertEquals("20", field(key, "reserved"));
        assertEquals(4L, firstTemplate.opsForHash().size(key).block(TIMEOUT));
    }

    @Test
    void testConcurrentRetriesReserveOnlyOnce() {
        final String key = newKey();
        final List<ReservationResult> results = Mono.zip(
                firstStore.reserve(key, "same-request", 100L, 10L, WINDOW_TTL),
                secondStore.reserve(key, "same-request", 100L, 10L, WINDOW_TTL))
                .map(pair -> List.of(pair.getT1(), pair.getT2())).block(TIMEOUT);

        assertNotNull(results);
        assertEquals(1, Collections.frequency(results, ReservationResult.RESERVED));
        assertEquals(1, Collections.frequency(results, ReservationResult.ALREADY_RESERVED));
        assertEquals("10", field(key, "reserved"));
        assertEquals("10", field(key, "r:same-request"));
    }

    @Test
    void testRejectingAnOversizedRequestDoesNotCreateAWindow() {
        final String key = newKey();

        assertEquals(ReservationResult.REJECTED, firstStore.reserve(key, "request", 100L, 101L, WINDOW_TTL).block(TIMEOUT));
        assertFalse(Boolean.TRUE.equals(firstTemplate.hasKey(key).block(TIMEOUT)));
    }

    @Test
    void testNewWindowHasAnExpiry() {
        final String key = newKey();

        assertEquals(ReservationResult.RESERVED, firstStore.reserve(key, "request", 100L, 10L, WINDOW_TTL).block(TIMEOUT));

        final Duration ttl = firstTemplate.getExpire(key).block(TIMEOUT);
        assertNotNull(ttl);
        assertTrue(ttl.compareTo(Duration.ZERO) > 0);
        assertTrue(ttl.compareTo(WINDOW_TTL) <= 0);
    }

    @Test
    void testAdmissionAndRetryDoNotExtendAnExistingWindow() {
        final String key = newKey();
        seed(key, 80L);
        final Duration before = firstTemplate.getExpire(key).block(TIMEOUT);

        assertEquals(ReservationResult.RESERVED, firstStore.reserve(key, "request", 100L, 10L, Duration.ofHours(1)).block(TIMEOUT));
        assertEquals(ReservationResult.ALREADY_RESERVED, secondStore.reserve(key, "request", 100L, 10L, Duration.ofHours(1)).block(TIMEOUT));

        final Duration after = firstTemplate.getExpire(key).block(TIMEOUT);
        assertNotNull(before);
        assertNotNull(after);
        assertTrue(after.compareTo(Duration.ZERO) > 0);
        assertTrue(after.compareTo(before) <= 0);
    }

    @Test
    void testDifferentWindowsRemainIndependent() {
        final String first = newKey();
        final String second = newKey();
        seed(first, 80L);
        seed(second, 80L);

        assertEquals(ReservationResult.RESERVED, firstStore.reserve(first, "request", 100L, 20L, WINDOW_TTL).block(TIMEOUT));
        assertEquals(ReservationResult.RESERVED, secondStore.reserve(second, "request", 100L, 20L, WINDOW_TTL).block(TIMEOUT));
        assertEquals("20", field(first, "reserved"));
        assertEquals("20", field(second, "reserved"));
    }

    @Test
    void testRequestIdCannotBeReusedWithADifferentAmount() {
        final String key = newKey();
        assertEquals(ReservationResult.RESERVED, firstStore.reserve(key, "request", 100L, 10L, WINDOW_TTL).block(TIMEOUT));

        StepVerifier.create(secondStore.reserve(key, "request", 100L, 11L, WINDOW_TTL)).expectError().verify(TIMEOUT);

        assertEquals("10", field(key, "reserved"));
        assertEquals("10", field(key, "r:request"));
    }

    @Test
    void testMissingExpiryIsAnError() {
        final String key = newKey();
        firstTemplate.<String, String>opsForHash().putAll(key, Map.of("used", "80", "reserved", "0")).block(TIMEOUT);

        StepVerifier.create(firstStore.reserve(key, "request", 100L, 10L, WINDOW_TTL)).expectError().verify(TIMEOUT);

        assertEquals("0", field(key, "reserved"));
    }

    @Test
    void testInvalidStoredCounterIsAnError() {
        final String key = newKey();
        seed(key, 80L);
        firstTemplate.<String, String>opsForHash().put(key, "used", "invalid").block(TIMEOUT);

        StepVerifier.create(firstStore.reserve(key, "request", 100L, 10L, WINDOW_TTL)).expectError().verify(TIMEOUT);

        assertEquals("0", field(key, "reserved"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"used", "reserved"})
    void testMissingCounterCannotResetAnExistingBudget(final String missing) {
        final String key = newKey();
        seed(key, 80L);
        assertEquals(ReservationResult.RESERVED, firstStore.reserve(key, "existing", 100L, 10L, WINDOW_TTL).block(TIMEOUT));
        firstTemplate.<String, String>opsForHash().remove(key, missing).block(TIMEOUT);
        final Map<String, String> before = firstTemplate.<String, String>opsForHash().entries(key).collectMap(Map.Entry::getKey, Map.Entry::getValue).block(TIMEOUT);

        StepVerifier.create(secondStore.reserve(key, "new-request", 100L, 10L, WINDOW_TTL)).expectError().verify(TIMEOUT);

        assertEquals(before, firstTemplate.<String, String>opsForHash().entries(key).collectMap(Map.Entry::getKey, Map.Entry::getValue).block(TIMEOUT));
    }

    @Test
    void testLargestExactLuaIntegerIsPreserved() {
        final String key = newKey();
        final long maximum = 9_007_199_254_740_991L;

        assertEquals(ReservationResult.RESERVED, firstStore.reserve(key, "request", maximum, maximum, WINDOW_TTL).block(TIMEOUT));
        assertEquals(Long.toString(maximum), field(key, "reserved"));
        assertEquals(ReservationResult.REJECTED, secondStore.reserve(key, "extra", maximum, 1L, WINDOW_TTL).block(TIMEOUT));
    }

    @ParameterizedTest
    @CsvSource({"6,86", "0,80", "30,110"})
    void testSettlementReleasesReservationAndRecordsActualUsage(final long actual, final String expectedUsed) {
        final String key = newKey();
        seed(key, 80L);
        assertEquals(ReservationResult.RESERVED, firstStore.reserve(key, "request", 100L, 10L, WINDOW_TTL).block(TIMEOUT));
        final Duration before = firstTemplate.getExpire(key).block(TIMEOUT);

        assertEquals(SettlementResult.SETTLED, firstStore.settle(key, "request", actual).block(TIMEOUT));
        assertEquals(SettlementResult.ALREADY_SETTLED, secondStore.settle(key, "request", actual).block(TIMEOUT));
        assertEquals(ReservationResult.ALREADY_SETTLED, secondStore.reserve(key, "request", 100L, 10L, Duration.ofHours(1)).block(TIMEOUT));

        assertEquals(expectedUsed, field(key, "used"));
        assertEquals("0", field(key, "reserved"));
        assertEquals("10", field(key, "r:request"));
        assertEquals(Long.toString(actual), field(key, "s:request"));
        final Duration after = firstTemplate.getExpire(key).block(TIMEOUT);
        assertNotNull(before);
        assertNotNull(after);
        assertTrue(after.compareTo(Duration.ZERO) > 0);
        assertTrue(after.compareTo(before) <= 0);
        final ReservationResult next = actual > 20L ? ReservationResult.REJECTED : ReservationResult.RESERVED;
        assertEquals(next, firstStore.reserve(key, "next", 100L, 10L, WINDOW_TTL).block(TIMEOUT));
    }

    @Test
    void testConcurrentDuplicateSettlementsChargeOnlyOnce() {
        final String key = newKey();
        seed(key, 80L);
        assertEquals(ReservationResult.RESERVED, firstStore.reserve(key, "request", 100L, 10L, WINDOW_TTL).block(TIMEOUT));

        final List<SettlementResult> results = Mono.zip(firstStore.settle(key, "request", 6L), secondStore.settle(key, "request", 6L))
                .map(pair -> List.of(pair.getT1(), pair.getT2())).block(TIMEOUT);

        assertNotNull(results);
        assertEquals(1, Collections.frequency(results, SettlementResult.SETTLED));
        assertEquals(1, Collections.frequency(results, SettlementResult.ALREADY_SETTLED));
        assertEquals("86", field(key, "used"));
        assertEquals("0", field(key, "reserved"));
    }

    @Test
    void testConcurrentDifferentSettlementsDoNotLoseUsage() {
        final String key = newKey();
        seed(key, 80L);
        assertEquals(ReservationResult.RESERVED, firstStore.reserve(key, "first", 100L, 10L, WINDOW_TTL).block(TIMEOUT));
        assertEquals(ReservationResult.RESERVED, secondStore.reserve(key, "second", 100L, 10L, WINDOW_TTL).block(TIMEOUT));

        final List<SettlementResult> results = Mono.zip(firstStore.settle(key, "first", 6L), secondStore.settle(key, "second", 4L))
                .map(pair -> List.of(pair.getT1(), pair.getT2())).block(TIMEOUT);

        assertEquals(List.of(SettlementResult.SETTLED, SettlementResult.SETTLED), results);
        assertEquals("90", field(key, "used"));
        assertEquals("0", field(key, "reserved"));
    }

    @Test
    void testConflictingSettlementAndReservationRetriesFailWithoutChangingState() {
        final String key = newKey();
        seed(key, 80L);
        assertEquals(ReservationResult.RESERVED, firstStore.reserve(key, "request", 100L, 10L, WINDOW_TTL).block(TIMEOUT));
        assertEquals(SettlementResult.SETTLED, firstStore.settle(key, "request", 6L).block(TIMEOUT));

        StepVerifier.create(secondStore.settle(key, "request", 7L)).expectError().verify(TIMEOUT);
        StepVerifier.create(secondStore.reserve(key, "request", 100L, 11L, WINDOW_TTL)).expectError().verify(TIMEOUT);

        assertEquals("86", field(key, "used"));
        assertEquals("0", field(key, "reserved"));
        assertEquals("6", field(key, "s:request"));
    }

    @Test
    void testLateSettlementCannotRecreateExpiredWindowOrChangeTheNextWindow() {
        final String oldWindow = newKey();
        final String nextWindow = newKey();
        assertEquals(ReservationResult.RESERVED, firstStore.reserve(oldWindow, "request", 100L, 10L, WINDOW_TTL).block(TIMEOUT));
        assertTrue(Boolean.TRUE.equals(firstTemplate.expire(oldWindow, Duration.ZERO).block(TIMEOUT)));
        seed(nextWindow, 20L);
        assertEquals(ReservationResult.RESERVED, secondStore.reserve(nextWindow, "request", 100L, 10L, WINDOW_TTL).block(TIMEOUT));

        assertEquals(SettlementResult.WINDOW_NOT_FOUND, firstStore.settle(oldWindow, "request", 6L).block(TIMEOUT));

        assertFalse(Boolean.TRUE.equals(firstTemplate.hasKey(oldWindow).block(TIMEOUT)));
        assertEquals("20", field(nextWindow, "used"));
        assertEquals("10", field(nextWindow, "reserved"));
        assertNull(field(nextWindow, "s:request"));
    }

    @Test
    void testUnknownReservationDoesNotModifyLiveWindow() {
        final String key = newKey();
        seed(key, 80L);

        assertEquals(SettlementResult.RESERVATION_NOT_FOUND, firstStore.settle(key, "missing", 6L).block(TIMEOUT));

        assertEquals("80", field(key, "used"));
        assertEquals("0", field(key, "reserved"));
        assertEquals(2L, firstTemplate.opsForHash().size(key).block(TIMEOUT));
    }

    @Test
    void testInvalidReservedTotalDoesNotPartiallySettle() {
        final String key = newKey();
        seed(key, 80L);
        assertEquals(ReservationResult.RESERVED, firstStore.reserve(key, "request", 100L, 10L, WINDOW_TTL).block(TIMEOUT));
        firstTemplate.<String, String>opsForHash().put(key, "reserved", "9").block(TIMEOUT);

        StepVerifier.create(firstStore.settle(key, "request", 6L)).expectError().verify(TIMEOUT);

        assertEquals("80", field(key, "used"));
        assertEquals("9", field(key, "reserved"));
        assertNull(field(key, "s:request"));
    }

    @Test
    void testSettlementCannotRoundUsageBeyondExactLuaIntegerRange() {
        final String key = newKey();
        final long maximum = 9_007_199_254_740_991L;
        seed(key, maximum - 2L);
        assertEquals(ReservationResult.RESERVED, firstStore.reserve(key, "request", maximum, 1L, WINDOW_TTL).block(TIMEOUT));

        StepVerifier.create(firstStore.settle(key, "request", 3L)).expectError().verify(TIMEOUT);

        assertEquals(Long.toString(maximum - 2L), field(key, "used"));
        assertEquals("1", field(key, "reserved"));
        assertNull(field(key, "s:request"));
    }

    @Test
    void testConcurrentUnforwardedCompletionsReleaseOnlyOnce() {
        final String key = newKey();
        seed(key, 80L);
        assertEquals(ReservationResult.RESERVED, firstStore.reserve(key, "request", 100L, 20L, WINDOW_TTL).block(TIMEOUT));
        final Duration before = firstTemplate.getExpire(key).block(TIMEOUT);

        final List<CompletionResult> results = Mono.zip(
                firstStore.completeWithoutUsage(key, "request", UsageOutcome.NOT_FORWARDED),
                secondStore.completeWithoutUsage(key, "request", UsageOutcome.NOT_FORWARDED))
                .map(pair -> List.of(pair.getT1(), pair.getT2())).block(TIMEOUT);

        assertEquals(List.of(CompletionResult.RELEASED, CompletionResult.RELEASED), results);
        assertEquals(CompletionResult.RELEASED, firstStore.completeWithoutUsage(key, "request", UsageOutcome.USAGE_UNKNOWN).block(TIMEOUT));
        assertEquals(ReservationResult.ALREADY_RELEASED, firstStore.reserve(key, "request", 100L, 20L, WINDOW_TTL).block(TIMEOUT));
        StepVerifier.create(secondStore.settle(key, "request", 6L)).expectError().verify(TIMEOUT);
        assertEquals("80", field(key, "used"));
        assertEquals("0", field(key, "reserved"));
        assertEquals("0", field(key, "s:request"));
        assertEquals("NOT_FORWARDED", field(key, "f:request"));
        assertTtlNotExtended(key, before);
        assertEquals(ReservationResult.RESERVED, secondStore.reserve(key, "next", 100L, 20L, WINDOW_TTL).block(TIMEOUT));
    }

    @Test
    void testConcurrentUnknownCompletionsRetainBudgetAndBlockReadmission() {
        final String key = newKey();
        seed(key, 80L);
        assertEquals(ReservationResult.RESERVED, firstStore.reserve(key, "request", 100L, 20L, WINDOW_TTL).block(TIMEOUT));
        final Duration before = firstTemplate.getExpire(key).block(TIMEOUT);

        final List<CompletionResult> results = Mono.zip(
                firstStore.completeWithoutUsage(key, "request", UsageOutcome.USAGE_UNKNOWN),
                secondStore.completeWithoutUsage(key, "request", UsageOutcome.USAGE_UNKNOWN))
                .map(pair -> List.of(pair.getT1(), pair.getT2())).block(TIMEOUT);

        assertEquals(List.of(CompletionResult.USAGE_HELD, CompletionResult.USAGE_HELD), results);
        assertEquals(ReservationResult.USAGE_UNKNOWN, secondStore.reserve(key, "request", 100L, 20L, WINDOW_TTL).block(TIMEOUT));
        assertEquals(ReservationResult.REJECTED, secondStore.reserve(key, "next", 100L, 1L, WINDOW_TTL).block(TIMEOUT));
        StepVerifier.create(firstStore.completeWithoutUsage(key, "request", UsageOutcome.NOT_FORWARDED)).expectError().verify(TIMEOUT);
        assertEquals("80", field(key, "used"));
        assertEquals("20", field(key, "reserved"));
        assertEquals("USAGE_UNKNOWN", field(key, "f:request"));
        assertNull(field(key, "s:request"));
        assertTtlNotExtended(key, before);
    }

    @ParameterizedTest
    @ValueSource(longs = {0L, 6L})
    void testUnknownUsageCanLaterBeSettledWithReliableUsage(final long actual) {
        final String key = newKey();
        seed(key, 80L);
        assertEquals(ReservationResult.RESERVED, firstStore.reserve(key, "request", 100L, 20L, WINDOW_TTL).block(TIMEOUT));
        assertEquals(CompletionResult.USAGE_HELD, firstStore.completeWithoutUsage(key, "request", UsageOutcome.USAGE_UNKNOWN).block(TIMEOUT));

        assertEquals(SettlementResult.SETTLED, secondStore.settle(key, "request", actual).block(TIMEOUT));
        assertEquals(SettlementResult.ALREADY_SETTLED, firstStore.settle(key, "request", actual).block(TIMEOUT));
        assertEquals(CompletionResult.ALREADY_SETTLED, firstStore.completeWithoutUsage(key, "request", UsageOutcome.USAGE_UNKNOWN).block(TIMEOUT));
        assertEquals(ReservationResult.ALREADY_SETTLED, firstStore.reserve(key, "request", 100L, 20L, WINDOW_TTL).block(TIMEOUT));
        assertEquals(Long.toString(80L + actual), field(key, "used"));
        assertEquals("0", field(key, "reserved"));
    }

    @Test
    void testConcurrentSettlementAndUnknownCompletionKeepActualUsage() {
        final String key = newKey();
        seed(key, 80L);
        assertEquals(ReservationResult.RESERVED, firstStore.reserve(key, "request", 100L, 10L, WINDOW_TTL).block(TIMEOUT));

        Mono.zip(firstStore.settle(key, "request", 6L), secondStore.completeWithoutUsage(key, "request", UsageOutcome.USAGE_UNKNOWN))
                .doOnNext(pair -> {
                    assertEquals(SettlementResult.SETTLED, pair.getT1());
                    assertTrue(pair.getT2() == CompletionResult.USAGE_HELD || pair.getT2() == CompletionResult.ALREADY_SETTLED);
                }).block(TIMEOUT);

        assertEquals("86", field(key, "used"));
        assertEquals("0", field(key, "reserved"));
        assertEquals(ReservationResult.ALREADY_SETTLED, firstStore.reserve(key, "request", 100L, 10L, WINDOW_TTL).block(TIMEOUT));
    }

    @ParameterizedTest
    @EnumSource(UsageOutcome.class)
    void testCompletionCannotReleaseAlreadySettledUsage(final UsageOutcome outcome) {
        final String key = newKey();
        seed(key, 80L);
        assertEquals(ReservationResult.RESERVED, firstStore.reserve(key, "request", 100L, 10L, WINDOW_TTL).block(TIMEOUT));
        assertEquals(SettlementResult.SETTLED, firstStore.settle(key, "request", 6L).block(TIMEOUT));

        assertEquals(CompletionResult.ALREADY_SETTLED, secondStore.completeWithoutUsage(key, "request", outcome).block(TIMEOUT));

        assertEquals("86", field(key, "used"));
        assertEquals("0", field(key, "reserved"));
        assertNull(field(key, "f:request"));
    }

    @ParameterizedTest
    @EnumSource(UsageOutcome.class)
    void testUnknownReservationsExpireAndLateCompletionDoesNotTouchNextWindow(final UsageOutcome outcome) {
        final String oldWindow = newKey();
        final String nextWindow = newKey();
        assertEquals(ReservationResult.RESERVED, firstStore.reserve(oldWindow, "request", 100L, 10L, WINDOW_TTL).block(TIMEOUT));
        assertEquals(CompletionResult.USAGE_HELD, firstStore.completeWithoutUsage(oldWindow, "request", UsageOutcome.USAGE_UNKNOWN).block(TIMEOUT));
        assertTrue(Boolean.TRUE.equals(firstTemplate.expire(oldWindow, Duration.ZERO).block(TIMEOUT)));
        assertEquals(ReservationResult.RESERVED, secondStore.reserve(nextWindow, "request", 100L, 10L, WINDOW_TTL).block(TIMEOUT));

        assertEquals(CompletionResult.WINDOW_NOT_FOUND, firstStore.completeWithoutUsage(oldWindow, "request", outcome).block(TIMEOUT));

        assertFalse(Boolean.TRUE.equals(firstTemplate.hasKey(oldWindow).block(TIMEOUT)));
        assertEquals("0", field(nextWindow, "used"));
        assertEquals("10", field(nextWindow, "reserved"));
        assertNull(field(nextWindow, "f:request"));
    }

    @ParameterizedTest
    @EnumSource(UsageOutcome.class)
    void testCompletionOfMissingReservationDoesNotChangeLiveWindow(final UsageOutcome outcome) {
        final String key = newKey();
        seed(key, 80L);

        assertEquals(CompletionResult.RESERVATION_NOT_FOUND, firstStore.completeWithoutUsage(key, "missing", outcome).block(TIMEOUT));

        assertEquals("80", field(key, "used"));
        assertEquals("0", field(key, "reserved"));
        assertEquals(2L, firstTemplate.opsForHash().size(key).block(TIMEOUT));
    }

    @ParameterizedTest
    @CsvSource({"reserved,9", "used,invalid", "f:request,invalid"})
    void testInvalidCompletionStateCannotPartiallyReleaseOrHold(final String field, final String value) {
        final String key = newKey();
        seed(key, 80L);
        assertEquals(ReservationResult.RESERVED, firstStore.reserve(key, "request", 100L, 10L, WINDOW_TTL).block(TIMEOUT));
        firstTemplate.<String, String>opsForHash().put(key, field, value).block(TIMEOUT);
        final Map<String, String> before = firstTemplate.<String, String>opsForHash().entries(key).collectMap(Map.Entry::getKey, Map.Entry::getValue).block(TIMEOUT);

        StepVerifier.create(firstStore.completeWithoutUsage(key, "request", UsageOutcome.NOT_FORWARDED)).expectError().verify(TIMEOUT);
        StepVerifier.create(secondStore.completeWithoutUsage(key, "request", UsageOutcome.USAGE_UNKNOWN)).expectError().verify(TIMEOUT);

        assertEquals(before, firstTemplate.<String, String>opsForHash().entries(key).collectMap(Map.Entry::getKey, Map.Entry::getValue).block(TIMEOUT));
    }

    @Test
    void testCompletionRefusesWindowWithoutExpiry() {
        final String key = newKey();
        assertEquals(ReservationResult.RESERVED, firstStore.reserve(key, "request", 100L, 10L, WINDOW_TTL).block(TIMEOUT));
        assertTrue(Boolean.TRUE.equals(firstTemplate.persist(key).block(TIMEOUT)));

        StepVerifier.create(firstStore.completeWithoutUsage(key, "request", UsageOutcome.NOT_FORWARDED)).expectError().verify(TIMEOUT);
        StepVerifier.create(secondStore.completeWithoutUsage(key, "request", UsageOutcome.USAGE_UNKNOWN)).expectError().verify(TIMEOUT);

        assertEquals("10", field(key, "reserved"));
        assertNull(field(key, "s:request"));
        assertNull(field(key, "f:request"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"invalid", "NOT_FORWARDED", "orphan"})
    void testMalformedCompletionMarkersAreRejectedAcrossBudgetOperations(final String marker) {
        final String key = newKey();
        assertEquals(ReservationResult.RESERVED, firstStore.reserve(key, "request", 100L, 10L, WINDOW_TTL).block(TIMEOUT));
        if ("orphan".equals(marker)) {
            firstTemplate.<String, String>opsForHash().remove(key, "r:request").block(TIMEOUT);
        }
        firstTemplate.<String, String>opsForHash().put(key, "f:request", "orphan".equals(marker) ? "USAGE_UNKNOWN" : marker).block(TIMEOUT);
        final Map<String, String> before = firstTemplate.<String, String>opsForHash().entries(key).collectMap(Map.Entry::getKey, Map.Entry::getValue).block(TIMEOUT);

        StepVerifier.create(firstStore.reserve(key, "request", 100L, 10L, WINDOW_TTL)).expectError().verify(TIMEOUT);
        StepVerifier.create(secondStore.settle(key, "request", 6L)).expectError().verify(TIMEOUT);
        StepVerifier.create(firstStore.completeWithoutUsage(key, "request", UsageOutcome.USAGE_UNKNOWN)).expectError().verify(TIMEOUT);

        assertEquals(before, firstTemplate.<String, String>opsForHash().entries(key).collectMap(Map.Entry::getKey, Map.Entry::getValue).block(TIMEOUT));
    }

    @Test
    void testIndependentClientsSelectTheSameWindowAndShareAdmission() {
        final Duration windowSize = Duration.ofMillis(TokenBudgetWindow.MAX_EXACT_INTEGER);
        final TokenBudgetWindow first = trackedWindow(firstStore, windowSize);
        final TokenBudgetWindow second = trackedWindow(secondStore, windowSize);
        assertEquals(first.getKey(), second.getKey());
        assertFalse(Boolean.TRUE.equals(firstTemplate.hasKey(first.getKey()).block(TIMEOUT)));

        final List<ReservationResult> results = Mono.zip(firstStore.reserve(first, "first", 10L, 10L), secondStore.reserve(second, "second", 10L, 10L))
                .map(pair -> List.of(pair.getT1(), pair.getT2())).block(TIMEOUT);

        assertNotNull(results);
        assertEquals(1, Collections.frequency(results, ReservationResult.RESERVED));
        assertEquals(1, Collections.frequency(results, ReservationResult.REJECTED));
        assertEquals("10", field(first.getKey(), "reserved"));
    }

    @Test
    void testFixedWindowExpiryMatchesItsAbsoluteEnd() {
        final TokenBudgetWindow window = trackedWindow(firstStore, Duration.ofMillis(TokenBudgetWindow.MAX_EXACT_INTEGER));
        final long before = redisTimeMillis();

        assertEquals(ReservationResult.RESERVED, firstStore.reserve(window, "request", 100L, 10L).block(TIMEOUT));

        final Duration ttl = firstTemplate.getExpire(window.getKey()).block(TIMEOUT);
        final long after = redisTimeMillis();
        assertNotNull(ttl);
        assertTrue(ttl.toMillis() <= window.getEndMillis() - before);
        assertTrue(ttl.toMillis() >= window.getEndMillis() - after);
        assertEquals(ReservationResult.ALREADY_RESERVED, secondStore.reserve(window, "request", 100L, 10L).block(TIMEOUT));
        assertTtlNotExtended(window.getKey(), ttl);
    }

    @Test
    void testStaleWindowAdmissionCannotRecreateAnOldHash() {
        final TokenBudgetWindow stale = TokenBudgetWindow.at(scopeId, 0L, 60_000L);
        keys.add(stale.getKey());

        assertEquals(ReservationResult.WINDOW_NOT_CURRENT, firstStore.reserve(stale, "request", 100L, 10L).block(TIMEOUT));
        assertFalse(Boolean.TRUE.equals(firstTemplate.hasKey(stale.getKey()).block(TIMEOUT)));

        final TokenBudgetWindow current = trackedWindow(secondStore, Duration.ofMillis(TokenBudgetWindow.MAX_EXACT_INTEGER));
        assertEquals(ReservationResult.RESERVED, secondStore.reserve(current, "request", 100L, 10L).block(TIMEOUT));
        assertEquals(ReservationResult.WINDOW_NOT_CURRENT, firstStore.reserve(stale, "request", 100L, 10L).block(TIMEOUT));
        assertEquals(SettlementResult.WINDOW_NOT_FOUND, firstStore.settle(stale.getKey(), "request", 6L).block(TIMEOUT));
        assertEquals("0", field(current.getKey(), "used"));
        assertEquals("10", field(current.getKey(), "reserved"));
    }

    @Test
    void testFutureWindowAdmissionCannotAllocateBudgetEarly() {
        final TokenBudgetWindow future = TokenBudgetWindow.at(scopeId, TokenBudgetWindow.MAX_EXACT_INTEGER - 1L, 1L);
        keys.add(future.getKey());

        assertEquals(ReservationResult.WINDOW_NOT_CURRENT, firstStore.reserve(future, "request", 100L, 10L).block(TIMEOUT));
        assertFalse(Boolean.TRUE.equals(firstTemplate.hasKey(future.getKey()).block(TIMEOUT)));
    }

    private TokenBudgetWindow trackedWindow(final RedisTokenBudgetStore store, final Duration windowSize) {
        final TokenBudgetWindow window = store.currentWindow(scopeId, windowSize).block(TIMEOUT);
        assertNotNull(window);
        keys.add(window.getKey());
        return window;
    }

    private long redisTimeMillis() {
        final TokenBudgetWindow clock = firstStore.currentWindow(scopeId, Duration.ofMillis(1L)).block(TIMEOUT);
        assertNotNull(clock);
        return clock.getStartMillis();
    }

    private void assertTtlNotExtended(final String key, final Duration before) {
        final Duration after = firstTemplate.getExpire(key).block(TIMEOUT);
        assertNotNull(before);
        assertNotNull(after);
        assertTrue(after.compareTo(Duration.ZERO) > 0);
        assertTrue(after.compareTo(before) <= 0);
    }

    private String newKey() {
        final String key = "SHENYU:AI:TOKENLIMIT:RESERVATION:{test-" + scopeId + "}:window-" + keys.size();
        keys.add(key);
        return key;
    }

    private void seed(final String key, final long used) {
        firstTemplate.<String, String>opsForHash().putAll(key, Map.of("used", Long.toString(used), "reserved", "0"))
                .then(firstTemplate.expire(key, WINDOW_TTL)).block(TIMEOUT);
    }

    private String field(final String key, final String name) {
        return firstTemplate.<String, String>opsForHash().get(key, name).block(TIMEOUT);
    }
}
