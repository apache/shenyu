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

package org.apache.shenyu.springboot.starter.ai.ledger;

import org.apache.shenyu.common.ledger.LedgerPriceBook;
import org.apache.shenyu.common.ledger.LedgerRecord;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.Duration;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class UsageLedgerServiceTest {

    @Test
    void exportUsesTheStoreQueryContract() {
        UsageLedgerStore store = mock(UsageLedgerStore.class);
        UsageLedgerStore.Scope scope = LedgerTestFixtures.scope("a");
        when(store.query(scope, 1000)).thenReturn(List.of());
        when(store.diagnostics()).thenReturn(new UsageLedgerStore.Diagnostics(0, 2000, 0, 0, 0, 0, 0, LedgerTestFixtures.NOW, "epoch"));
        UsageLedgerService.Export exported = new UsageLedgerService(store, LedgerTestFixtures.prices(), LedgerTestFixtures.CLOCK).export(scope, 10);
        verify(store).query(scope, 1000);
        assertTrue(exported.retainedViewComplete());
        assertFalse(exported.truncated());
        assertEquals(0, exported.matchedRetainedRecords());
    }

    @ParameterizedTest
    @ValueSource(ints = {999, 1000, 1001})
    void reachingTheQueryCapCannotClaimACompleteRetainedView(final int count) {
        InMemoryUsageLedger store = new InMemoryUsageLedger(2000, Duration.ofMinutes(1), () -> 0L);
        for (int index = 0; index < count; index++) {
            store.append(LedgerTestFixtures.terminal("request-" + index, "a", 0));
        }
        store.append(LedgerTestFixtures.terminal("other-tenant", "b", 0));
        UsageLedgerService.Export exported = new UsageLedgerService(store, LedgerTestFixtures.prices(), LedgerTestFixtures.CLOCK)
                .export(LedgerTestFixtures.scope("a"), 1000);
        assertEquals(Math.min(count, 1000), exported.rows().size());
        assertEquals(Math.min(count, 1000), exported.matchedRetainedRecords());
        assertEquals(count < 1000, exported.retainedViewComplete());
        assertEquals(count >= 1000, exported.truncated());
        assertEquals(count >= 1000, exported.summary().possibleUnobservedConsumption());
        assertEquals(Math.min(count, 1000), exported.summary().noOutboundRequests());
        assertTrue(exported.rows().stream().allMatch(row -> "a".equals(row.facts().tenant())));
    }

    @Test
    void retryAndFallbackTotalsExcludeLogicalRollupsAndKeepTheFallbackSubset() {
        InMemoryUsageLedger store = new InMemoryUsageLedger(20, Duration.ofMinutes(1));
        store.append(LedgerTestFixtures.attempt("request", 1, "PRIMARY", "a", "main", LedgerTestFixtures.usage(1, 0)));
        store.append(LedgerTestFixtures.attempt("request", 2, "RETRY", "a", "main", LedgerTestFixtures.usage(2, 1)));
        store.append(LedgerTestFixtures.attempt("request", 3, "FALLBACK", "a", "fallback", LedgerTestFixtures.usage(3, 4)));
        store.append(LedgerTestFixtures.terminal("request", "a", 3));
        UsageLedgerService service = new UsageLedgerService(store, LedgerTestFixtures.prices(), LedgerTestFixtures.CLOCK);
        UsageLedgerService.Export exported = service.export(LedgerTestFixtures.scope("a"), 1);
        assertTrue(exported.truncated());
        assertEquals(4, exported.matchedRetainedRecords());
        assertEquals(3, exported.summary().actualAttempts());
        assertEquals(BigInteger.valueOf(6), exported.summary().knownProviderInput());
        assertEquals(BigInteger.valueOf(5), exported.summary().knownProviderOutput());
        assertEquals(0, new BigDecimal("0.000062").compareTo(exported.summary().costs().get("USD").knownEstimatedSubtotal()));
        assertEquals(0, new BigDecimal("0.000046").compareTo(exported.summary().costs().get("USD").knownFallbackSubtotal()));
        assertFalse(exported.summary().possibleUnobservedConsumption());
        assertEquals(2, exported.summary().attributedCosts().size());
        UsageLedgerService.AttributedCost fallback = exported.summary().attributedCosts().stream()
                .filter(group -> "fallback".equals(group.attribution().model())).findFirst().orElseThrow();
        assertEquals("a", fallback.attribution().tenant());
        assertEquals("app", fallback.attribution().application());
        assertEquals("feature", fallback.attribution().feature());
        assertEquals(1, fallback.cost().attempts());
        assertEquals(0, new BigDecimal("0.000046").compareTo(fallback.cost().knownEstimatedSubtotal()));
    }

    @Test
    void revaluationIsSeparatelyAuditableAndOriginalEstimateRemainsFrozen() {
        InMemoryUsageLedger store = new InMemoryUsageLedger(10, Duration.ofMinutes(1));
        LedgerRecord original = LedgerTestFixtures.attempt("request", 1, "PRIMARY", "a", "main", LedgerTestFixtures.usage(3, 12));
        store.append(original);
        LedgerPriceBook prices = LedgerTestFixtures.prices();
        prices.replace(List.of(LedgerTestFixtures.price("p2", "main", "4", "20")));
        UsageLedgerService service = new UsageLedgerService(store, prices, LedgerTestFixtures.CLOCK);
        LedgerRecord valuation = service.revalue(LedgerTestFixtures.scope("a"), original.id(), "review-1", LedgerTestFixtures.NOW);
        assertNotEquals(original.id(), valuation.id());
        assertEquals(original.id(), valuation.originalId());
        assertEquals("p1", store.find(LedgerTestFixtures.scope("a"), original.id()).estimate().priceVersion());
        assertEquals("p2", valuation.estimate().priceVersion());
        assertEquals(0, new BigDecimal("0.000252").compareTo(valuation.estimate().amount()));
        assertEquals(1, service.export(LedgerTestFixtures.scope("a"), 100).summary().actualAttempts());
        service.revalue(LedgerTestFixtures.scope("a"), original.id(), "review-1", LedgerTestFixtures.NOW);
        assertEquals(2, store.query(LedgerTestFixtures.scope("a"), 100).size());
        assertThrows(SecurityException.class, () -> service.revalue(LedgerTestFixtures.scope("b"), original.id(), "other", LedgerTestFixtures.NOW));
    }

    @Test
    void unknownUsageAndMissingTerminalStayIncompleteWithoutOverflowOrZeroCostClaims() {
        InMemoryUsageLedger store = new InMemoryUsageLedger(10, Duration.ofMinutes(1));
        store.append(LedgerTestFixtures.attempt("request", 1, "PRIMARY", "a", "main", LedgerTestFixtures.usage(Long.MAX_VALUE, 0)));
        store.append(LedgerTestFixtures.attempt("request", 2, "RETRY", "a", "main", LedgerTestFixtures.usage(Long.MAX_VALUE, 0)));
        store.append(LedgerTestFixtures.attempt("request", 3, "FALLBACK", "a", "main", LedgerRecord.Usage.unavailable(LedgerRecord.Quality.UNKNOWN)));
        UsageLedgerService.Summary summary = new UsageLedgerService(store, LedgerTestFixtures.prices(), LedgerTestFixtures.CLOCK)
                .export(LedgerTestFixtures.scope("a"), 100).summary();
        assertEquals(BigInteger.valueOf(Long.MAX_VALUE).multiply(BigInteger.TWO), summary.knownProviderInput());
        assertEquals(1, summary.unknownUsageAttempts());
        assertTrue(summary.possibleUnobservedConsumption());
        assertEquals(1, summary.costs().get("USD").unknownAttempts());
    }
}
