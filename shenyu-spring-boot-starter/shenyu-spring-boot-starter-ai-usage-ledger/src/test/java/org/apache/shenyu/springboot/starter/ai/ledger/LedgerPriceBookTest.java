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
import java.time.Instant;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

class LedgerPriceBookTest {

    @Test
    void fixedPriceUsesMeasuredInputOutputWithoutAddingReportedTotal() {
        LedgerRecord.Estimate estimate = LedgerTestFixtures.prices().estimate(LedgerTestFixtures.facts("tenant", "main"),
                LedgerTestFixtures.usage(3, 12), LedgerTestFixtures.NOW);
        assertEquals(0, new BigDecimal("0.000126").compareTo(estimate.amount()));
        assertEquals("USD", estimate.currency());
        assertEquals("p1", estimate.priceVersion());
        assertEquals("operator-fixture", estimate.priceSource());
        assertEquals(new BigDecimal("2"), estimate.inputPerMillion());
        assertEquals(LedgerRecord.PriceStatus.ESTIMATED, estimate.status());
    }

    @Test
    void missingUsageMissingPriceAndReportedZeroStayDistinct() {
        LedgerPriceBook book = LedgerTestFixtures.prices();
        LedgerRecord.Facts facts = LedgerTestFixtures.facts("tenant", "main");
        LedgerRecord.Estimate unknown = book.estimate(facts, LedgerRecord.Usage.unavailable(LedgerRecord.Quality.UNKNOWN), LedgerTestFixtures.NOW);
        assertEquals(LedgerRecord.PriceStatus.UNKNOWN_USAGE, unknown.status());
        assertNull(unknown.amount());
        assertEquals("p1", unknown.priceVersion());
        assertEquals(LedgerRecord.PriceStatus.UNKNOWN_PRICE,
                book.estimate(LedgerTestFixtures.facts("tenant", "unpriced"), LedgerTestFixtures.usage(0, 0), LedgerTestFixtures.NOW).status());
        assertEquals(0, BigDecimal.ZERO.compareTo(book.estimate(facts, LedgerTestFixtures.usage(0, 0), LedgerTestFixtures.NOW).amount()));
    }

    @Test
    void unsupportedCachedCategoriesNeverSilentlyReceiveNormalTokenPrices() {
        LedgerRecord.Usage usage = new LedgerRecord.Usage(10L, 1L, 11L, 5L, Map.of(),
                LedgerRecord.Quality.REPORTED, LedgerRecord.Origin.PROVIDER_REPORTED);
        LedgerRecord.Estimate price = LedgerTestFixtures.prices().estimate(LedgerTestFixtures.facts("tenant", "main"), usage, LedgerTestFixtures.NOW);
        assertEquals(LedgerRecord.PriceStatus.UNSUPPORTED_CATEGORY, price.status());
        assertNull(price.amount());
    }

    @Test
    void effectiveBoundaryAmbiguityAndVersionMutationAreExplicit() {
        Instant boundary = LedgerTestFixtures.NOW;
        LedgerPriceBook.Price first = new LedgerPriceBook.Price("old", "fixture", "openai", "main", "openai-chat", "USD",
                boundary.minusSeconds(60), boundary, BigDecimal.ONE, BigDecimal.ONE);
        LedgerPriceBook.Price next = new LedgerPriceBook.Price("new", "fixture", "openai", "main", "openai-chat", "USD",
                boundary, null, BigDecimal.TEN, BigDecimal.TEN);
        LedgerPriceBook book = new LedgerPriceBook(List.of(first, next));
        assertEquals("old", book.estimate(LedgerTestFixtures.facts("tenant", "main"), LedgerTestFixtures.usage(1, 1), boundary.minusNanos(1)).priceVersion());
        assertEquals("new", book.estimate(LedgerTestFixtures.facts("tenant", "main"), LedgerTestFixtures.usage(1, 1), boundary).priceVersion());
        assertThrows(IllegalArgumentException.class, () -> new LedgerPriceBook(List.of(next, LedgerTestFixtures.price("overlap", "main", "2", "10"))));
        assertThrows(IllegalArgumentException.class, () -> book.replace(List.of(LedgerTestFixtures.price("new", "main", "2", "10"))));
        book.replace(List.of());
        assertThrows(IllegalArgumentException.class, () -> book.replace(List.of(LedgerTestFixtures.price("new", "main", "2", "10"))));
    }

    @ParameterizedTest
    @ValueSource(strings = {"-1", "1E100000", "0.0000000000001", "1234567890123456789"})
    void invalidRatesAreRejectedBeforeTraffic(final String value) {
        assertThrows(IllegalArgumentException.class, () -> LedgerTestFixtures.price("bad", "main", value, "10"));
    }
}
