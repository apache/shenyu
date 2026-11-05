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

package org.apache.shenyu.common.ledger;

import java.math.BigDecimal;
import java.time.Instant;
import java.util.Currency;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;

/** Atomic operator-owned fixed input/output prices, separate from all reservation counters. */
public final class LedgerPriceBook {

    private static final BigDecimal MILLION = new BigDecimal("1000000");

    private final AtomicReference<List<Price>> prices = new AtomicReference<>(List.of());

    private final Map<String, Price> revisions = new HashMap<>();

    public LedgerPriceBook(final List<Price> initial) {
        replace(initial);
    }

    /**
     * Install a complete validated price snapshot, preserving already recorded estimates.
     * @param replacement trusted operator schedules
     */
    public synchronized void replace(final List<Price> replacement) {
        List<Price> copy = List.copyOf(replacement);
        if (copy.size() > 64 || copy.stream().map(Price::version).distinct().count() != copy.size()) {
            throw new IllegalArgumentException("Invalid price schedule count or duplicate revision");
        }
        if (java.util.stream.Stream.concat(revisions.keySet().stream(), copy.stream().map(Price::version)).distinct().count() > 1024) {
            throw new IllegalArgumentException("Price revision history is full");
        }
        for (Price previous : revisions.values()) {
            if (copy.stream().anyMatch(price -> price.version().equals(previous.version()) && !price.equals(previous))) {
                throw new IllegalArgumentException("A price revision must not be rewritten");
            }
        }
        for (int i = 0; i < copy.size(); i++) {
            for (int j = i + 1; j < copy.size(); j++) {
                Price a = copy.get(i);
                Price b = copy.get(j);
                if (a.sameRoute(b) && (Objects.isNull(a.until()) || b.from().isBefore(a.until()))
                        && (Objects.isNull(b.until()) || a.from().isBefore(b.until()))) {
                    throw new IllegalArgumentException("Ambiguous effective price schedules");
                }
            }
        }
        copy.forEach(price -> revisions.put(price.version(), price));
        prices.set(copy);
    }

    /**
     * Calculate only declared categories from normalized quantities.
     * @param facts trusted route
     * @param usage original attempt snapshot
     * @param asOf explicit effective-time selection
     * @return immutable estimate and provenance, or unknown rather than zero
     */
    public LedgerRecord.Estimate estimate(final LedgerRecord.Facts facts, final LedgerRecord.Usage usage, final Instant asOf) {
        Price selected = prices.get().stream().filter(price -> price.matches(facts, asOf)).findFirst().orElse(null);
        if (Objects.isNull(selected)) {
            return LedgerRecord.Estimate.unavailable(LedgerRecord.PriceStatus.UNKNOWN_PRICE, asOf);
        }
        LedgerRecord.PriceStatus status;
        BigDecimal amount = null;
        if (usage.quality() != LedgerRecord.Quality.REPORTED && usage.quality() != LedgerRecord.Quality.ESTIMATED
                || Objects.isNull(usage.inputTokens()) || Objects.isNull(usage.outputTokens())) {
            status = LedgerRecord.PriceStatus.UNKNOWN_USAGE;
        } else if (Objects.nonNull(usage.cachedInputTokens()) && usage.cachedInputTokens() > 0
                || usage.categories().values().stream().anyMatch(value -> value > 0)) {
            status = LedgerRecord.PriceStatus.UNSUPPORTED_CATEGORY;
        } else {
            status = LedgerRecord.PriceStatus.ESTIMATED;
            amount = selected.inputPerMillion().multiply(BigDecimal.valueOf(usage.inputTokens()))
                    .add(selected.outputPerMillion().multiply(BigDecimal.valueOf(usage.outputTokens()))).divide(MILLION);
        }
        return new LedgerRecord.Estimate(status, amount, selected.currency(), selected.version(), selected.source(), selected.from(), selected.until(), asOf,
                selected.inputPerMillion(), selected.outputPerMillion());
    }

    /**
     * A fixed input/output price in one documented provider/client-protocol route.
     * @param version immutable operator revision
     * @param source operator supplied source reference
     * @param provider actual provider key
     * @param model actual resolved model
     * @param clientProtocol measured client protocol; does not invent a native protocol
     * @param currency ISO currency without FX
     * @param from inclusive effective start
     * @param until exclusive effective end, nullable
     * @param inputPerMillion input price
     * @param outputPerMillion output price
     */
    public record Price(String version, String source, String provider, String model, String clientProtocol,
                         String currency, Instant from, Instant until, BigDecimal inputPerMillion, BigDecimal outputPerMillion) {
        public Price {
            for (String value : new String[]{version, source, provider, model, clientProtocol, currency}) {
                LedgerRecord.required(value);
            }
            Currency.getInstance(currency);
            Objects.requireNonNull(from, "from");
            if (Objects.nonNull(until) && !from.isBefore(until)) {
                throw new IllegalArgumentException("Invalid price interval");
            }
            for (BigDecimal rate : new BigDecimal[]{inputPerMillion, outputPerMillion}) {
                if (Objects.isNull(rate) || rate.signum() < 0 || rate.precision() > 18 || rate.scale() < -12 || rate.scale() > 12) {
                    throw new IllegalArgumentException("Invalid bounded price");
                }
            }
        }

        private boolean sameRoute(final Price other) {
            return provider.equals(other.provider) && model.equals(other.model) && clientProtocol.equals(other.clientProtocol);
        }

        private boolean matches(final LedgerRecord.Facts facts, final Instant when) {
            return provider.equals(facts.provider()) && model.equals(facts.resolvedModel()) && clientProtocol.equals(facts.clientProtocol())
                    && !when.isBefore(from) && (Objects.isNull(until) || when.isBefore(until));
        }
    }
}
