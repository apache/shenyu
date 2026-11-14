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

import org.apache.shenyu.common.metrics.AiAttemptEvent;
import org.apache.shenyu.common.metrics.AiTerminalEvent;

import java.time.Clock;
import java.time.Instant;
import java.util.Objects;
import java.util.concurrent.atomic.LongAdder;

/** Consumes existing terminal/attempt observations once; never creates a token settlement pipeline. */
public final class AiUsageLedgerRecorder {

    private final LedgerPriceBook prices;

    private final UsageLedgerCollector collector;

    private final LedgerAttributionResolver attribution;

    private final LedgerUsageResolver quantities;

    private final Clock clock;

    private final LongAdder invalid = new LongAdder();

    public AiUsageLedgerRecorder(final LedgerPriceBook prices, final UsageLedgerCollector collector,
                                 final LedgerAttributionResolver attribution, final LedgerUsageResolver quantities, final Clock clock) {
        this.prices = Objects.requireNonNull(prices, "prices");
        this.collector = Objects.requireNonNull(collector, "collector");
        this.attribution = Objects.requireNonNull(attribution, "attribution");
        this.quantities = Objects.requireNonNull(quantities, "quantities");
        this.clock = Objects.requireNonNull(clock, "clock");
    }

    public void recordAttempt(final AiAttemptEvent event) {
        try {
            Instant now = clock.instant();
            LedgerRecord.Facts facts = attribution.resolve(event);
            LedgerRecord.Usage usage = quantities.resolve(event);
            String id = LedgerRecord.key("attempt", facts.namespace(), facts.tenant(), event.context().getRequestId(), Integer.toString(event.attemptNumber()));
            collector.submit(new LedgerRecord(id, LedgerRecord.Kind.ATTEMPT, null, null, event.context().getRequestId(),
                    event.attemptNumber(), event.kind().name(), now, now, facts, usage, prices.estimate(facts, usage, now),
                    event.outcome().name(), event.httpStatus(), 0, 1, event.streaming()));
        } catch (RuntimeException error) {
            invalid.increment();
        }
    }

    public void record(final AiTerminalEvent event) {
        try {
            Instant now = clock.instant();
            LedgerRecord.Facts facts = attribution.resolve(event.context(), null, null);
            String id = LedgerRecord.key("logical", facts.namespace(), facts.tenant(), event.context().getRequestId());
            collector.submit(new LedgerRecord(id, LedgerRecord.Kind.LOGICAL, null, null, event.context().getRequestId(), 0, null,
                    now, now, facts, LedgerRecord.Usage.unavailable(LedgerRecord.Quality.UNKNOWN),
                    LedgerRecord.Estimate.unavailable(LedgerRecord.PriceStatus.NOT_APPLICABLE, now), event.outcome().name(), event.httpStatus(), event.attemptCount(), 1, false));
        } catch (RuntimeException error) {
            invalid.increment();
        }
    }

    /**
     * Invalid attribution or observation mapping drops remain visible without body logging.
     * @return number of rejected metadata mappings
     */
    public long invalidRecords() {
        return invalid.sum();
    }
}
