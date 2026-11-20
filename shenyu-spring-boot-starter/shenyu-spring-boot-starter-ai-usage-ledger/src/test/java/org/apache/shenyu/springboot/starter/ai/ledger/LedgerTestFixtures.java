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

import org.apache.shenyu.common.dto.AgentRequestContext;
import org.apache.shenyu.common.metrics.AiAttemptEvent;
import org.apache.shenyu.common.metrics.AiCallOutcome;
import org.apache.shenyu.common.metrics.AiTerminalEvent;
import org.apache.shenyu.common.metrics.AiUsage;

import java.math.BigDecimal;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.List;
import java.util.Map;

final class LedgerTestFixtures {

    static final Instant NOW = Instant.parse("2026-10-31T00:00:00Z");

    static final Clock CLOCK = Clock.fixed(NOW, ZoneOffset.UTC);

    private LedgerTestFixtures() {
    }

    static LedgerRecord.Facts facts(final String tenant, final String model) {
        return new LedgerRecord.Facts("namespace", tenant, "subject", "app", "feature", "internal-key-id",
                "openai", "requested", model, "openai-chat", "openai-chat", "CHAT_COMPLETION", 7L);
    }

    static LedgerRecord.Usage usage(final long input, final long output) {
        return new LedgerRecord.Usage(input, output, input + output, null, Map.of(),
                LedgerRecord.Quality.REPORTED, LedgerRecord.Origin.PROVIDER_REPORTED);
    }

    static LedgerPriceBook.Price price(final String version, final String model, final String input, final String output) {
        return new LedgerPriceBook.Price(version, "operator-fixture", "openai", model, "openai-chat", "USD",
                NOW.minusSeconds(3600), null, new BigDecimal(input), new BigDecimal(output));
    }

    static LedgerPriceBook prices() {
        return new LedgerPriceBook(List.of(price("p1", "main", "2", "10"), price("f1", "fallback", "2", "10"),
                new LedgerPriceBook.Price("responses-v1", "operator-fixture", "openai", "main", "openai-responses", "USD",
                        NOW.minusSeconds(3600), null, new BigDecimal("4"), new BigDecimal("20")),
                new LedgerPriceBook.Price("anthropic-v1", "operator-fixture", "anthropic", "main", "anthropic-messages", "USD",
                        NOW.minusSeconds(3600), null, new BigDecimal("7"), new BigDecimal("30"))));
    }

    static LedgerRecord attempt(final String invocation, final int number, final String phase, final String tenant,
                                final String model, final LedgerRecord.Usage usage) {
        LedgerRecord.Facts facts = facts(tenant, model);
        return new LedgerRecord(LedgerRecord.key("attempt", "namespace", tenant, invocation, Integer.toString(number)),
                LedgerRecord.Kind.ATTEMPT, null, null, invocation, number, phase, NOW, NOW, facts, usage,
                prices().estimate(facts, usage, NOW), "SUCCESS", 200, 0, 1, false);
    }

    static LedgerRecord terminal(final String invocation, final String tenant, final int count) {
        return new LedgerRecord(LedgerRecord.key("logical", "namespace", tenant, invocation), LedgerRecord.Kind.LOGICAL,
                null, null, invocation, 0, null, NOW, NOW, facts(tenant, "main"),
                LedgerRecord.Usage.unavailable(LedgerRecord.Quality.UNKNOWN),
                LedgerRecord.Estimate.unavailable(LedgerRecord.PriceStatus.NOT_APPLICABLE, NOW), "SUCCESS", 200, count, 1, false);
    }

    static AiAttemptEvent observed(final String invocation, final String tenant, final int number,
                                   final AiAttemptEvent.Kind kind, final AiUsage usage) {
        return new AiAttemptEvent(context(invocation, tenant), number, kind, "openai", "openai-chat", false,
                AiCallOutcome.SUCCESS, 200, 10L, usage);
    }

    static AiTerminalEvent completed(final String invocation, final String tenant, final int count, final AiUsage usage) {
        return new AiTerminalEvent(context(invocation, tenant), AiCallOutcome.SUCCESS, 200, 20L, count, usage);
    }

    static AgentRequestContext context(final String invocation, final String tenant) {
        return new AgentRequestContext(invocation, "llm", "selector", "rule").withIdentity(tenant, "subject")
                .withTarget("main", "CHAT_COMPLETION", null).withConfigurationVersion(7);
    }

    static UsageLedgerStore.Scope scope(final String tenant) {
        return new UsageLedgerStore.Scope("namespace", tenant, null);
    }
}
