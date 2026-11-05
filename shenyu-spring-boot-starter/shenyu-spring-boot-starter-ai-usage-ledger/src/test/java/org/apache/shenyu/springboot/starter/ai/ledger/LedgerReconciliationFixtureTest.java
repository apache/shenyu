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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.common.ledger.LedgerRecord;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.Duration;

import static org.junit.jupiter.api.Assertions.assertEquals;

/** Independent input/expected totals, without networking or the calculator as the oracle. */
class LedgerReconciliationFixtureTest {

    @Test
    void replayVersionedFixtureRowsAndExpectedSubtotals() throws Exception {
        JsonNode fixtures = new ObjectMapper().readTree(getClass().getResourceAsStream("/ledger-reconciliation.json"));
        for (JsonNode scenario : fixtures) {
            InMemoryUsageLedger store = new InMemoryUsageLedger(20, Duration.ofMinutes(1));
            int sequence = 0;
            for (JsonNode attempt : scenario.path("attempts")) {
                LedgerRecord.Usage usage = attempt.path("input").isNull() ? LedgerRecord.Usage.unavailable(LedgerRecord.Quality.UNKNOWN)
                        : LedgerTestFixtures.usage(attempt.path("input").longValue(), attempt.path("output").longValue());
                LedgerRecord original = LedgerTestFixtures.attempt("request", ++sequence, attempt.path("phase").asText(), "a", attempt.path("model").asText(), usage);
                LedgerRecord.Facts facts = original.facts();
                if (scenario.has("protocol")) {
                    facts = new LedgerRecord.Facts(facts.namespace(), facts.tenant(), facts.subject(), facts.application(), facts.feature(), facts.apiKeyId(),
                            scenario.path("provider").asText(), null, facts.resolvedModel(), scenario.path("protocol").asText(),
                            scenario.path("protocol").asText(), facts.operation(), facts.configurationVersion());
                }
                LedgerRecord row = new LedgerRecord(original.id(), original.kind(), null, null, original.invocationId(), original.attemptNumber(),
                        original.phase(), original.observedAt(), original.recordedAt(), facts, original.usage(),
                        LedgerTestFixtures.prices().estimate(facts, usage, LedgerTestFixtures.NOW),
                        attempt.path("outcome").asText(), "ERROR".equals(attempt.path("outcome").asText()) ? 503 : 200, 0, 1, false);
                store.append(row);
                if (scenario.path("duplicate").asBoolean()) {
                    store.append(row);
                }
            }
            store.append(LedgerTestFixtures.terminal("request", "a", sequence));
            UsageLedgerService.Summary summary = new UsageLedgerService(store, LedgerTestFixtures.prices(), LedgerTestFixtures.CLOCK)
                    .export(LedgerTestFixtures.scope("a"), 100).summary();
            JsonNode expected = scenario.path("expected");
            String name = scenario.path("name").asText();
            final UsageLedgerService.Cost cost = summary.costs().get(expected.path("group").asText());
            assertEquals(sequence, summary.actualAttempts(), name);
            assertEquals(new BigInteger(expected.path("input").asText()), summary.knownProviderInput(), name);
            assertEquals(new BigInteger(expected.path("output").asText()), summary.knownProviderOutput(), name);
            assertEquals(0, new BigDecimal(expected.path("cost").asText()).compareTo(cost.knownEstimatedSubtotal()), name);
            assertEquals(expected.path("unknown").asInt(), cost.unknownAttempts(), name);
            assertEquals(expected.path("exposure").asBoolean(), summary.possibleUnobservedConsumption(), name);
        }
    }
}
