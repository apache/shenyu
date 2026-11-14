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

import org.apache.shenyu.common.ledger.LedgerRecord;
import org.apache.shenyu.common.metrics.AiAttemptEvent;
import org.apache.shenyu.common.metrics.AiAttemptRoute;
import org.apache.shenyu.common.metrics.AiCallOutcome;
import org.apache.shenyu.common.metrics.AiUsage;
import org.junit.jupiter.api.Test;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

class LedgerSharedContractTest {

    @Test
    void actualRouteIsDistinctFromDiagnosticContextAndClientProtocol() {
        AiAttemptEvent event = event(new AiUsage(3L, 2L, 5L, AiUsage.Status.REPORTED));
        LedgerRecord.Facts facts = LedgerAttributionResolver.fromContext("namespace").resolve(event);
        assertEquals("requested", facts.requestedModel());
        assertEquals("native-model", facts.resolvedModel());
        assertEquals("openai-responses", facts.clientProtocol());
        assertEquals("openai-chat", facts.nativeProtocol());
        assertEquals("tenant", facts.tenant());
        assertNull(facts.apiKeyId());
    }

    @Test
    void legacyConstructorDoesNotInventNativeProtocolOrModelFacts() {
        AiAttemptEvent event = new AiAttemptEvent(LedgerTestFixtures.context("legacy", "tenant"), 1,
                AiAttemptEvent.Kind.PRIMARY, "openai", "openai-chat", false, AiCallOutcome.SUCCESS,
                200, 1L, AiUsage.UNKNOWN);
        LedgerRecord.Facts facts = LedgerAttributionResolver.fromContext("namespace").resolve(event);
        assertNull(facts.nativeProtocol());
        assertNull(facts.requestedModel());
        assertNull(facts.resolvedModel());
    }

    @Test
    void estimatesRetainTheirOriginInsteadOfBecomingActualUsage() {
        AiUsage usage = new AiUsage(3L, 2L, 5L, null, Map.of(), AiUsage.Status.REPORTED,
                AiUsage.Source.GATEWAY_ESTIMATE, AiUsage.Aggregation.CUMULATIVE_SNAPSHOT);
        LedgerRecord.Usage mapped = LedgerUsageResolver.currentContract().resolve(event(usage));
        assertEquals(LedgerRecord.Quality.ESTIMATED, mapped.quality());
        assertEquals(LedgerRecord.Origin.GATEWAY_ESTIMATE, mapped.origin());
    }

    @Test
    void incrementalCountsCannotBeChargedAsFinalSnapshots() {
        AiUsage usage = new AiUsage(3L, 2L, 5L, null, Map.of(), AiUsage.Status.REPORTED,
                AiUsage.Source.PROVIDER_REPORTED, AiUsage.Aggregation.INCREMENTAL);
        LedgerRecord.Usage mapped = LedgerUsageResolver.currentContract().resolve(event(usage));
        assertEquals(LedgerRecord.Quality.UNSUPPORTED, mapped.quality());
        assertNull(mapped.inputTokens());
    }

    private AiAttemptEvent event(final AiUsage usage) {
        return new AiAttemptEvent(LedgerTestFixtures.context("actual", "tenant"), 1, AiAttemptEvent.Kind.PRIMARY,
                new AiAttemptRoute("openai", "openai-responses", "openai-chat", "requested", "native-model"),
                false, AiCallOutcome.SUCCESS, 200, 1L, usage);
    }
}
