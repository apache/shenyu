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

package org.apache.shenyu.plugin.ai.proxy.enhanced.evaluation;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.AgentRequestContext;
import org.apache.shenyu.common.dto.convert.rule.AiEvaluationRouteConfig;
import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;
import org.apache.shenyu.common.metrics.AiCallOutcome;
import org.apache.shenyu.common.metrics.AiTerminalEvent;
import org.apache.shenyu.common.metrics.AiUsage;
import org.apache.shenyu.plugin.ai.common.evaluation.AiEvaluationExportRecord;
import org.apache.shenyu.plugin.ai.common.evaluation.AiEvaluationFeedback;
import org.apache.shenyu.plugin.ai.proxy.enhanced.handler.AiProxyPluginHandler;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Deterministic tests for the opt-in evaluation sample manager: the double opt-in
 * chain, the metadata-only contract, the identity allow-list, the feedback join
 * authorization and idempotency semantics, forged and expired references, and the
 * observation that never throws.
 */
class AiEvaluationSampleManagerTest {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    private static final String SELECTOR_ID = "eval-selector";

    private static final String TENANT = "tenant-a";

    private final List<AiEvaluationExportRecord> exported = new ArrayList<>();

    private final AiProxyPluginHandler handler = new AiProxyPluginHandler();

    private AiEvaluationProperties properties;

    private AiEvaluationSampleManager manager;

    private AiEvaluationExporter exporter;

    private AiEvaluationRouteConfig route;

    @BeforeEach
    void setUp() {
        properties = new AiEvaluationProperties();
        route = new AiEvaluationRouteConfig();
        AiProxyHandle handle = new AiProxyHandle();
        handle.setEvaluation(route);
        handler.getSelectorCachedHandle().cachedHandle(CacheKeyUtils.INST.getKey(SELECTOR_ID, Constants.DEFAULT_RULE), handle);
        rebuildManager(30 * 60 * 1000L);
    }

    private void rebuildManager(final long retentionMillis) {
        properties.setJoinRetentionMillis(retentionMillis);
        exporter = new AiEvaluationExporter(64, 8, exported::addAll, false);
        manager = new AiEvaluationSampleManager(handler, properties, exporter);
    }

    @AfterEach
    void tearDown() {
        handler.getSelectorCachedHandle().removeHandle(CacheKeyUtils.INST.getKey(SELECTOR_ID, Constants.DEFAULT_RULE));
    }

    private void recordTerminal() {
        recordTerminal(TENANT);
    }

    private void recordTerminal(final String tenant) {
        AgentRequestContext context = new AgentRequestContext("req-1", "ai", SELECTOR_ID, "rule-1")
                .withIdentity(tenant, "subject-1");
        AiTerminalEvent event = new AiTerminalEvent(context, AiCallOutcome.SUCCESS, 200, 55L, 1,
                new AiUsage(10L, 20L, 30L, AiUsage.Status.REPORTED));
        manager.record(event);
    }

    private String drainFirstSamplePayload() {
        exporter.drainOnce();
        assertEquals(1, exported.size());
        AiEvaluationExportRecord record = exported.get(0);
        assertEquals(AiEvaluationExportRecord.Type.SAMPLE, record.type());
        assertEquals("eval-sample-v1", record.schemaVersion());
        return record.payloadJson();
    }

    private String drainedSampleId() throws Exception {
        return MAPPER.readTree(drainFirstSamplePayload()).path("sampleId").asText();
    }

    @Test
    void disabledRouteProducesNothing() {
        route.setEnabled(false);
        recordTerminal();
        assertEquals(0, exporter.drainOnce());
        assertTrue(exported.isEmpty());
    }

    @Test
    void zeroRateNeverSamples() {
        route.setEnabled(true);
        route.setSampleRate("0");
        recordTerminal();
        assertEquals(0, exporter.drainOnce());
        assertTrue(exported.isEmpty());
    }

    @Test
    void sampledRouteExportsMetadataOnlySample() throws Exception {
        route.setEnabled(true);
        recordTerminal();

        JsonNode payload = MAPPER.readTree(drainFirstSamplePayload());
        assertEquals("eval-sample-v1", payload.path("schemaVersion").asText());
        assertEquals("req-1", payload.path("requestId").asText());
        assertEquals(SELECTOR_ID, payload.path("selectorId").asText());
        assertEquals("SUCCESS", payload.path("outcome").asText());
        assertEquals(1, payload.path("attemptCount").asInt());
        assertEquals(30L, payload.path("totalTokens").asLong());
        assertEquals("REPORTED", payload.path("usageStatus").asText());
        assertTrue(payload.path("tenant").isNull());
        assertTrue(payload.path("subject").isNull());
        assertTrue(payload.path("target").isNull());
    }

    @Test
    void identityIsExportedOnlyWhenAllowListed() throws Exception {
        route.setEnabled(true);
        route.setIncludeIdentity(true);
        recordTerminal();

        JsonNode payload = MAPPER.readTree(drainFirstSamplePayload());
        assertEquals(TENANT, payload.path("tenant").asText());
        assertEquals("subject-1", payload.path("subject").asText());
    }

    @Test
    void unknownUsageStaysUnknownNotZero() throws Exception {
        route.setEnabled(true);
        AgentRequestContext context = new AgentRequestContext("req-1", "ai", SELECTOR_ID, "rule-1");
        manager.record(new AiTerminalEvent(context, AiCallOutcome.TIMEOUT, null, 5L, 1, AiUsage.UNKNOWN));
        JsonNode payload = MAPPER.readTree(drainFirstSamplePayload());
        assertTrue(payload.path("promptTokens").isNull());
        assertTrue(payload.path("httpStatus").isNull());
        assertEquals("TIMEOUT", payload.path("outcome").asText());
    }

    @Test
    void acceptedJoinExportsTheFeedbackEvent() throws Exception {
        route.setEnabled(true);
        recordTerminal();
        String sampleId = drainedSampleId();

        AiFeedbackJoinResult result = manager.joinFeedback(new AiEvaluationFeedback(sampleId, TENANT, "judge-1",
                "eval-set-7", "judge-9", Map.of("correct", "true"), 123L));

        assertEquals(AiFeedbackJoinResult.Status.ACCEPTED, result.status());
        exporter.drainOnce();
        AiEvaluationExportRecord feedbackRecord = exported.get(exported.size() - 1);
        assertEquals(AiEvaluationExportRecord.Type.FEEDBACK, feedbackRecord.type());
        assertEquals("eval-feedback-v1", feedbackRecord.schemaVersion());
        JsonNode feedback = MAPPER.readTree(feedbackRecord.payloadJson());
        assertEquals(sampleId, feedback.path("sampleId").asText());
        assertEquals("judge-1", feedback.path("judgeId").asText());
        assertEquals("true", feedback.path("labels").path("correct").asText());
    }

    @Test
    void duplicateJudgeUpdatesLabelsIdempotently() throws Exception {
        route.setEnabled(true);
        recordTerminal();
        String sampleId = drainedSampleId();

        assertEquals(AiFeedbackJoinResult.Status.ACCEPTED, manager.joinFeedback(join(sampleId, "judge-1", "v1")).status());
        assertEquals(AiFeedbackJoinResult.Status.UPDATED, manager.joinFeedback(join(sampleId, "judge-1", "v2")).status());

        exporter.drainOnce();
        long feedbackCount = exported.stream().filter(r -> r.type() == AiEvaluationExportRecord.Type.FEEDBACK).count();
        assertEquals(2, feedbackCount);
    }

    @Test
    void distinctJudgesAppendUntilTheBound() throws Exception {
        route.setEnabled(true);
        properties.setMaxJoinsPerSample(2);
        rebuildManager(properties.getJoinRetentionMillis());
        recordTerminal();
        exporter.drainOnce();
        String sampleId = drainedSampleId();

        assertEquals(AiFeedbackJoinResult.Status.ACCEPTED, manager.joinFeedback(join(sampleId, "judge-1", null)).status());
        assertEquals(AiFeedbackJoinResult.Status.ACCEPTED, manager.joinFeedback(join(sampleId, "judge-2", null)).status());
        assertEquals(AiFeedbackJoinResult.Status.REJECTED_BOUND, manager.joinFeedback(join(sampleId, "judge-3", null)).status());
    }

    @Test
    void forgedSampleReferencesAreRejectedWithoutCreatingState() {
        route.setEnabled(true);
        assertEquals(AiFeedbackJoinResult.Status.UNKNOWN_SAMPLE,
                manager.joinFeedback(join("forged-sample-id", "judge-1", null)).status());
    }

    @Test
    void tenantMismatchIsRejected() throws Exception {
        route.setEnabled(true);
        recordTerminal();
        String sampleId = drainedSampleId();

        assertEquals(AiFeedbackJoinResult.Status.TENANT_MISMATCH,
                manager.joinFeedback(join(sampleId, "tenant-b", "judge-1", null)).status());
    }

    @Test
    void malformedFeedbackIsRejected() {
        route.setEnabled(true);
        assertEquals(AiFeedbackJoinResult.Status.REJECTED_INVALID,
                manager.joinFeedback(new AiEvaluationFeedback("any", TENANT, "", null, null, Map.of("a", "b"), 1L)).status());
        Map<String, String> tooMany = Map.of("l1", "v", "l2", "v", "l3", "v", "l4", "v", "l5", "v",
                "l6", "v", "l7", "v", "l8", "v", "l9", "v");
        assertEquals(AiFeedbackJoinResult.Status.REJECTED_INVALID,
                manager.joinFeedback(new AiEvaluationFeedback("any", TENANT, "judge", null, null, tooMany, 1L)).status());
    }

    @Test
    void expiredSampleReferencesExpireOutOfTheJoinRegistry() throws Exception {
        route.setEnabled(true);
        rebuildManager(1L);
        recordTerminal();
        exporter.drainOnce();
        String sampleId = drainedSampleId();

        await().atMost(5, TimeUnit.SECONDS).until(() ->
                manager.joinFeedback(join(sampleId, TENANT, "judge-1")).status() == AiFeedbackJoinResult.Status.UNKNOWN_SAMPLE);
    }

    @Test
    void aBrokenSamplerConfigurationNeverThrowsOutOfTheObserver() {
        route.setEnabled(true);
        route.setSampleRate("not-a-number");
        recordTerminal();
        assertTrue(exported.isEmpty());
    }

    private AiEvaluationFeedback join(final String sampleId, final String judgeId, final String judgeVersion) {
        return join(sampleId, TENANT, judgeId, judgeVersion);
    }

    private AiEvaluationFeedback join(final String sampleId, final String tenant, final String judgeId,
                                      final String judgeVersion) {
        return new AiEvaluationFeedback(sampleId, tenant, judgeId, null, judgeVersion, Map.of("correct", "true"), 1L);
    }
}
