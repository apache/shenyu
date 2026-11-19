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

package org.apache.shenyu.plugin.ai.common.view;

import org.apache.shenyu.common.ai.AiNormalizedRequestView;
import org.apache.shenyu.plugin.ai.protocol.AiProtocols;
import org.junit.jupiter.api.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Deterministic tests for the bounded view publication, consumption and generation
 * semantics. No test sleeps; every exchange is isolated.
 */
class AiRequestViewOperationsTest {

    private static final String CHAT_BODY = "{\"model\":\"gpt-4\",\"stream\":true,"
            + "\"messages\":[{\"role\":\"system\",\"content\":\"be brief\"},{\"role\":\"user\",\"content\":\"hello\"}],"
            + "\"tools\":[{\"type\":\"function\",\"function\":{\"name\":\"ping\"}}]}";

    @Test
    void wireViewParsesTheChatShapeOnce() {
        MockServerWebExchange exchange = exchange();

        AiNormalizedRequestView view = AiRequestViewOperations.ensurePublished(exchange, CHAT_BODY, null);

        assertEquals("openai-chat", view.protocol());
        assertEquals("CHAT_COMPLETION", view.operation());
        assertEquals("gpt-4", view.model());
        assertTrue(view.stream());
        assertEquals(2, view.messages().size());
        assertEquals("system", view.messages().get(0).role());
        assertEquals("be brief", view.messages().get(0).textPrefix());
        assertFalse(view.messages().get(0).truncated());
        assertTrue(view.toolsPresent());
        assertFalse(view.toolCallsPresent());
        assertEquals(AiNormalizedRequestView.ViewKind.WIRE, view.viewKind());
        assertNull(view.mutator());
        assertEquals(1L, view.generation());
        assertTrue(view.supported());
        assertEquals(1L, AiRequestViewOperations.currentGeneration(exchange));
    }

    @Test
    void republishingTheSameBodyReturnsTheSameView() {
        MockServerWebExchange exchange = exchange();

        AiNormalizedRequestView first = AiRequestViewOperations.ensurePublished(exchange, CHAT_BODY, "aiPrompt");
        AiNormalizedRequestView second = AiRequestViewOperations.ensurePublished(exchange, CHAT_BODY, "aiPrompt");

        assertSame(first, second);
        assertEquals(1L, AiRequestViewOperations.currentGeneration(exchange));
    }

    @Test
    void theSameBodyWithADifferentProtocolStartsANewGeneration() {
        MockServerWebExchange exchange = exchange();
        AiRequestViewOperations.ensurePublished(exchange, CHAT_BODY, null, AiProtocols.OPENAI_RESPONSES);

        AiNormalizedRequestView view = AiRequestViewOperations.ensurePublished(exchange, CHAT_BODY, "aiProxy",
                AiProtocols.OPENAI_CHAT);

        assertEquals(2L, view.generation());
        assertEquals(AiProtocols.OPENAI_CHAT, view.protocol());
    }

    @Test
    void mutationPublishesANewEffectiveGeneration() {
        MockServerWebExchange exchange = exchange();
        AiRequestViewOperations.ensurePublished(exchange, CHAT_BODY, null);
        String decorated = CHAT_BODY.replace("\"messages\":[", "\"messages\":[{\"role\":\"system\",\"content\":\"prefix\"},");

        AiNormalizedRequestView view = AiRequestViewOperations.ensurePublished(exchange, decorated, "aiPrompt");

        assertEquals(2L, view.generation());
        assertEquals(AiNormalizedRequestView.ViewKind.EFFECTIVE, view.viewKind());
        assertEquals("aiPrompt", view.mutator());
        assertEquals(3, view.messages().size());
        assertEquals(2L, AiRequestViewOperations.currentGeneration(exchange));
    }

    @Test
    void aDerivedBodyWithoutRealChangeDoesNotBumpTheGeneration() {
        MockServerWebExchange exchange = exchange();
        AiRequestViewOperations.ensurePublished(exchange, CHAT_BODY, null);

        AiRequestViewOperations.ensurePublished(exchange, CHAT_BODY, "aiPrompt");

        assertEquals(1L, AiRequestViewOperations.currentGeneration(exchange));
        AiNormalizedRequestView view = AiRequestViewOperations.snapshot(exchange).orElseThrow();
        assertEquals(AiNormalizedRequestView.ViewKind.WIRE, view.viewKind());
    }

    @Test
    void inspectionsTrackTheGenerationTheySaw() {
        MockServerWebExchange exchange = exchange();
        AiRequestViewOperations.ensurePublished(exchange, CHAT_BODY, null);
        AiRequestViewOperations.snapshotAndRecordInspection(exchange, "early");

        String decorated = CHAT_BODY.replace("hello", "hello decorated");
        AiRequestViewOperations.ensurePublished(exchange, decorated, "aiPrompt");
        AiRequestViewOperations.snapshotAndRecordInspection(exchange, "late");

        List<AiRequestViewOperations.Inspection> report = AiRequestViewOperations.inspectionReport(exchange);
        assertEquals(2, report.size());
        assertEquals("early", report.get(0).consumer());
        assertEquals(1L, report.get(0).generation());
        assertFalse(report.get(0).current());
        assertEquals("late", report.get(1).consumer());
        assertEquals(2L, report.get(1).generation());
        assertTrue(report.get(1).current());
    }

    @Test
    void reInspectionReplacesTheConsumerEntry() {
        MockServerWebExchange exchange = exchange();
        AiRequestViewOperations.ensurePublished(exchange, CHAT_BODY, null);
        AiRequestViewOperations.snapshotAndRecordInspection(exchange, "consumer");

        AiRequestViewOperations.ensurePublished(exchange, CHAT_BODY.replace("hello", "changed"), "aiPrompt");
        AiRequestViewOperations.snapshotAndRecordInspection(exchange, "consumer");

        List<AiRequestViewOperations.Inspection> report = AiRequestViewOperations.inspectionReport(exchange);
        assertEquals(1, report.size());
        assertEquals(2L, report.get(0).generation());
        assertTrue(report.get(0).current());
    }

    @Test
    void inspectionLogStaysBounded() {
        MockServerWebExchange exchange = exchange();
        AiRequestViewOperations.ensurePublished(exchange, CHAT_BODY, null);

        for (int i = 0; i < AiNormalizedRequestView.MAX_INSPECTIONS + 3; i++) {
            AiRequestViewOperations.snapshotAndRecordInspection(exchange, "consumer-" + i);
        }

        List<AiRequestViewOperations.Inspection> report = AiRequestViewOperations.inspectionReport(exchange);
        assertEquals(AiNormalizedRequestView.MAX_INSPECTIONS, report.size());
        assertEquals("consumer-0", report.get(0).consumer());
    }

    @Test
    void inspectionWithoutAPublishedViewRecordsNothing() {
        MockServerWebExchange exchange = exchange();

        assertTrue(AiRequestViewOperations.snapshotAndRecordInspection(exchange, "consumer").isEmpty());
        assertTrue(AiRequestViewOperations.inspectionReport(exchange).isEmpty());
        assertEquals(0L, AiRequestViewOperations.currentGeneration(exchange));
        assertTrue(AiRequestViewOperations.snapshot(exchange).isEmpty());
    }

    @Test
    void malformedBodiesStayVisibleAsUnsupportedViews() {
        assertUnsupported("{\"messages\":[", "request body is not valid JSON");
        assertUnsupported("", "request body is empty");
        assertUnsupported("[1,2]", "request body is not a JSON object");
        assertUnsupported("{\"unrelated\":true}", "no known AI protocol fields");
    }

    private void assertUnsupported(final String body, final String expectedReason) {
        MockServerWebExchange exchange = exchange();
        AiNormalizedRequestView view = AiRequestViewOperations.ensurePublished(exchange, body, null);
        assertFalse(view.supported());
        assertEquals(expectedReason, view.unsupportedReason());
        assertNull(view.protocol());
        assertEquals(1L, view.generation());
    }

    @Test
    void anExplicitProtocolIdentityOverridesTheBodyShapeHint() {
        MockServerWebExchange exchange = exchange();

        AiNormalizedRequestView view = AiRequestViewOperations.ensurePublished(exchange, CHAT_BODY, null,
                AiProtocols.OPENAI_RESPONSES);

        assertEquals(AiProtocols.OPENAI_RESPONSES, view.protocol());
        assertFalse(view.supported());
        assertEquals("request body does not match the resolved AI protocol", view.unsupportedReason());
    }

    @Test
    void theUnknownSentinelDoesNotFallBackToTheBodyShapeHint() {
        MockServerWebExchange exchange = exchange();

        AiNormalizedRequestView view = AiRequestViewOperations.ensurePublished(exchange, CHAT_BODY, null,
                AiProtocols.UNKNOWN);

        assertNull(view.protocol());
        assertFalse(view.supported());
        assertEquals("no known AI protocol fields", view.unsupportedReason());
    }

    @Test
    void structuredContentIsNeverFlattenedIntoText() {
        MockServerWebExchange exchange = exchange();
        String body = "{\"model\":\"gpt-4\",\"messages\":[{\"role\":\"user\","
                + "\"content\":[{\"type\":\"text\",\"text\":\"hello\"}]}]}";

        AiNormalizedRequestView view = AiRequestViewOperations.ensurePublished(exchange, body, null);

        assertEquals(1, view.messages().size());
        AiNormalizedRequestView.BoundedMessage message = view.messages().get(0);
        assertEquals(AiNormalizedRequestView.ContentKind.STRUCTURED, message.contentKind());
        assertEquals(-1, message.charCount());
        assertNull(message.textPrefix());
    }

    @Test
    void toolCallMessagesAreDetected() {
        MockServerWebExchange exchange = exchange();
        String body = "{\"messages\":[{\"role\":\"assistant\",\"tool_calls\":[{\"id\":\"1\"}]}]}";

        AiNormalizedRequestView view = AiRequestViewOperations.ensurePublished(exchange, body, null);

        assertTrue(view.toolCallsPresent());
        assertEquals("assistant", view.messages().get(0).role());
        assertEquals(AiNormalizedRequestView.ContentKind.OPAQUE, view.messages().get(0).contentKind());
    }

    @Test
    void theOverviewIsBoundedPerMessageAndInTotal() {
        MockServerWebExchange exchange = exchange();
        StringBuilder messages = new StringBuilder();
        for (int i = 0; i < AiNormalizedRequestView.MAX_MESSAGES + 5; i++) {
            if (i > 0) {
                messages.append(',');
            }
            messages.append("{\"role\":\"user\",\"content\":\"").append("x".repeat(AiNormalizedRequestView.MAX_MESSAGE_CHARS + 1)).append("\"}");
        }
        String body = "{\"messages\":[" + messages + "]}";

        AiNormalizedRequestView view = AiRequestViewOperations.ensurePublished(exchange, body, null);

        assertEquals(AiNormalizedRequestView.MAX_MESSAGES, view.messages().size());
        AiNormalizedRequestView.BoundedMessage first = view.messages().get(0);
        assertTrue(first.truncated());
        assertEquals(AiNormalizedRequestView.MAX_MESSAGE_CHARS, first.textPrefix().length());
        assertEquals(AiNormalizedRequestView.MAX_MESSAGE_CHARS + 1, first.charCount());
    }

    @Test
    void responsesShapeIsStructurallyRecognized() {
        MockServerWebExchange exchange = exchange();
        String body = "{\"model\":\"gpt-4\",\"input\":\"say hi\",\"tools\":[]}";

        AiNormalizedRequestView view = AiRequestViewOperations.ensurePublished(exchange, body, null);

        assertEquals("openai-responses", view.protocol());
        assertEquals("RESPONSE", view.operation());
        assertEquals("gpt-4", view.model());
        assertTrue(view.messages().isEmpty());
        assertFalse(view.toolsPresent());
        assertTrue(view.supported());
    }

    @Test
    void exchangesNeverShareViewState() {
        MockServerWebExchange first = exchange();
        MockServerWebExchange second = exchange();
        AiRequestViewOperations.ensurePublished(first, CHAT_BODY, null);
        AiRequestViewOperations.ensurePublished(second, CHAT_BODY.replace("gpt-4", "gpt-3.5"), null);

        assertEquals(1L, AiRequestViewOperations.currentGeneration(first));
        assertEquals(1L, AiRequestViewOperations.currentGeneration(second));
        assertEquals("gpt-4", AiRequestViewOperations.snapshot(first).orElseThrow().model());
        assertEquals("gpt-3.5", AiRequestViewOperations.snapshot(second).orElseThrow().model());
        assertTrue(AiRequestViewOperations.inspectionReport(first).isEmpty());
        assertTrue(AiRequestViewOperations.inspectionReport(second).isEmpty());
    }

    @Test
    void theWireViewSurvivesLaterEffectiveGenerations() {
        MockServerWebExchange exchange = exchange();
        AiRequestViewOperations.ensurePublished(exchange, CHAT_BODY, null);
        AiRequestViewOperations.ensurePublished(exchange, CHAT_BODY.replace("hello", "changed"), "aiPrompt");

        AiNormalizedRequestView wire = AiRequestViewOperations.wireSnapshot(exchange).orElseThrow();
        assertEquals(1L, wire.generation());
        assertEquals(AiNormalizedRequestView.ViewKind.WIRE, wire.viewKind());
        assertNull(wire.mutator());
        assertEquals("hello", wire.messages().get(1).textPrefix());
    }

    @Test
    void consumerNameIsRequired() {
        MockServerWebExchange exchange = exchange();
        assertThrows(NullPointerException.class,
                () -> AiRequestViewOperations.snapshotAndRecordInspection(exchange, null));
    }

    private MockServerWebExchange exchange() {
        return MockServerWebExchange.from(MockServerHttpRequest.post("/ai/view").build());
    }
}
