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

package org.apache.shenyu.plugin.ai.proxy.enhanced.service;

import org.apache.shenyu.common.dto.AgentRequestContext;
import org.apache.shenyu.common.metrics.AiAttemptEvent;
import org.apache.shenyu.common.metrics.AiRequestObservation;
import org.apache.shenyu.common.metrics.AiStreamCompletion;
import org.apache.shenyu.common.metrics.AiTerminalEvent;
import org.apache.shenyu.common.metrics.AiUsage;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.junit.jupiter.api.Test;
import reactor.core.publisher.Flux;
import reactor.core.publisher.SignalType;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

/**
 * Multi-protocol usage metering for the upstream tap (issue #8458): Anthropic
 * Messages and Gemini generateContent responses, streaming and not.
 */
class AiUpstreamUsageTapMultiProtocolTest {

    @Test
    void anthropicBodyReportsInputAndOutputWithoutInventingTotal() {
        final AiUsage usage = read("{\"model\":\"claude\",\"content\":[{\"type\":\"text\",\"text\":\"hi\"}],"
                + "\"usage\":{\"input_tokens\":12,\"output_tokens\":34},\"stop_reason\":\"end_turn\"}",
                false, "anthropic-messages", Map.of()).usage();
        assertEquals(AiUsage.Status.REPORTED, usage.status());
        assertEquals(12L, usage.promptTokens());
        assertEquals(34L, usage.completionTokens());
        assertNull(usage.totalTokens());
    }

    @Test
    void anthropicStreamReplacesSnapshotsAndMessageStopCompletesTheStream() {
        final String body = "event: message_start\ndata: {\"type\":\"message_start\",\"message\":{\"usage\":{\"input_tokens\":12,\"output_tokens\":1}}}\n\n"
                + "event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"delta\":{\"type\":\"text_delta\",\"text\":\"hi\"}}\n\n"
                + "event: message_delta\ndata: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":34}}\n\n"
                + "event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n";
        final AiTerminalEvent event = read(body, true, "anthropic-messages", Map.of());
        assertEquals(AiUsage.Status.REPORTED, event.usage().status());
        assertEquals(12L, event.usage().promptTokens());
        assertEquals(34L, event.usage().completionTokens());
        assertNull(event.usage().totalTokens());
        assertEquals(AiStreamCompletion.Evidence.UPSTREAM_MARKER, event.completion().evidence());
    }

    @Test
    void geminiBodyReportsTheThreeUsageMetadataCounters() {
        final AiUsage usage = read("{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"hi\"}],\"role\":\"model\"}}],"
                + "\"usageMetadata\":{\"promptTokenCount\":7,\"candidatesTokenCount\":3,\"totalTokenCount\":10}}",
                false, "gemini-generate-content", Map.of()).usage();
        assertEquals(AiUsage.Status.REPORTED, usage.status());
        assertEquals(7L, usage.promptTokens());
        assertEquals(3L, usage.completionTokens());
        assertEquals(10L, usage.totalTokens());
    }

    @Test
    void geminiStreamTakesTheFinalUsageMetadataSnapshot() {
        final String body = "data: {\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"he\"}]}}],"
                + "\"usageMetadata\":{\"promptTokenCount\":7,\"candidatesTokenCount\":2,\"totalTokenCount\":9}}\n\n"
                + "data: {\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"y\"}]}}],"
                + "\"usageMetadata\":{\"promptTokenCount\":7,\"candidatesTokenCount\":3,\"totalTokenCount\":10}}\n\n";
        final AiUsage usage = read(body, true, "gemini-generate-content", Map.of()).usage();
        assertEquals(AiUsage.Status.REPORTED, usage.status());
        assertEquals(7L, usage.promptTokens());
        assertEquals(3L, usage.completionTokens());
        assertEquals(10L, usage.totalTokens());
    }

    private AiTerminalEvent read(final String body, final boolean streaming, final String protocol, final Map<String, List<String>> headers) {
        final byte[] bytes = body.getBytes(StandardCharsets.UTF_8);
        final List<byte[]> chunks = new ArrayList<>();
        for (int offset = 0; offset < bytes.length; offset++) {
            chunks.add(Arrays.copyOfRange(bytes, offset, offset + 1));
        }
        return read(chunks, streaming, protocol, headers);
    }

    private AiTerminalEvent read(final List<byte[]> chunks, final boolean streaming, final String protocol, final Map<String, List<String>> headers) {
        final List<AiTerminalEvent> events = new ArrayList<>();
        final AgentRequestContext context = new AgentRequestContext("request", "llm", null, null);
        final AiRequestObservation owner = new AiRequestObservation(events::add, context);
        final AiRequestObservation.Attempt attempt = owner.startAttempt(context, AiAttemptEvent.Kind.PRIMARY, "upstream", protocol, streaming);
        final AtomicInteger subscriptions = new AtomicInteger();
        final AiUpstreamResponse response = new AiUpstreamResponse(200, headers, Flux.defer(() -> {
            subscriptions.incrementAndGet();
            return Flux.fromIterable(chunks);
        }));
        final List<byte[]> received = new ArrayList<>();
        StepVerifier.create(new AiUpstreamUsageTap(attempt, headers, streaming, protocol).attach(response).getBody()
                .doOnNext(received::add).then()).verifyComplete();
        attempt.finish(SignalType.ON_COMPLETE, null);
        owner.finish(SignalType.ON_COMPLETE, 200);
        assertEquals(1, subscriptions.get());
        assertEquals(chunks.size(), received.size());
        for (int index = 0; index < chunks.size(); index++) {
            assertSame(chunks.get(index), received.get(index));
        }
        return events.get(0);
    }
}
