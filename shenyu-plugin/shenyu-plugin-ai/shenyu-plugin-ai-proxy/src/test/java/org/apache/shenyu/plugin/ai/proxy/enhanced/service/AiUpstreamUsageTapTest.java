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
import org.apache.shenyu.common.metrics.AiTerminalEvent;
import org.apache.shenyu.common.metrics.AiUsage;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
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

class AiUpstreamUsageTapTest {

    @Test
    void fragmentedJsonPreservesBytesAndDoesNotInferMissingTotal() {
        final AiUsage usage = read("{\"text\":\"秘密\",\"usage\":{\"prompt_tokens\":0,\"completion_tokens\":3}}", false, Map.of());
        assertEquals(AiUsage.Status.REPORTED, usage.status());
        assertEquals(0L, usage.promptTokens());
        assertEquals(3L, usage.completionTokens());
        assertNull(usage.totalTokens());
    }

    @ParameterizedTest
    @ValueSource(strings = {"{}", "{\"usage\":null}", "{\"usage\":{}}", "{\"usage\":{\"prompt_tokens\":null}}"})
    void absentUsageIsUnknownNotZero(final String body) {
        final AiUsage usage = read(body, false, Map.of());
        assertEquals(AiUsage.Status.UNKNOWN, usage.status());
        assertNull(usage.promptTokens());
        assertNull(usage.completionTokens());
        assertNull(usage.totalTokens());
    }

    @ParameterizedTest
    @ValueSource(strings = {"-1", "1.5", "\"2\"", "true", "9223372036854775808", "[]"})
    void invalidCountersInvalidateSnapshotWithoutEstimates(final String count) {
        final AiUsage usage = read("{\"usage\":{\"prompt_tokens\":5,\"completion_tokens\":" + count + "}}", false, Map.of());
        assertEquals(AiUsage.Status.INVALID, usage.status());
        assertNull(usage.promptTokens());
        assertNull(usage.completionTokens());
    }

    @ParameterizedTest
    @ValueSource(strings = {"{\"usage\":[]}", "{\"usage\":{\"total_tokens\":1,\"total_tokens\":2}}", "{}{}", "not JSON"})
    void rejectsAmbiguousOrMalformedUsage(final String body) {
        assertEquals(AiUsage.Status.INVALID, read(body, false, Map.of()).status());
    }

    @Test
    void sseSupportsUtf8ByteSplitsMultilineCrLfAndLastCompleteSnapshot() {
        final String body = ": keepalive\r\nevent: message\r\ndata: {\"text\":\"秘密\",\r\ndata: \"usage\":{\"prompt_tokens\":1,\"total_tokens\":9}}\r\n\r\n"
                + "data: {\"usage\":{\"completion_tokens\":4}}\n\n"
                + "data: {\"choices\":[]}\n\ndata: [DONE]\n\n";
        final AiUsage usage = read(body, true, Map.of());
        assertEquals(AiUsage.Status.REPORTED, usage.status());
        assertNull(usage.promptTokens());
        assertEquals(4L, usage.completionTokens());
        assertNull(usage.totalTokens());
    }

    @Test
    void incompleteSseEventAtEofIsNotInventedAsFinalUsage() {
        assertEquals(AiUsage.Status.UNKNOWN, read("data: {\"usage\":{\"total_tokens\":7}}\n", true, Map.of()).status());
    }

    @Test
    void capsJsonAndEachSseEventRatherThanEntireStream() {
        assertEquals(AiUsage.Status.LIMIT_EXCEEDED, read("x".repeat(AiUpstreamUsageTap.MAX_BYTES + 1), false, Map.of()).status());
        assertEquals(AiUsage.Status.LIMIT_EXCEEDED, read("data: " + "x".repeat(AiUpstreamUsageTap.MAX_BYTES), true, Map.of()).status());
        final String many = "data: {\"choices\":[]}\n\n".repeat(4000) + "data: {\"usage\":{\"total_tokens\":7}}\n\n";
        assertEquals(7L, read(many, true, Map.of()).totalTokens());
    }

    @Test
    void contentTypeOverridesRequestedStreamingAndEncodedBodiesAreNotDecoded() {
        final String json = "{\"usage\":{\"total_tokens\":7}}";
        assertEquals(7L, read(json, true, Map.of("content-type", List.of("application/json; charset=utf-8"))).totalTokens());
        assertEquals(AiUsage.Status.UNSUPPORTED, read(json, false, Map.of("Content-Encoding", List.of("gzip"))).status());
        assertEquals(AiUsage.Status.UNSUPPORTED, read(json, false, Map.of("Content-Type", List.of("text/plain"))).status());
    }

    @Test
    void invalidUtf8InSseDoesNotChangeWireBytes() {
        final byte[] body = new byte[] {'d', 'a', 't', 'a', ':', (byte) 0xc3, 0x28, '\n', '\n'};
        assertEquals(AiUsage.Status.INVALID, read(List.of(body), true, Map.of()).status());
    }

    private AiUsage read(final String body, final boolean streaming, final Map<String, List<String>> headers) {
        final byte[] bytes = body.getBytes(StandardCharsets.UTF_8);
        final List<byte[]> chunks = new ArrayList<>();
        // Split within UTF-8 codepoints and every JSON/SSE boundary.
        for (int offset = 0; offset < bytes.length; offset++) {
            chunks.add(Arrays.copyOfRange(bytes, offset, offset + 1));
        }
        return read(chunks, streaming, headers);
    }

    private AiUsage read(final List<byte[]> chunks, final boolean streaming, final Map<String, List<String>> headers) {
        final List<AiTerminalEvent> events = new ArrayList<>();
        final AgentRequestContext context = new AgentRequestContext("request", "llm", null, null);
        final AiRequestObservation owner = new AiRequestObservation(events::add, context);
        final AiRequestObservation.Attempt attempt = owner.startAttempt(context, AiAttemptEvent.Kind.PRIMARY, "openai", "openai-chat", streaming);
        final AtomicInteger subscriptions = new AtomicInteger();
        final AiUpstreamResponse response = new AiUpstreamResponse(200, headers, Flux.defer(() -> {
            subscriptions.incrementAndGet();
            return Flux.fromIterable(chunks);
        }));
        final List<byte[]> received = new ArrayList<>();
        StepVerifier.create(new AiUpstreamUsageTap(attempt, headers, streaming).attach(response).getBody().doOnNext(received::add).then()).verifyComplete();
        attempt.finish(SignalType.ON_COMPLETE, null);
        owner.finish(SignalType.ON_COMPLETE, 200);
        assertEquals(1, subscriptions.get());
        assertEquals(chunks.size(), received.size());
        for (int index = 0; index < chunks.size(); index++) {
            assertSame(chunks.get(index), received.get(index));
        }
        return events.get(0).usage();
    }
}
