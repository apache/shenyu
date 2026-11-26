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
import org.junit.jupiter.params.provider.CsvSource;
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
import static org.junit.jupiter.api.Assertions.assertTrue;

class AiUpstreamUsageTapTest {

    @Test
    void fragmentedJsonPreservesBytesAndDoesNotInferMissingTotal() {
        final AiUsage usage = read("{\"text\":\"秘密\",\"usage\":{\"prompt_tokens\":0,\"completion_tokens\":3}}", false, Map.of());
        assertEquals(AiUsage.Status.REPORTED, usage.status());
        assertEquals(0L, usage.promptTokens());
        assertEquals(3L, usage.completionTokens());
        assertNull(usage.totalTokens());
    }

    @Test
    void extractsSupportedCacheCategoriesAndBoundedProviderCounts() {
        final AiUsage usage = read("{\"usage\":{\"prompt_tokens\":10,\"completion_tokens\":2,\"total_tokens\":12,"
                + "\"prompt_tokens_details\":{\"cached_tokens\":4},\"reasoning_tokens\":1}}", false, Map.of());
        assertEquals(AiUsage.Status.REPORTED, usage.status());
        assertEquals(4L, usage.cachedPromptTokens());
        assertEquals(Map.of("reasoning_tokens", 1L), usage.providerCounts());
        assertEquals(AiUsage.Source.PROVIDER_REPORTED, usage.source());
        assertEquals(AiUsage.Aggregation.CUMULATIVE_SNAPSHOT, usage.aggregation());
    }

    @Test
    void acceptsDeepSeekCacheAliasWithoutInventingACacheCount() {
        final AiUsage usage = read("{\"usage\":{\"prompt_tokens\":10,\"prompt_cache_hit_tokens\":3}}", false, Map.of());
        assertEquals(3L, usage.cachedPromptTokens());
        assertTrue(usage.providerCounts().isEmpty());
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
    @ValueSource(strings = {"cached_tokens", "prompt_cache_hit_tokens", "prompt_tokens_details.cached_tokens"})
    void invalidCacheCountsInvalidateSnapshotWithoutEstimates(final String field) {
        String body = field.startsWith("prompt_tokens_details.")
                ? "{\"usage\":{\"prompt_tokens_details\":{\"cached_tokens\":\"bad\"}}}"
                : "{\"usage\":{\"" + field + "\":\"bad\"}}";
        assertEquals(AiUsage.Status.INVALID, read(body, false, Map.of()).status());
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
        return read(body, streaming, headers, "openai-chat");
    }

    private AiUsage read(final String body, final boolean streaming, final Map<String, List<String>> headers, final String protocol) {
        final byte[] bytes = body.getBytes(StandardCharsets.UTF_8);
        final List<byte[]> chunks = new ArrayList<>();
        // Split within UTF-8 codepoints and every JSON/SSE boundary.
        for (int offset = 0; offset < bytes.length; offset++) {
            chunks.add(Arrays.copyOfRange(bytes, offset, offset + 1));
        }
        return read(chunks, streaming, headers, protocol);
    }

    private AiUsage read(final List<byte[]> chunks, final boolean streaming, final Map<String, List<String>> headers) {
        return read(chunks, streaming, headers, "openai-chat");
    }

    private AiUsage read(final List<byte[]> chunks, final boolean streaming, final Map<String, List<String>> headers, final String protocol) {
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
        StepVerifier.create(new AiUpstreamUsageTap(attempt, headers, streaming, protocol).attach(response).getBody().doOnNext(received::add).then()).verifyComplete();
        attempt.finish(SignalType.ON_COMPLETE, null);
        owner.finish(SignalType.ON_COMPLETE, 200);
        assertEquals(1, subscriptions.get());
        assertEquals(chunks.size(), received.size());
        for (int index = 0; index < chunks.size(); index++) {
            assertSame(chunks.get(index), received.get(index));
        }
        return events.get(0).usage();
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void responsesUsesNativeInputOutputBeforeClientConversion(final boolean streaming) {
        String response = "{\"usage\":{\"input_tokens\":3,\"output_tokens\":12,\"total_tokens\":15,\"input_tokens_details\":{\"cached_tokens\":0}}}";
        String wire = streaming ? "data: {\"type\":\"response.completed\",\"response\":" + response + "}\n\n" : response;
        AiUsage actual = read(wire, streaming, Map.of(), "openai-responses");
        assertEquals(3L, actual.promptTokens());
        assertEquals(12L, actual.completionTokens());
        assertEquals(15L, actual.totalTokens());
        assertEquals(0L, actual.cachedPromptTokens());
    }

    @ParameterizedTest
    @ValueSource(strings = {"response.completed", "response.incomplete", "response.failed"})
    void terminalResponsesUsageIsOneSnapshotNotAnIncrement(final String event) {
        String wire = "data: {\"type\":\"response.created\",\"response\":{\"usage\":{\"input_tokens\":99,\"output_tokens\":99}}}\n\n"
                + "data: {\"type\":\"" + event + "\",\"response\":{\"usage\":{\"input_tokens\":3,\"output_tokens\":12}}}\n\n";
        AiUsage actual = read(wire, true, Map.of(), "openai-responses");
        assertEquals(3L, actual.promptTokens());
        assertEquals(12L, actual.completionTokens());
        assertNull(actual.totalTokens());
    }

    @Test
    void anthropicSparseCumulativeUpdatesRetainInputButNeverSumOutput() {
        String start = "data: {\"type\":\"message_start\",\"message\":{\"usage\":{\"input_tokens\":3,\"output_tokens\":1,"
                + "\"cache_read_input_tokens\":0,\"cache_creation_input_tokens\":0}}}\n\n";
        String wire = start + "data: {\"type\":\"message_delta\",\"usage\":{\"output_tokens\":5}}\n\n"
                + "data: {\"type\":\"message_delta\",\"usage\":{\"output_tokens\":12}}\n\n"
                + "data: {\"type\":\"message_stop\"}\n\n";
        AiUsage actual = read(wire, true, Map.of(), "anthropic-messages");
        assertEquals(3L, actual.promptTokens());
        assertEquals(12L, actual.completionTokens());
        assertEquals(0L, actual.cachedPromptTokens());
        assertEquals(0L, actual.providerCounts().get("cache_creation_input_tokens"));
        assertNull(actual.totalTokens());
        AiUsage zero = read(start + "data: {\"type\":\"message_delta\",\"usage\":{\"input_tokens\":0,\"output_tokens\":0}}\n\n", true, Map.of(), "anthropic-messages");
        assertEquals(0L, zero.promptTokens());
        assertEquals(0L, zero.completionTokens());
    }

    @Test
    void anthropicCacheAndServiceUnitsAreNotSilentlyPricedAsRegularTokens() {
        AiUsage actual = read("{\"usage\":{\"input_tokens\":3,\"output_tokens\":12,\"cache_read_input_tokens\":5,"
                + "\"cache_creation_input_tokens\":7,\"server_tool_use\":{\"web_search_requests\":1}}}", false, Map.of(), "anthropic-messages");
        assertEquals(3L, actual.promptTokens());
        assertEquals(5L, actual.cachedPromptTokens());
        assertEquals(7L, actual.providerCounts().get("cache_creation_input_tokens"));
        assertEquals(1L, actual.providerCounts().get("server_tool_use.web_search_requests"));
    }

    @ParameterizedTest
    @CsvSource({"openai-responses,input_tokens", "anthropic-messages,output_tokens", "anthropic-messages,cache_read_input_tokens"})
    void malformedNativeCountersDoNotBecomeZero(final String protocol, final String field) {
        AiUsage actual = read("{\"usage\":{\"" + field + "\":-1}}", false, Map.of(), protocol);
        assertEquals(AiUsage.Status.INVALID, actual.status());
        assertNull(actual.promptTokens());
    }
}
