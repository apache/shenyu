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

package org.apache.shenyu.plugin.ai.proxy.enhanced.protocol;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import io.github.agenticai.connect.ProviderProfile;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.junit.jupiter.api.Test;
import reactor.core.publisher.Flux;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Library contracts at ShenYu's JSON/reactive boundary, without live-provider calls.
 */
class ConnectProtocolAdapterTest {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    private static final String RESPONSE = "{\"id\":\"chatcmpl-test\",\"model\":\"upstream\",\"choices\":[{\"index\":0,"
            + "\"message\":{\"role\":\"assistant\",\"content\":\"hello\"},\"finish_reason\":\"stop\"}],"
            + "\"usage\":{\"prompt_tokens\":3,\"completion_tokens\":2,\"total_tokens\":5}}";

    private static final String CHUNK = "{\"id\":\"chatcmpl-test\",\"model\":\"upstream\",\"choices\":[{\"index\":0,"
            + "\"delta\":{\"role\":\"assistant\",\"content\":\"hello\"},\"finish_reason\":null}]}";

    @Test
    void delegatesRequestConversionAndProviderAdaptationExactlyOnce() throws Exception {
        AtomicInteger calls = new AtomicInteger();
        ProviderProfile profile = (payload, adaptation) -> {
            calls.incrementAndGet();
            assertEquals("routed", payload.get("model").getAsString());
            payload.addProperty("custom", true);
            adaptation.warning("custom_profile", "custom", "Test profile");
        };
        ObjectNode input = object("{\"model\":\"alias\",\"instructions\":\"be helpful\",\"input\":\"hello\",\"max_output_tokens\":12}");
        ObjectNode before = input.deepCopy();
        ObjectNode result = new ConnectProtocolAdapter(OpenAiResponses.NAME, "routed", profile).convertRequest(input);
        assertEquals(1, calls.get());
        assertEquals(before, input);
        assertEquals("system", result.at("/messages/0/role").asText());
        assertEquals("hello", result.at("/messages/1/content").asText());
        assertEquals(12, result.get("max_completion_tokens").asInt());
        assertTrue(result.get("custom").asBoolean());
    }

    @Test
    void keepsGenericIdentityFieldsAndRejectsUnrepresentableResponsesState() throws Exception {
        ObjectNode input = object("{\"messages\":[{\"role\":\"user\"}],\"vendor\":true,\"content\":null}");
        assertEquals(input, new ConnectProtocolAdapter(OpenAiChat.NAME, null, null).convertRequest(input));
        ConnectProtocolAdapter adapter = adapter();
        for (String body : List.of("{\"model\":\"x\",\"input\":\"hi\",\"previous_response_id\":\"resp_old\"}", "{\"input\":\"hi\"}")) {
            AiProxyException error = assertThrows(AiProxyException.class, () -> adapter.convertRequest(object(body)));
            assertEquals(AiProxyException.Kind.REQUEST, error.getKind());
        }
    }

    @Test
    void adaptsDeepSeekTokensAndReasoningWithoutGuessingFromAModel() throws Exception {
        ObjectNode input = object("{\"model\":\"deepseek-model\",\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],"
                + "\"max_completion_tokens\":20,\"max_tokens\":10,\"reasoning_effort\":\"medium\"}");
        assertEquals(input, new ConnectProtocolAdapter(OpenAiChat.NAME, null, null).convertRequest(input));
        ObjectNode result = new ConnectProtocolAdapter(OpenAiChat.NAME, null, ProviderProfile.deepSeekChat()).convertRequest(input);
        assertEquals(20, result.get("max_tokens").asInt());
        assertFalse(result.has("max_completion_tokens"));
        assertEquals("high", result.get("reasoning_effort").asText());
        assertEquals("enabled", result.at("/thinking/type").asText());
        input.put("tool_choice", "required");
        assertThrows(AiProxyException.class,
                () -> new ConnectProtocolAdapter(OpenAiChat.NAME, null, ProviderProfile.deepSeekChat()).convertRequest(input));
    }

    @Test
    void convertsCompleteResponsesAndRemovesStaleRepresentationHeaders() throws Exception {
        AiUpstreamResponse upstream = new AiUpstreamResponse(200, Map.of("content-length", List.of("123"), "ETag", List.of("old"),
                "Content-MD5", List.of("old"), "Digest", List.of("old"), "X-Request-ID", List.of("kept")),
                Flux.just(bytes(RESPONSE.substring(0, 20)), bytes(RESPONSE.substring(20))));
        AiUpstreamResponse converted = adapter().convertResponse(upstream, false);
        assertEquals(Map.of("Content-Type", List.of("application/json"), "X-Request-ID", List.of("kept")), converted.getHeaders());
        JsonNode result = MAPPER.readTree(converted.getBody().single().block(Duration.ofSeconds(3)));
        assertEquals("response", result.get("object").asText());
        assertEquals("hello", result.at("/output/0/content/0/text").asText());
        assertEquals(3, result.at("/usage/input_tokens").asInt());
        assertEquals(2, result.at("/usage/output_tokens").asInt());
    }

    @Test
    void restoresCustomToolsUsingTheOriginalRequestContext() throws Exception {
        ConnectProtocolAdapter adapter = adapter();
        ObjectNode converted = adapter.convertRequest(object("{\"model\":\"x\",\"input\":\"hi\",\"tools\":[{\"type\":\"custom\",\"name\":\"execute\"}]}"));
        String name = converted.at("/tools/0/function/name").asText();
        String body = "{\"model\":\"x\",\"choices\":[{\"index\":0,\"finish_reason\":\"tool_calls\",\"message\":{\"role\":\"assistant\","
                + "\"tool_calls\":[{\"id\":\"call_one\",\"type\":\"function\",\"function\":{\"name\":\"" + name + "\",\"arguments\":\"{\\\"input\\\":\\\"print(1)\\\"}\"}}]}}]}";
        JsonNode result = MAPPER.readTree(adapter.convertResponse(response(200, Flux.just(bytes(body))), false).getBody().single().block());
        assertEquals("custom_tool_call", result.at("/output/0/type").asText());
        assertEquals("execute", result.at("/output/0/name").asText());
        assertEquals("print(1)", result.at("/output/0/input").asText());
        JsonNode unrelated = MAPPER.readTree(adapter().convertResponse(response(200, Flux.just(bytes(body))), false).getBody().single().block());
        assertEquals("function_call", unrelated.at("/output/0/type").asText());
    }

    @Test
    void restoresFragmentedCustomToolStreamsWithoutCrossRequestState() throws Exception {
        ConnectProtocolAdapter custom = adapter();
        ObjectNode request = custom.convertRequest(object("{\"model\":\"x\",\"input\":\"hi\",\"tools\":[{\"type\":\"custom\",\"name\":\"execute\"}]}"));
        String name = request.at("/tools/0/function/name").asText();
        ObjectNode start = object("{\"choices\":[{\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"call_one\",\"type\":\"function\",\"function\":{}}]}}]}");
        ObjectNode function = (ObjectNode) start.at("/choices/0/delta/tool_calls/0/function");
        function.put("name", name);
        function.put("arguments", "{\"input\":\"print(");
        ObjectNode next = start.deepCopy();
        ObjectNode continuation = (ObjectNode) next.at("/choices/0/delta/tool_calls/0");
        continuation.remove(List.of("id", "type"));
        ((ObjectNode) continuation.get("function")).remove("name");
        ((ObjectNode) continuation.get("function")).put("arguments", "1)\"}");
        Flux<byte[]> chunks = Flux.just(bytes("data: " + start + "\n\n"), bytes("data: " + next + "\n\n"),
                bytes("data: {\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"tool_calls\"}]}\n\n"));
        Flux<String> restored = custom.convertResponse(response(200, chunks), true).getBody().map(bytes -> new String(bytes, StandardCharsets.UTF_8));
        Flux<String> isolated = adapter().convertResponse(response(200, chunks), true).getBody().map(bytes -> new String(bytes, StandardCharsets.UTF_8));
        List<List<String>> results = reactor.core.publisher.Mono.zip(restored.collectList(), isolated.collectList(), List::of).block(Duration.ofSeconds(3));
        JsonNode customResult = terminalResponse(results.get(0));
        JsonNode genericResult = terminalResponse(results.get(1));
        assertEquals("custom_tool_call", customResult.at("/response/output/0/type").asText());
        assertEquals("print(1)", customResult.at("/response/output/0/input").asText());
        assertEquals("function_call", genericResult.at("/response/output/0/type").asText());
        assertEquals("{\"input\":\"print(1)\"}", genericResult.at("/response/output/0/arguments").asText());
    }

    private JsonNode terminalResponse(final List<String> events) throws Exception {
        String terminal = events.get(events.size() - 1);
        return MAPPER.readTree(terminal.substring(terminal.indexOf("data: ") + 6).trim());
    }

    @Test
    void preservesHttpErrorsAndChatResponsesWithoutConsumingThem() {
        for (int status : List.of(199, 400, 500)) {
            AiUpstreamResponse upstream = response(status, Flux.error(new AssertionError("Body must not be consumed")));
            assertSame(upstream, adapter().convertResponse(upstream, false));
            assertSame(upstream, adapter().convertResponse(upstream, true));
        }
        AiUpstreamResponse upstream = response(200, Flux.empty());
        assertSame(upstream, new ConnectProtocolAdapter(OpenAiChat.NAME, null, null).convertResponse(upstream, true));
    }

    @Test
    void rejectsMalformedEmptyTrailingAndOversizedCompleteResponses() {
        for (String body : List.of("invalid", "[]", "", "null", RESPONSE + " {}", "x".repeat(5 * 1024 * 1024 + 1))) {
            StepVerifier.create(adapter().convertResponse(response(200, Flux.just(bytes(body))), false).getBody())
                    .expectErrorMatches(error -> error instanceof AiProxyException && ((AiProxyException) error).getKind() == AiProxyException.Kind.PROTOCOL).verify();
        }
        StepVerifier.create(adapter().convertResponse(response(200, Flux.empty()), false).getBody()).expectError(AiProxyException.class).verify();
    }

    @Test
    void convertsFragmentedMultilineSseAndProducesOneTerminalEvent() {
        String wire = ": heartbeat\r\ndata: {\r\ndata: " + CHUNK.substring(1) + "\r\n\r\n"
                + "data: {\"choices\":[],\"usage\":{\"prompt_tokens\":3,\"completion_tokens\":2}}\r\n\r\ndata: [DONE]\r\n\r\n";
        List<byte[]> fragments = new ArrayList<>();
        for (byte value : bytes(wire)) {
            fragments.add(new byte[] {value});
        }
        List<String> events = stream(Flux.fromIterable(fragments)).collectList().block(Duration.ofSeconds(3));
        assertTrue(events.stream().anyMatch(event -> event.startsWith("event: response.output_text.delta\n") && event.contains("hello")));
        assertEquals(1, events.stream().filter(event -> event.startsWith("event: response.completed\n")).count());
        assertTrue(events.get(events.size() - 1).contains("\"input_tokens\":3"));
        assertTrue(events.stream().allMatch(event -> event.contains("\"sequence_number\":") && !event.contains("[DONE]")));
    }

    @Test
    void isolatesStreamStateOnResubscriptionAndCancelsWithoutATerminalEvent() {
        Flux<String> reusable = stream(Flux.just(bytes("data: " + CHUNK + "\n\n")));
        for (int attempt = 0; attempt < 2; attempt++) {
            List<String> events = reusable.collectList().block();
            assertEquals(1, events.stream().filter(event -> event.startsWith("event: response.completed\n")).count());
        }
        AtomicBoolean cancelled = new AtomicBoolean();
        Flux<String> live = stream(Flux.just(bytes("data: " + CHUNK + "\n\n")).concatWith(Flux.never()).doOnCancel(() -> cancelled.set(true)));
        StepVerifier.create(live, 0).thenRequest(1).expectNextMatches(event -> event.startsWith("event: response.created\n"))
                .thenCancel().verify(Duration.ofSeconds(3));
        assertTrue(cancelled.get());
    }

    @Test
    void doesNotSynthesizeSuccessAfterMalformedEventsOrTransportFailure() {
        for (Flux<byte[]> body : List.of(Flux.just(bytes("data: " + CHUNK + "\n\ndata: invalid\n\n")),
                Flux.just(bytes("data: " + CHUNK + "\n\n")).concatWith(Flux.error(new IllegalStateException("socket closed"))))) {
            List<String> events = new ArrayList<>();
            StepVerifier.create(stream(body).doOnNext(events::add)).thenConsumeWhile(event -> !event.startsWith("event: response.completed\n"))
                    .expectError(AiProxyException.class).verify();
            assertTrue(events.stream().noneMatch(event -> event.startsWith("event: response.completed\n")));
        }
        AiUpstreamResponse json = new AiUpstreamResponse(200, Map.of("Content-Type", List.of("application/json")), Flux.just(bytes(RESPONSE)));
        StepVerifier.create(adapter().convertResponse(json, true).getBody()).expectError(AiProxyException.class).verify();
    }

    @Test
    void boundsCumulativeConversionStateAndCancelsAtDone() {
        String large = "data: " + CHUNK.replace("hello", "x".repeat(1024 * 1024)) + "\n\n";
        StepVerifier.create(stream(Flux.range(0, 6).map(index -> bytes(large))))
                .thenConsumeWhile(event -> !event.startsWith("event: response.completed\n")).expectError(AiProxyException.class).verify();
        AtomicBoolean cancelled = new AtomicBoolean();
        List<String> events = stream(Flux.just(bytes("data: [DONE]\n\n")).concatWith(Flux.never()).doOnCancel(() -> cancelled.set(true)))
                .collectList().block(Duration.ofSeconds(3));
        assertTrue(cancelled.get());
        assertEquals(1, events.stream().filter(event -> event.startsWith("event: response.completed\n")).count());
    }

    private ConnectProtocolAdapter adapter() {
        return new ConnectProtocolAdapter(OpenAiResponses.NAME, null, null);
    }

    private ObjectNode object(final String json) throws Exception {
        return (ObjectNode) MAPPER.readTree(json);
    }

    private AiUpstreamResponse response(final int status, final Flux<byte[]> body) {
        return new AiUpstreamResponse(status, Map.of(), body);
    }

    private Flux<String> stream(final Flux<byte[]> body) {
        return adapter().convertResponse(response(200, body), true).getBody().map(bytes -> new String(bytes, StandardCharsets.UTF_8));
    }

    private byte[] bytes(final String value) {
        return value.getBytes(StandardCharsets.UTF_8);
    }
}
