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

package org.apache.shenyu.plugin.ai.proxy.enhanced.conversion;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiResponses;
import org.junit.jupiter.api.Test;
import reactor.core.publisher.Flux;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ConnectResponsesToChatConverterTest {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    private static final String RESPONSE = "{\"id\":\"chatcmpl-test\",\"model\":\"upstream\",\"choices\":[{\"index\":0,"
            + "\"message\":{\"role\":\"assistant\",\"content\":\"hello\"},\"finish_reason\":\"stop\"}],"
            + "\"usage\":{\"prompt_tokens\":3,\"completion_tokens\":2,\"total_tokens\":5}}";

    private static final String CHUNK = "{\"id\":\"chatcmpl-test\",\"model\":\"upstream\",\"choices\":[{\"index\":0,"
            + "\"delta\":{\"role\":\"assistant\",\"content\":\"hello\"},\"finish_reason\":null}]}";

    @Test
    void testConvertsResponsesRequestWithoutMutatingSource() throws Exception {
        ShenyuAiRequest source = request("{\"model\":\"test\",\"instructions\":\"be helpful\","
                + "\"input\":\"hello\",\"max_output_tokens\":12}");
        final ObjectNode before = source.getRawPayload().deepCopy();

        AiProtocolConversion conversion = new ConnectResponsesToChatConverter().convert(source, false);

        assertEquals(OpenAiChat.NAME, conversion.request().getProtocol());
        assertEquals("system", conversion.request().getRawPayload().at("/messages/0/role").asText());
        assertEquals("hello", conversion.request().getRawPayload().at("/messages/1/content").asText());
        assertEquals(12, conversion.request().getRawPayload().get("max_completion_tokens").asInt());
        assertEquals(before, source.getRawPayload());
    }

    @Test
    void testUsesRequestContextForReverseResponseConversion() throws Exception {
        AiProtocolConversion conversion = new ConnectResponsesToChatConverter().convert(request(
                "{\"model\":\"test\",\"input\":\"hi\",\"tools\":[{\"type\":\"custom\",\"name\":\"execute\"}]}"), false);
        String mappedName = conversion.request().getRawPayload().at("/tools/0/function/name").asText();
        String body = "{\"model\":\"test\",\"choices\":[{\"index\":0,\"finish_reason\":\"tool_calls\","
                + "\"message\":{\"role\":\"assistant\",\"tool_calls\":[{\"id\":\"call_one\",\"type\":\"function\","
                + "\"function\":{\"name\":\"" + mappedName
                + "\",\"arguments\":\"{\\\"input\\\":\\\"print(1)\\\"}\"}}]}}]}";

        AiUpstreamResponse converted = conversion.responseAdapter().adapt(response(200, Flux.just(bytes(body))));
        JsonNode result = MAPPER.readTree(converted.getBody().single().block(Duration.ofSeconds(3)));

        assertEquals("custom_tool_call", result.at("/output/0/type").asText());
        assertEquals("execute", result.at("/output/0/name").asText());
        assertEquals("print(1)", result.at("/output/0/input").asText());
    }

    @Test
    void testConvertsCompleteResponseAndRepresentationHeaders() throws Exception {
        AiProtocolConversion conversion = new ConnectResponsesToChatConverter().convert(
                request("{\"model\":\"test\",\"input\":\"hello\"}"), false);
        AiUpstreamResponse upstream = new AiUpstreamResponse(200,
                Map.of("content-length", List.of("123"), "ETag", List.of("old"),
                        "X-Request-ID", List.of("kept")), Flux.just(bytes(RESPONSE)));

        AiUpstreamResponse converted = conversion.responseAdapter().adapt(upstream);
        JsonNode result = MAPPER.readTree(converted.getBody().single().block(Duration.ofSeconds(3)));

        assertEquals(Map.of("Content-Type", List.of("application/json"),
                "X-Request-ID", List.of("kept")), converted.getHeaders());
        assertEquals("response", result.get("object").asText());
        assertEquals("hello", result.at("/output/0/content/0/text").asText());
    }

    @Test
    void testPreservesHttpErrorsWithoutConsumingBody() throws Exception {
        AiProtocolConversion conversion = new ConnectResponsesToChatConverter().convert(
                request("{\"model\":\"test\",\"input\":\"hello\"}"), false);
        AiUpstreamResponse upstream = response(400, Flux.error(new AssertionError("Body must not be consumed")));

        assertSame(upstream, conversion.responseAdapter().adapt(upstream));
    }

    @Test
    void testIsolatesStreamingStateOnEachSubscription() throws Exception {
        AiProtocolConversion conversion = new ConnectResponsesToChatConverter().convert(
                request("{\"model\":\"test\",\"input\":\"hello\",\"stream\":true}"), true);
        AiUpstreamResponse upstream = response(200, Flux.just(bytes("data: " + CHUNK + "\n\ndata: [DONE]\n\n")));
        Flux<String> converted = conversion.responseAdapter().adapt(upstream).getBody()
                .map(bytes -> new String(bytes, StandardCharsets.UTF_8));

        for (int attempt = 0; attempt < 2; attempt++) {
            List<String> events = converted.collectList().block(Duration.ofSeconds(3));
            assertEquals(1, events.stream()
                    .filter(event -> event.startsWith("event: response.completed\n")).count());
            assertEquals(1, events.stream()
                    .filter(event -> event.startsWith("event: response.output_text.delta\n")
                            && event.contains("hello")).count());
        }
    }

    @Test
    void testRejectsMalformedAndOversizedCompleteResponses() throws Exception {
        AiProtocolConversion conversion = new ConnectResponsesToChatConverter().convert(
                request("{\"model\":\"test\",\"input\":\"hello\"}"), false);
        for (String body : List.of("invalid", "[]", "", "null", RESPONSE + " {}",
                "x".repeat(5 * 1024 * 1024 + 1))) {
            StepVerifier.create(conversion.responseAdapter().adapt(response(200, Flux.just(bytes(body)))).getBody())
                    .expectErrorMatches(error -> error instanceof AiProxyException
                            && ((AiProxyException) error).getKind() == AiProxyException.Kind.PROTOCOL)
                    .verify();
        }
        StepVerifier.create(conversion.responseAdapter().adapt(response(200, Flux.empty())).getBody())
                .expectError(AiProxyException.class)
                .verify();
    }

    @Test
    void testConvertsFragmentedMultilineStreamAndProducesOneTerminalEvent() throws Exception {
        String wire = ": heartbeat\r\ndata: {\r\ndata: " + CHUNK.substring(1) + "\r\n\r\n"
                + "data: {\"choices\":[],\"usage\":{\"prompt_tokens\":3,\"completion_tokens\":2}}\r\n\r\n"
                + "data: [DONE]\r\n\r\n";
        List<byte[]> fragments = new ArrayList<>();
        for (byte value : bytes(wire)) {
            fragments.add(new byte[] {value});
        }

        List<String> events = stream(Flux.fromIterable(fragments)).collectList().block(Duration.ofSeconds(3));

        assertTrue(events.stream().anyMatch(event -> event.startsWith("event: response.output_text.delta\n")
                && event.contains("hello")));
        assertEquals(1, events.stream().filter(event -> event.startsWith("event: response.completed\n")).count());
        assertTrue(events.get(events.size() - 1).contains("\"input_tokens\":3"));
    }

    @Test
    void testDoesNotCompleteAfterMalformedEventOrTransportFailure() throws Exception {
        for (Flux<byte[]> body : List.of(
                Flux.just(bytes("data: " + CHUNK + "\n\ndata: invalid\n\n")),
                Flux.just(bytes("data: " + CHUNK + "\n\n"))
                        .concatWith(Flux.error(new IllegalStateException("socket closed"))))) {
            List<String> events = new ArrayList<>();
            StepVerifier.create(stream(body).doOnNext(events::add))
                    .thenConsumeWhile(event -> !event.startsWith("event: response.completed\n"))
                    .expectError(AiProxyException.class)
                    .verify();
            assertTrue(events.stream().noneMatch(event -> event.startsWith("event: response.completed\n")));
        }
    }

    @Test
    void testCancelsUpstreamAtDone() throws Exception {
        AtomicBoolean cancelled = new AtomicBoolean();

        List<String> events = stream(Flux.just(bytes("data: [DONE]\n\n"))
                .concatWith(Flux.never()).doOnCancel(() -> cancelled.set(true)))
                .collectList().block(Duration.ofSeconds(3));

        assertTrue(cancelled.get());
        assertEquals(1, events.stream().filter(event -> event.startsWith("event: response.completed\n")).count());
    }

    @Test
    void testRejectsWrongSourceProtocol() throws Exception {
        ObjectNode payload = object("{\"model\":\"test\",\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}]}");
        ShenyuAiRequest request = new OpenAiChat().decodeRequest(payload.toString(), payload);

        AiProxyException error = assertThrows(AiProxyException.class,
                () -> new ConnectResponsesToChatConverter().convert(request, false));

        assertEquals(AiProxyException.Kind.REQUEST, error.getKind());
    }

    private ShenyuAiRequest request(final String body) throws Exception {
        ObjectNode payload = object(body);
        return new OpenAiResponses().decodeRequest(body, payload);
    }

    private ObjectNode object(final String value) throws Exception {
        return (ObjectNode) MAPPER.readTree(value);
    }

    private AiUpstreamResponse response(final int status, final Flux<byte[]> body) {
        return new AiUpstreamResponse(status, Map.of(), body);
    }

    private Flux<String> stream(final Flux<byte[]> body) throws Exception {
        AiProtocolConversion conversion = new ConnectResponsesToChatConverter().convert(
                request("{\"model\":\"test\",\"input\":\"hello\",\"stream\":true}"), true);
        return conversion.responseAdapter().adapt(response(200, body)).getBody()
                .map(bytes -> new String(bytes, StandardCharsets.UTF_8));
    }

    private byte[] bytes(final String value) {
        return value.getBytes(StandardCharsets.UTF_8);
    }
}
