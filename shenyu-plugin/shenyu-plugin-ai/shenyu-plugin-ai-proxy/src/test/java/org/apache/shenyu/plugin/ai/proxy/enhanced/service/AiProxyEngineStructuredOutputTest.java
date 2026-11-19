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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.common.dto.convert.rule.AiStructuredOutputConfig;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAi;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAiCompatible;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransport;
import org.junit.jupiter.api.Test;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;

import static org.apache.shenyu.plugin.ai.proxy.enhanced.AiProxyTestUtils.protocolRouteResolver;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Deterministic tests for the opt-in structured output policy: every
 * classification maps to a distinct documented outcome, the repair regeneration is
 * bounded, carries its own attempt identity and stays inside the shared deadline,
 * and the default path forwards bytes unchanged.
 */
class AiProxyEngineStructuredOutputTest {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    private static final String BODY = "{\"messages\":[{\"role\":\"user\",\"content\":\"give me json\"}]}";

    private static final String SCHEMA = "{\"type\":\"object\",\"required\":[\"answer\"]}";

    private static final String VALID_CONTENT = "{\"answer\":42}";

    private static final String MISMATCH_CONTENT = "{\"other\":1}";

    private final List<String> received = new ArrayList<>();

    private final List<AiUpstreamRequest> requests = new ArrayList<>();

    private final List<AiProxyAttemptEvent> events = new ArrayList<>();

    private final AtomicInteger calls = new AtomicInteger();

    private AiStructuredOutputConfig structuredOutput;

    private AiProxyEngine engine;

    @Test
    void validContentForwardsUnchangedBytesWithASingleUpstreamCall() {
        structuredOutput = policy();
        AiProxyEngine engine = engine(transport(of(200, envelope(VALID_CONTENT, "stop"))));
        StepVerifier.create(execute()).verifyComplete();
        assertEquals(1, calls.get());
        assertEquals(envelope(VALID_CONTENT, "stop"), String.join("", received));
        assertTrue(events.stream().anyMatch(event -> event.reason() == AiProxyAttemptEvent.Reason.CONTENT_VALIDATED));
    }

    @Test
    void disabledPolicyForwardsWithoutInspection() {
        structuredOutput = null;
        AiProxyEngine engine = engine(transport(of(200, envelope(MISMATCH_CONTENT, "stop"))));
        StepVerifier.create(execute()).verifyComplete();
        assertEquals(1, calls.get());
        assertEquals(envelope(MISMATCH_CONTENT, "stop"), String.join("", received));
        assertTrue(events.stream().noneMatch(event -> event.reason().name().startsWith("CONTENT")));
    }

    @Test
    void schemaMismatchWithoutRepairIsRejected() {
        structuredOutput = new AiStructuredOutputConfig();
        structuredOutput.setSchema(SCHEMA);
        AiProxyEngine engine = engine(transport(of(200, envelope(MISMATCH_CONTENT, "stop"))));
        StepVerifier.create(execute()).verifyError(AiProxyException.class);
        assertEquals(1, calls.get());
        assertTrue(events.stream().anyMatch(event -> event.reason() == AiProxyAttemptEvent.Reason.CONTENT_REJECTED));
    }

    @Test
    void repairRegeneratesWithItsOwnAttemptIdentityAndForwardsTheRepairedAnswer() {
        structuredOutput = policy();
        AiProxyEngine engine = engine(transport(index -> index == 1
                ? of(200, envelope(MISMATCH_CONTENT, "stop"))
                : of(200, envelope(VALID_CONTENT, "stop"))));
        StepVerifier.create(execute()).verifyComplete();
        assertEquals(2, calls.get());
        assertEquals(envelope(VALID_CONTENT, "stop"), String.join("", received));
        try {
            JsonNode repair = MAPPER.readTree(requests.get(1).getBody());
            JsonNode last = repair.path("messages").get(repair.path("messages").size() - 1);
            assertEquals("user", last.path("role").asText());
            String content = last.path("content").asText();
            assertTrue(content.contains("Fix the output"), "operator repair prompt must be the instruction");
            assertTrue(content.contains(SCHEMA), "the authoritative schema must be embedded");
            assertTrue(content.contains(MISMATCH_CONTENT), "the previous output must be carried as data");
            assertEquals(false, repair.path("stream").asBoolean());
            assertEquals("main-model", repair.path("model").asText());
            // the provider dialect may legally rename the field (max_completion_tokens)
            int repairTokenBound = Math.max(repair.path("max_tokens").asInt(), repair.path("max_completion_tokens").asInt());
            assertEquals(1024, repairTokenBound, "the conservative default token bound must apply when unset");
        } catch (Exception error) {
            throw new IllegalStateException(error);
        }
        assertTrue(events.stream().anyMatch(event -> event.reason() == AiProxyAttemptEvent.Reason.CONTENT_REPAIR));
        assertTrue(events.stream().anyMatch(event -> event.phase() == AiProxyAttemptEvent.Phase.REPAIR
                && event.reason() == AiProxyAttemptEvent.Reason.COMPLETED));
    }

    @Test
    void failedRepairsTerminateWithTheConfiguredBudget() {
        structuredOutput = policy(2);
        AiProxyEngine engine = engine(transport(index -> of(200, envelope(MISMATCH_CONTENT, "stop"))));
        StepVerifier.create(execute()).verifyError(AiProxyException.class);
        assertEquals(3, calls.get(), "primary plus two bounded repair generations");
        assertTrue(events.stream().anyMatch(event -> event.reason() == AiProxyAttemptEvent.Reason.CONTENT_REPAIR_FAILED));
    }

    @Test
    void repairDisabledByDefaultEvenWithAPromptConfigured() {
        structuredOutput = new AiStructuredOutputConfig();
        structuredOutput.setSchema(SCHEMA);
        structuredOutput.setRepairPrompt("Fix the output");
        AiProxyEngine engine = engine(transport(of(200, envelope(MISMATCH_CONTENT, "stop"))));
        StepVerifier.create(execute()).verifyError(AiProxyException.class);
        assertEquals(1, calls.get());
    }

    @Test
    void invalidJsonContentIsRepairable() {
        structuredOutput = policy();
        AiProxyEngine engine = engine(transport(index -> index == 1
                ? of(200, envelope("{not json", "stop"))
                : of(200, envelope(VALID_CONTENT, "stop"))));
        StepVerifier.create(execute()).verifyComplete();
        assertEquals(2, calls.get());
        assertEquals(envelope(VALID_CONTENT, "stop"), String.join("", received));
    }

    @Test
    void emptyContentIsRejectedWithoutRepair() {
        structuredOutput = policy();
        AiProxyEngine engine = engine(transport(of(200, envelope("", "stop"))));
        StepVerifier.create(execute()).verifyError(AiProxyException.class);
        assertEquals(1, calls.get());
    }

    @Test
    void truncatedContentIsRejectedAndNeverRepaired() {
        structuredOutput = policy();
        AiProxyEngine engine = engine(transport(of(200, envelope(VALID_CONTENT, "length"))));
        StepVerifier.create(execute()).verifyError(AiProxyException.class);
        assertEquals(1, calls.get());
    }

    @Test
    void refusalIsRejectedAndNeverRepaired() {
        structuredOutput = policy();
        AiProxyEngine engine = engine(transport(of(200, envelope(VALID_CONTENT, "content_filter"))));
        StepVerifier.create(execute()).verifyError(AiProxyException.class);
        assertEquals(1, calls.get());
    }

    @Test
    void toolCallResponsesAreForwardedUntouchedAndNeverRepaired() {
        structuredOutput = policy();
        String toolBody = "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"tool_calls\":[{\"id\":\"1\"}]},"
                + "\"finish_reason\":\"tool_calls\"}]}";
        AiProxyEngine engine = engine(transport(of(200, toolBody)));
        StepVerifier.create(execute()).verifyComplete();
        assertEquals(1, calls.get());
        assertEquals(toolBody, String.join("", received));
    }

    @Test
    void toolBearingRequestsNeverTriggerRepair() {
        structuredOutput = policy();
        String body = "{\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],\"tools\":[{\"type\":\"function\"}]}";
        AiProxyEngine engine = engine(transport(of(200, envelope(MISMATCH_CONTENT, "stop"))));
        StepVerifier.create(engine.execute(body, config(), Optional.empty(), consumeBody()))
                .verifyError(AiProxyException.class);
        assertEquals(1, calls.get());
    }

    @Test
    void legacyFunctionBearingRequestsNeverTriggerRepair() {
        structuredOutput = policy();
        String legacy = "{\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],"
                + "\"functions\":[{\"name\":\"ping\"}],\"function_call\":\"auto\"}";
        AiProxyEngine engine = engine(transport(of(200, envelope(MISMATCH_CONTENT, "stop"))));
        StepVerifier.create(engine.execute(legacy, config(), Optional.empty(), consumeBody()))
                .verifyError(AiProxyException.class);
        assertEquals(1, calls.get(), "legacy functions/function_call traffic is effectful: repair stays off");
    }

    @Test
    void aRepairedToolCallAnswerIsForwardedNotRejected() {
        structuredOutput = policy();
        String toolCalls = "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"tool_calls\":[{\"id\":\"1\"}]},"
                + "\"finish_reason\":\"tool_calls\"}]}";
        AiProxyEngine engine = engine(transport(index -> index == 1
                ? of(200, envelope(MISMATCH_CONTENT, "stop"))
                : of(200, toolCalls)));
        StepVerifier.create(execute()).verifyComplete();
        assertEquals(2, calls.get());
        assertEquals(toolCalls, String.join("", received), "a repair returning tool calls is the client's decision");
    }

    @Test
    void aValidatedAnswerDropsTheStaleFramingHeaders() {
        structuredOutput = policy();
        List<Map<String, List<String>>> seenHeaders = new ArrayList<>();
        AiProxyEngine engine = engine(transport(index -> Mono.just(new AiUpstreamResponse(200,
                Map.of("Content-Length", List.of("999"), "Content-Encoding", List.of("gzip")),
                Flux.just(bytes(envelope(VALID_CONTENT, "stop")))))));
        StepVerifier.create(engine.execute(BODY, config(), Optional.empty(),
                response -> {
                    seenHeaders.add(response.getHeaders());
                    return response.getBody().then();
                })).verifyComplete();
        Map<String, List<String>> headers = seenHeaders.get(0);
        assertTrue(!headers.containsKey("Content-Length") && !headers.containsKey("Content-Encoding"),
                "the re-framed body must not keep the upstream framing headers");
    }

    @Test
    void anOversizedValidDocumentIsRejectedWithoutRepair() {
        structuredOutput = policy();
        StringBuilder oversized = new StringBuilder("{");
        for (int i = 0; i < 2100; i++) {
            if (i > 0) {
                oversized.append(',');
            }
            oversized.append("\"k").append(i).append("\":\"v\"");
        }
        oversized.append('}');
        AiProxyEngine engine = engine(transport(of(200, envelope(oversized.toString(), "stop"))));
        StepVerifier.create(execute()).verifyError(AiProxyException.class);
        assertEquals(1, calls.get(), "a merely oversized document must not be regenerated");
        assertTrue(events.stream().anyMatch(event -> event.reason() == AiProxyAttemptEvent.Reason.CONTENT_REJECTED));
    }

    @Test
    void repairAfterFallbackReAsksTheFallbackUpstream() {
        structuredOutput = policy();
        AiProxyRetryProperties noRetries = new AiProxyRetryProperties(0, 1, Duration.ofMillis(10));
        noRetries.setJitterFraction(0d);
        AiCommonConfig fallbackConfig = fallbackConfig();
        AiProxyEngine engine = new AiProxyEngine(new AiProxyProtocolFactory(List.of(new OpenAiChat())),
                new AiProxyProviderFactory(List.of(new OpenAi(), new OpenAiCompatible("DeepSeek"))),
                protocolRouteResolver(),
                request -> {
                    int index = calls.incrementAndGet();
                    requests.add(request);
                    if (index == 1) {
                        return Mono.just(new AiUpstreamResponse(500, Map.of(), Flux.just(bytes("{}"))));
                    }
                    if (index == 2) {
                        return of(200, envelope(MISMATCH_CONTENT, "stop"));
                    }
                    return of(200, envelope(VALID_CONTENT, "stop"));
                }, noRetries, (AiProxyAttemptListener) events::add);
        StepVerifier.create(engine.execute(BODY, config(), Optional.of(fallbackConfig), consumeBody()))
                .verifyComplete();
        assertEquals(3, calls.get(), "primary fails, fallback mismatches, the repair re-asks the fallback");
        assertEquals("fallback.example", requests.get(1).getUri().getHost());
        assertEquals("fallback.example", requests.get(2).getUri().getHost(), "the repair re-asks the fallback upstream");
        assertEquals("fallback-model", upstreamModel(requests.get(2)));
    }

    private String upstreamModel(final AiUpstreamRequest request) {
        try {
            return MAPPER.readTree(request.getBody()).path("model").asText();
        } catch (Exception error) {
            throw new IllegalStateException(error);
        }
    }

    private AiCommonConfig fallbackConfig() {
        AiCommonConfig fallback = new AiCommonConfig();
        fallback.setProvider("OpenAI");
        fallback.setBaseUrl("https://fallback.example");
        fallback.setApiKey("fallback-key");
        fallback.setModel("fallback-model");
        fallback.setStream(false);
        return fallback;
    }

    @Test
    void streamingResponsesAreNotInspected() {
        structuredOutput = policy();
        AiCommonConfig streamConfig = config();
        streamConfig.setStream(true);
        AiProxyEngine engine = engine(transport(of(200, "data: {\"oops\":true}\n\n")));
        StepVerifier.create(engine.execute(BODY, streamConfig, Optional.empty(), consumeBody()))
                .verifyComplete();
        assertEquals(1, calls.get());
        assertTrue(String.join("", received).startsWith("data: {\"oops\":true}"),
                "streaming responses must be forwarded through the SSE framing untouched by inspection");
    }

    @Test
    void transportFailureDuringRepairIsNotNetworkRetried() {
        structuredOutput = policy();
        AiProxyEngine engine = engine(transport(index -> index == 1
                ? of(200, envelope(MISMATCH_CONTENT, "stop"))
                : Mono.error(new org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiTransportException(
                        org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiTransportException.Kind.IO_FAILURE,
                        "repair transport failure", null))));
        StepVerifier.create(execute()).verifyError(AiProxyException.class);
        assertEquals(2, calls.get(), "a repair generation has its own budget; the network retry budget must not engage");
        assertTrue(events.stream().noneMatch(event -> event.reason() == AiProxyAttemptEvent.Reason.RETRY_TRANSPORT));
    }

    @Test
    void policyMisconfigurationFailsFast() {
        AiStructuredOutputConfig badSchema = new AiStructuredOutputConfig();
        badSchema.setSchema("{\"unknownAssertion\":true}");
        structuredOutput = badSchema;
        engine(transport(of(200, envelope(VALID_CONTENT, "stop"))));
        StepVerifier.create(execute()).verifyError(AiProxyException.class);

        AiStructuredOutputConfig noPrompt = new AiStructuredOutputConfig();
        noPrompt.setSchema(SCHEMA);
        noPrompt.setRepairEnabled(true);
        structuredOutput = noPrompt;
        engine(transport(of(200, envelope(VALID_CONTENT, "stop"))));
        StepVerifier.create(execute()).verifyError(AiProxyException.class);

        AiStructuredOutputConfig tooManyAttempts = new AiStructuredOutputConfig();
        tooManyAttempts.setSchema(SCHEMA);
        tooManyAttempts.setRepairEnabled(true);
        tooManyAttempts.setRepairPrompt("Fix");
        tooManyAttempts.setRepairMaxAttempts(AiStructuredOutputConfig.MAX_REPAIR_ATTEMPTS_LIMIT + 1);
        structuredOutput = tooManyAttempts;
        engine(transport(of(200, envelope(VALID_CONTENT, "stop"))));
        StepVerifier.create(execute()).verifyError(AiProxyException.class);
    }

    private String envelope(final String content, final String finishReason) {
        return "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":\"" + content.replace("\"", "\\\"")
                + "\"},\"finish_reason\":\"" + finishReason + "\"}]}";
    }

    private AiStructuredOutputConfig policy() {
        return policy(1);
    }

    private AiStructuredOutputConfig policy(final int maxAttempts) {
        AiStructuredOutputConfig config = new AiStructuredOutputConfig();
        config.setSchema(SCHEMA);
        config.setRepairEnabled(true);
        config.setRepairPrompt("Fix the output");
        config.setRepairMaxAttempts(maxAttempts);
        return config;
    }

    private Mono<AiUpstreamResponse> of(final int status, final String body) {
        return Mono.just(new AiUpstreamResponse(status, Map.of(), Flux.just(bytes(body))));
    }

    private byte[] bytes(final String value) {
        return value.getBytes(StandardCharsets.UTF_8);
    }

    private Function<AiUpstreamResponse, Mono<Void>> consumeBody() {
        return response -> response.getBody()
                .doOnNext(chunk -> received.add(new String(chunk, StandardCharsets.UTF_8))).then();
    }

    private AiProxyTransport transport(final Mono<AiUpstreamResponse> single) {
        return transport(index -> single);
    }

    private AiProxyTransport transport(final Function<Integer, Mono<AiUpstreamResponse>> script) {
        return request -> {
            int index = calls.incrementAndGet();
            requests.add(request);
            return script.apply(index);
        };
    }

    private AiCommonConfig config() {
        AiCommonConfig config = new AiCommonConfig();
        config.setProvider("OpenAI");
        config.setBaseUrl("https://main.example");
        config.setApiKey("main-key");
        config.setModel("main-model");
        config.setStream(false);
        config.setStructuredOutput(structuredOutput);
        return config;
    }

    private AiProxyEngine engine(final AiProxyTransport transport) {
        this.engine = new AiProxyEngine(new AiProxyProtocolFactory(List.of(new OpenAiChat())),
                new AiProxyProviderFactory(List.of(new OpenAi(), new OpenAiCompatible("DeepSeek"))),
                protocolRouteResolver(), transport, new AiProxyRetryProperties(3, 1, Duration.ofSeconds(1)),
                (AiProxyAttemptListener) events::add);
        return this.engine;
    }

    private Mono<Void> execute() {
        return engine.execute(BODY, config(), Optional.empty(), consumeBody());
    }
}
