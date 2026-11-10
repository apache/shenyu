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

package org.apache.shenyu.plugin.ai.protocol;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.common.constant.Constants;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpHeaders;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Predicate;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests of the three evidence levels of {@link AiProtocolDetector} and of the per
 * request caching of {@link AiProtocolDetector#resolve}. Ambiguous evidence is not
 * constructible from the two built-in descriptors, so those cases drive the package
 * private detect with stub descriptors.
 */
class AiProtocolDetectorTest {

    private static final String CHAT_BODY = "{\"model\":\"gpt-4\",\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}]}";

    private static final String RESPONSES_BODY = "{\"model\":\"gpt-4\",\"input\":\"hi\"}";

    @Test
    void detectsTheProtocolByExactPath() {
        assertEquals(Optional.of(AiProtocols.OPENAI_CHAT), AiProtocolDetector.detect("/v1/chat/completions", null, (String) null));
        assertEquals(Optional.of(AiProtocols.OPENAI_RESPONSES), AiProtocolDetector.detect("/v1/responses", null, (String) null));
    }

    @Test
    void detectsTheProtocolByPathSuffix() {
        assertEquals(Optional.of(AiProtocols.OPENAI_RESPONSES), AiProtocolDetector.detect("/gateway/foo/v1/responses", null, (String) null));
        assertEquals(Optional.of(AiProtocols.OPENAI_CHAT), AiProtocolDetector.detect("/api/chat/completions", null, (String) null));
    }

    @Test
    void trailingSlashesDoNotChangeThePathVerdict() {
        assertEquals(Optional.of(AiProtocols.OPENAI_RESPONSES), AiProtocolDetector.detect("/v1/responses/", null, (String) null));
        assertEquals(Optional.of(AiProtocols.OPENAI_CHAT), AiProtocolDetector.detect("/v1/chat/completions//", null, (String) null));
        assertEquals(Optional.empty(), AiProtocolDetector.detect("/v1/responsive/", null, (String) null));
    }

    @Test
    void detectsTheProtocolByHeaderHint() {
        StubDescriptor hinted = new StubDescriptor("stub-hint", List.of(), Map.of("x-stub-protocol", "stub"), payload -> false);
        HttpHeaders headers = new HttpHeaders();
        headers.put("x-stub-protocol", List.of("stub"));
        assertEquals(Optional.of("stub-hint"),
                AiProtocolDetector.detect(List.of(new OpenAiChatProtocol(), hinted), "/infer", headers, "{}"));
    }

    @Test
    void detectsTheProtocolByBodyShape() {
        assertEquals(Optional.of(AiProtocols.OPENAI_CHAT), AiProtocolDetector.detect("/infer", null, CHAT_BODY));
        assertEquals(Optional.of(AiProtocols.OPENAI_RESPONSES), AiProtocolDetector.detect("/infer", null, RESPONSES_BODY));
    }

    @Test
    void detectsTheBodyShapeFromAnAlreadyParsedPayload() throws Exception {
        ObjectMapper mapper = new ObjectMapper();
        assertEquals(Optional.of(AiProtocols.OPENAI_CHAT),
                AiProtocolDetector.detect("/infer", null, mapper.readTree(CHAT_BODY)));
        assertEquals(Optional.of(AiProtocols.OPENAI_RESPONSES),
                AiProtocolDetector.detect("/infer", null, mapper.readTree(RESPONSES_BODY)));
        assertEquals(Optional.of(AiProtocols.OPENAI_RESPONSES), AiProtocolDetector.detect("/v1/responses", null, (JsonNode) null));
    }

    @Test
    void multipleBodyCandidatesResolveToUnknown() {
        List<AiProtocolDescriptor> descriptors = List.of(
                new StubDescriptor("stub-a", List.of(), Map.of(), payload -> true),
                new StubDescriptor("stub-b", List.of(), Map.of(), payload -> true));
        assertEquals(Optional.empty(), AiProtocolDetector.detect(descriptors, "/infer", null, "{\"x\":1}"));
    }

    @Test
    void multiplePathCandidatesWithoutBodyEvidenceResolveToUnknown() {
        List<AiProtocolDescriptor> descriptors = List.of(
                new StubDescriptor("stub-a", List.of("/v1/ambiguous"), Map.of(), payload -> false),
                new StubDescriptor("stub-b", List.of("/v1/ambiguous"), Map.of(), payload -> false));
        assertEquals(Optional.empty(), AiProtocolDetector.detect(descriptors, "/gateway/v1/ambiguous", null, "{\"x\":1}"));
    }

    @Test
    void unknownEvidenceResolvesToEmpty() {
        HttpHeaders headers = new HttpHeaders();
        assertEquals(Optional.empty(), AiProtocolDetector.detect("/health", headers, "{\"foo\":\"bar\"}"));
    }

    @Test
    void malformedBodyResolvesToEmpty() {
        assertEquals(Optional.empty(), AiProtocolDetector.detect("/infer", null, "{not json"));
    }

    @Test
    void nullEvidenceSkipsTheMatchingLevels() {
        assertEquals(Optional.of(AiProtocols.OPENAI_CHAT), AiProtocolDetector.detect(null, null, CHAT_BODY));
        assertEquals(Optional.empty(), AiProtocolDetector.detect(null, null, (String) null));
    }

    @Test
    void resolveCachesTheProtocolPerRequest() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/v1/responses").build());

        assertEquals(Optional.of(AiProtocols.OPENAI_RESPONSES), AiProtocolDetector.resolve(exchange, (String) null));
        assertEquals(AiProtocols.OPENAI_RESPONSES, exchange.getAttributes().get(Constants.AI_PROTOCOL));
    }

    @Test
    void resolveCachesTheProtocolFromAParsedPayload() throws Exception {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/infer").build());

        assertEquals(Optional.of(AiProtocols.OPENAI_CHAT),
                AiProtocolDetector.resolve(exchange, new ObjectMapper().readTree(CHAT_BODY)));
        assertEquals(AiProtocols.OPENAI_CHAT, exchange.getAttributes().get(Constants.AI_PROTOCOL));
    }

    @Test
    void resolveResolvesTheBodyWhenThePathIsUnknown() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/infer").build());

        assertEquals(Optional.of(AiProtocols.OPENAI_CHAT), AiProtocolDetector.resolve(exchange, CHAT_BODY));
    }

    @Test
    void resolveCachesUnknownAndDoesNotReDetect() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/infer").build());

        assertEquals(Optional.empty(), AiProtocolDetector.resolve(exchange, (String) null));
        assertEquals("", exchange.getAttributes().get(Constants.AI_PROTOCOL));

        ServerWebExchange rerouted = exchange.mutate().request(builder -> builder.path("/v1/responses")).build();
        assertEquals(Optional.empty(), AiProtocolDetector.resolve(rerouted, (String) null));
    }

    @Test
    void resolvedReadsTheCachedDecisionWithoutDetecting() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/v1/responses").build());

        assertEquals(Optional.empty(), AiProtocolDetector.resolved(exchange));

        AiProtocolDetector.resolve(exchange, (String) null);
        assertEquals(Optional.of(AiProtocols.OPENAI_RESPONSES), AiProtocolDetector.resolved(exchange));
    }

    @Test
    void onlyBuiltInDescriptorsAreDetectable() {
        assertTrue(AiProtocolDetector.isDetectable(AiProtocols.OPENAI_CHAT));
        assertTrue(AiProtocolDetector.isDetectable(AiProtocols.OPENAI_RESPONSES));
        assertFalse(AiProtocolDetector.isDetectable("custom-chat"));
        assertFalse(AiProtocolDetector.isDetectable(null));
    }

    private static final class StubDescriptor implements AiProtocolDescriptor {

        private final String id;

        private final List<String> paths;

        private final Map<String, String> headerHints;

        private final Predicate<JsonNode> body;

        private StubDescriptor(final String id, final List<String> paths, final Map<String, String> headerHints, final Predicate<JsonNode> body) {
            this.id = id;
            this.paths = paths;
            this.headerHints = headerHints;
            this.body = body;
        }

        @Override
        public String id() {
            return this.id;
        }

        @Override
        public List<String> paths() {
            return this.paths;
        }

        @Override
        public Map<String, String> headerHints() {
            return this.headerHints;
        }

        @Override
        public boolean bodyMatches(final JsonNode payload) {
            return this.body.test(payload);
        }
    }
}
