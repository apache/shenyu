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

package org.apache.shenyu.plugin.ai.token.limiter;

import org.junit.jupiter.api.Test;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.core.publisher.Flux;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Output-token accounting for Anthropic Messages and Gemini payloads (#8458).
 * Anthropic splits its usage across stream events — the cumulative
 * {@code message_delta.usage.output_tokens} must be recorded exactly once;
 * Gemini re-carries {@code usageMetadata} on every chunk and the final
 * snapshot wins.
 */
class AiTokenLimiterMultiProtocolUsageTest {

    private static final Duration TIMEOUT = Duration.ofSeconds(5);

    private static final String ANTHROPIC_BODY = "{\"model\":\"claude\",\"content\":[{\"type\":\"text\",\"text\":\"hi\"}],"
            + "\"usage\":{\"input_tokens\":12,\"output_tokens\":34},\"stop_reason\":\"end_turn\"}";

    private static final String ANTHROPIC_STREAM = "event: message_start\ndata: {\"type\":\"message_start\",\"message\":{\"usage\":{\"input_tokens\":12,\"output_tokens\":1}}}\n\n"
            + "event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"delta\":{\"type\":\"text_delta\",\"text\":\"hi\"}}\n\n"
            + "event: message_delta\ndata: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":34}}\n\n"
            + "event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n";

    private static final String GEMINI_BODY = "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"hi\"}],\"role\":\"model\"}}],"
            + "\"usageMetadata\":{\"promptTokenCount\":7,\"candidatesTokenCount\":3,\"totalTokenCount\":10}}";

    private static final String GEMINI_STREAM = "data: {\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"he\"}]}}],"
            + "\"usageMetadata\":{\"promptTokenCount\":7,\"candidatesTokenCount\":2,\"totalTokenCount\":9}}\n\n"
            + "data: {\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"y\"}]}}],"
            + "\"usageMetadata\":{\"promptTokenCount\":7,\"candidatesTokenCount\":3,\"totalTokenCount\":10}}\n\n";

    @Test
    void recordsAnthropicNonStreamingOutputTokens() {
        assertEquals(34L, recordedUsage(false, ANTHROPIC_BODY));
    }

    @Test
    void recordsAnthropicStreamOutputTokensExactlyOnceFromTheCumulativeEvent() {
        assertEquals(34L, recordedUsage(true, ANTHROPIC_STREAM));
    }

    @Test
    void recordsGeminiNonStreamingCandidatesTokenCount() {
        assertEquals(3L, recordedUsage(false, GEMINI_BODY));
    }

    @Test
    void recordsGeminiStreamFinalUsageMetadata() {
        assertEquals(3L, recordedUsage(true, GEMINI_STREAM));
    }

    private long recordedUsage(final boolean stream, final String body) {
        List<Long> recorded = new ArrayList<>();
        MockServerWebExchange exchange = exchange(stream);
        AiTokenLimiterPlugin.AiStatisticServerHttpResponse observer = new AiTokenLimiterPlugin.AiStatisticServerHttpResponse(
                exchange, exchange.getResponse(), recorded::add);
        observer.writeWith(Flux.just(buffer(exchange, body.getBytes(StandardCharsets.UTF_8)))).block(TIMEOUT);
        assertEquals(1, recorded.size());
        return recorded.get(0);
    }

    private MockServerWebExchange exchange(final boolean stream) {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/v1/messages"));
        exchange.getResponse().getHeaders().setContentType(stream ? MediaType.TEXT_EVENT_STREAM : MediaType.APPLICATION_JSON);
        return exchange;
    }

    private DataBuffer buffer(final MockServerWebExchange exchange, final byte[] body) {
        return exchange.getResponse().bufferFactory().wrap(body);
    }
}
