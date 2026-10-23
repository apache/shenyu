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

import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.ConnectProtocolAdapter;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiResponses;
import org.junit.jupiter.api.Test;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.core.publisher.Flux;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Usage accounting on the exact bytes ShenYu's AI proxy converts to, for both client protocols.
 */
class AiTokenLimiterResponsesUsageTest {

    private static final Duration TIMEOUT = Duration.ofSeconds(5);

    private static final String CHAT_COMPLETION = "{\"id\":\"chatcmpl-1\",\"object\":\"chat.completion\",\"model\":\"model\","
            + "\"choices\":[{\"index\":0,\"message\":{\"role\":\"assistant\",\"content\":\"hello\"},\"finish_reason\":\"stop\"}],"
            + "\"usage\":{\"prompt_tokens\":3,\"completion_tokens\":12,\"total_tokens\":15}}";

    private static final String CHAT_STREAM = "data: {\"id\":\"chatcmpl-1\",\"object\":\"chat.completion.chunk\",\"model\":\"model\","
            + "\"choices\":[{\"index\":0,\"delta\":{\"role\":\"assistant\",\"content\":\"hello\"},\"finish_reason\":\"stop\"}]}\n\n"
            + "data: {\"id\":\"chatcmpl-1\",\"object\":\"chat.completion.chunk\",\"model\":\"model\",\"choices\":[],"
            + "\"usage\":{\"prompt_tokens\":3,\"completion_tokens\":12,\"total_tokens\":15}}\n\n"
            + "data: [DONE]\n\n";

    @Test
    void recordsTheSameUsageForChatAndConvertedResponsesBodies() {
        // Issue #7911: the Responses body is produced by the real adapter, not a hand-written fixture.
        byte[] responses = converted(false);
        assertEquals(12L, recordedUsage(false, CHAT_COMPLETION.getBytes(StandardCharsets.UTF_8)));
        assertEquals(12L, recordedUsage(false, responses));
    }

    @Test
    void recordsTheSameUsageForChatAndConvertedResponsesStreams() {
        byte[] responses = converted(true);
        assertEquals(12L, recordedUsage(true, CHAT_STREAM.getBytes(StandardCharsets.UTF_8)));
        assertEquals(12L, recordedUsage(true, responses));
    }

    @Test
    void forwardsTheConvertedBytesUnchanged() {
        for (boolean stream : new boolean[] {false, true}) {
            byte[] responses = converted(stream);
            assertArrayEquals(responses, forwarded(stream, responses));
        }
    }

    private byte[] converted(final boolean stream) {
        ConnectProtocolAdapter adapter = new ConnectProtocolAdapter(OpenAiResponses.NAME, null, null);
        AiUpstreamResponse upstream = new AiUpstreamResponse(200,
                Map.of("Content-Type", List.of(stream ? MediaType.TEXT_EVENT_STREAM_VALUE : MediaType.APPLICATION_JSON_VALUE)),
                Flux.just((stream ? CHAT_STREAM : CHAT_COMPLETION).getBytes(StandardCharsets.UTF_8)));
        return adapter.convertResponse(upstream, stream).getBody()
                .reduce(new ByteArrayOutputStream(), (buffer, bytes) -> {
                    buffer.writeBytes(bytes);
                    return buffer;
                })
                .map(ByteArrayOutputStream::toByteArray)
                .block(TIMEOUT);
    }

    private long recordedUsage(final boolean stream, final byte[] body) {
        List<Long> recorded = new ArrayList<>();
        MockServerWebExchange exchange = exchange(stream);
        AiTokenLimiterPlugin.AiStatisticServerHttpResponse observer = new AiTokenLimiterPlugin.AiStatisticServerHttpResponse(
                exchange, exchange.getResponse(), recorded::add);
        observer.writeWith(Flux.just(buffer(exchange, body))).block(TIMEOUT);
        assertEquals(1, recorded.size());
        return recorded.get(0);
    }

    private byte[] forwarded(final boolean stream, final byte[] body) {
        MockServerWebExchange exchange = exchange(stream);
        AiTokenLimiterPlugin.AiStatisticServerHttpResponse observer = new AiTokenLimiterPlugin.AiStatisticServerHttpResponse(
                exchange, exchange.getResponse(), tokens -> { });
        observer.writeWith(Flux.just(buffer(exchange, body))).block(TIMEOUT);
        return DataBufferUtils.join(exchange.getResponse().getBody()).map(buffer -> {
            byte[] bytes = new byte[buffer.readableByteCount()];
            buffer.read(bytes);
            DataBufferUtils.release(buffer);
            return bytes;
        }).block(TIMEOUT);
    }

    private MockServerWebExchange exchange(final boolean stream) {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/v1/responses"));
        exchange.getResponse().getHeaders().setContentType(stream ? MediaType.TEXT_EVENT_STREAM : MediaType.APPLICATION_JSON);
        return exchange;
    }

    private DataBuffer buffer(final MockServerWebExchange exchange, final byte[] body) {
        return exchange.getResponse().bufferFactory().wrap(body);
    }
}
