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

package org.apache.shenyu.plugin.ai.pii;

import org.apache.shenyu.common.dto.convert.rule.PiiDetectionHandle;
import org.apache.shenyu.plugin.ai.pii.handler.PiiDetectionRule;
import org.junit.jupiter.api.Test;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.core.publisher.Flux;

import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link PiiResponseDecorator}.
 */
public final class PiiResponseDecoratorTest {

    private static final String NATIONAL_ID_BLOCK_ACTIONS = "NATIONAL_ID_CN=BLOCK";

    private final java.util.List<org.apache.shenyu.plugin.ai.pii.audit.PiiAuditRecord> audits =
            new java.util.concurrent.CopyOnWriteArrayList<>();

    @Test
    public void testRedactsJsonAndFixesContentLength() {
        PiiDetectionRule rule = PiiDetectionRule.compile("rule-1", new PiiDetectionHandle());
        String upstream = "{\"choices\":[{\"message\":{\"content\":\"dial 13800138000\"}}]}";
        MockServerWebExchange exchange = exchange(MediaType.APPLICATION_JSON);
        write(rule, exchange, upstream);

        String body = exchange.getResponse().getBodyAsString().block();
        assertTrue(body.contains("<PHONE_CN>"));
        assertFalse(body.contains("13800138000"));
        assertEquals(String.valueOf(body.getBytes(StandardCharsets.UTF_8).length),
                exchange.getResponse().getHeaders().getFirst(HttpHeaders.CONTENT_LENGTH));
    }

    @Test
    public void testBlocksStrictFindings() {
        PiiDetectionHandle handle = new PiiDetectionHandle();
        handle.setActions(NATIONAL_ID_BLOCK_ACTIONS);
        PiiDetectionRule rule = PiiDetectionRule.compile("rule-1", handle);
        MockServerWebExchange exchange = exchange(MediaType.APPLICATION_JSON);
        write(rule, exchange, "{\"choices\":[{\"message\":{\"content\":\"id 11010519491231002X\"}}]}");

        assertEquals(HttpStatus.FORBIDDEN, exchange.getResponse().getStatusCode());
        String body = exchange.getResponse().getBodyAsString().block();
        assertTrue(body.contains("sensitive content detected"));
        assertFalse(body.contains("11010519491231002X"));
    }

    @Test
    public void testPassesServerSentEventsThrough() {
        PiiDetectionRule rule = PiiDetectionRule.compile("rule-1", new PiiDetectionHandle());
        MockServerWebExchange exchange = exchange(MediaType.TEXT_EVENT_STREAM);
        PiiResponseDecorator decorator = decorator(rule, exchange);
        String chunk = "data: {\"content\":\"13800138000\"}\n\n";

        decorator.writeAndFlushWith(Flux.just(Flux.just(buffer(chunk)))).block();

        assertEquals(chunk, exchange.getResponse().getBodyAsString().block());
    }

    @Test
    public void testPassesCompressedBodyThrough() {
        PiiDetectionRule rule = PiiDetectionRule.compile("rule-1", new PiiDetectionHandle());
        MockServerWebExchange exchange = exchange(MediaType.APPLICATION_JSON);
        exchange.getResponse().getHeaders().set(HttpHeaders.CONTENT_ENCODING, "gzip");
        write(rule, exchange, "compressed bytes");

        assertEquals("compressed bytes", exchange.getResponse().getBodyAsString().block());
    }

    @Test
    public void testHonoursTheSkipAttribute() {
        PiiDetectionRule rule = PiiDetectionRule.compile("rule-1", new PiiDetectionHandle());
        MockServerWebExchange exchange = exchange(MediaType.APPLICATION_JSON);
        exchange.getAttributes().put(PiiDetectionPlugin.SKIP_RESPONSE_ATTRIBUTE, Boolean.TRUE);
        write(rule, exchange, "{\"choices\":[{\"message\":{\"content\":\"13800138000\"}}]}");

        assertTrue(exchange.getResponse().getBodyAsString().block().contains("13800138000"));
    }

    @Test
    public void testErrorNullResponsesCompletionIsStillScanned() {
        // a successful openai responses completion carries "error": null, it must
        // not take the error-envelope bypass
        String upstream = "{\"object\":\"response\",\"status\":\"completed\",\"error\":null,"
                + "\"output\":[{\"type\":\"message\",\"role\":\"assistant\",\"content\":"
                + "[{\"type\":\"output_text\",\"text\":\"reach me at alice@example.com\"}]}]}";
        PiiDetectionRule rule = PiiDetectionRule.compile("rule-1", new PiiDetectionHandle());
        MockServerWebExchange exchange = exchange(MediaType.APPLICATION_JSON);
        write(rule, exchange, upstream);

        String body = exchange.getResponse().getBodyAsString().block();
        assertTrue(body.contains("<EMAIL>"));
        assertFalse(body.contains("alice@example.com"));
    }

    @Test
    public void testRealErrorEnvelopeIsNotRewritten() {
        PiiDetectionRule rule = PiiDetectionRule.compile("rule-1", new PiiDetectionHandle());
        MockServerWebExchange exchange = exchange(MediaType.APPLICATION_JSON);
        write(rule, exchange, "{\"error\":{\"message\":\"provider quota exceeded\",\"code\":429}}");

        assertTrue(exchange.getResponse().getBodyAsString().block().contains("provider quota exceeded"));
    }

    @Test
    public void testRealGzipBytesSurviveThePassThrough() throws Exception {
        PiiDetectionRule rule = PiiDetectionRule.compile("rule-1", new PiiDetectionHandle());
        MockServerWebExchange exchange = exchange(MediaType.APPLICATION_JSON);
        exchange.getResponse().getHeaders().set(HttpHeaders.CONTENT_ENCODING, "gzip");
        String text = "{\"choices\":[{\"message\":{\"content\":\"dial 13800138000\"}}]}";
        java.io.ByteArrayOutputStream gzipped = new java.io.ByteArrayOutputStream();
        try (java.util.zip.GZIPOutputStream gzip = new java.util.zip.GZIPOutputStream(gzipped)) {
            gzip.write(text.getBytes(StandardCharsets.UTF_8));
        }
        byte[] wire = gzipped.toByteArray();

        decorator(rule, exchange)
                .writeWith(Flux.just(DefaultDataBufferFactory.sharedInstance.wrap(wire))).block();

        byte[] written = exchange.getResponse().getBody()
                .map(dataBuffer -> {
                    byte[] bytes = new byte[dataBuffer.readableByteCount()];
                    dataBuffer.read(bytes);
                    org.springframework.core.io.buffer.DataBufferUtils.release(dataBuffer);
                    return bytes;
                })
                .collect(java.io.ByteArrayOutputStream::new, (out, bytes) -> out.write(bytes, 0, bytes.length))
                .map(java.io.ByteArrayOutputStream::toByteArray)
                .block();
        // byte level identity: a compressed body is never decoded and re encoded
        org.junit.jupiter.api.Assertions.assertArrayEquals(wire, written);
        java.io.ByteArrayOutputStream inflated = new java.io.ByteArrayOutputStream();
        try (java.util.zip.GZIPInputStream in = new java.util.zip.GZIPInputStream(
                new java.io.ByteArrayInputStream(written))) {
            byte[] chunk = new byte[1024];
            int read;
            while ((read = in.read(chunk)) > 0) {
                inflated.write(chunk, 0, read);
            }
        }
        assertEquals(text, new String(inflated.toByteArray(), StandardCharsets.UTF_8));
    }

    @Test
    public void testChunkedBodyOverTheBoundIsRejectedWhenFailClosed() {
        PiiDetectionHandle handle = new PiiDetectionHandle();
        handle.setMaxBodySize(10L);
        handle.setFailClosed(true);
        PiiDetectionRule rule = PiiDetectionRule.compile("rule-1", handle);
        MockServerWebExchange exchange = exchange(MediaType.APPLICATION_JSON);
        exchange.getResponse().getHeaders().remove(HttpHeaders.CONTENT_LENGTH);
        PiiResponseDecorator decorator = decorator(rule, exchange);

        decorator.writeWith(Flux.just(buffer("{\"choices\":[{\"message\":"),
                buffer("{\"content\":\"way over ten bytes\"}}]}"))).block();

        assertEquals(HttpStatus.FORBIDDEN, exchange.getResponse().getStatusCode());
    }

    @Test
    public void testChunkedBodyOverTheBoundPassesThroughCompleteWhenFailOpen() {
        PiiDetectionHandle handle = new PiiDetectionHandle();
        handle.setMaxBodySize(10L);
        PiiDetectionRule rule = PiiDetectionRule.compile("rule-1", handle);
        MockServerWebExchange exchange = exchange(MediaType.APPLICATION_JSON);
        exchange.getResponse().getHeaders().remove(HttpHeaders.CONTENT_LENGTH);
        PiiResponseDecorator decorator = decorator(rule, exchange);
        String complete = "{\"choices\":[{\"message\":"
                + "{\"content\":\"dial 13800138000\"}}]}";

        decorator.writeWith(Flux.just(buffer("{\"choices\":[{\"message\":"),
                buffer("{\"content\":\"dial 13800138000\"}}]}"))).block();

        // fail open switches to unscanned streaming at the bound: the prefix is
        // written back, the overflowing chunk and every later chunk follow as they
        // arrive, nothing is lost or duplicated
        assertEquals(complete, exchange.getResponse().getBodyAsString().block());
    }

    @Test
    public void testFailOpenStreamsThePrefixBeforeTheUpstreamCompletes() {
        PiiDetectionHandle handle = new PiiDetectionHandle();
        handle.setMaxBodySize(10L);
        PiiDetectionRule rule = PiiDetectionRule.compile("rule-1", handle);
        MockServerWebExchange exchange = exchange(MediaType.APPLICATION_JSON);
        exchange.getResponse().getHeaders().remove(HttpHeaders.CONTENT_LENGTH);
        PiiResponseDecorator decorator = decorator(rule, exchange);
        reactor.core.publisher.Sinks.Many<org.springframework.core.io.buffer.DataBuffer> upstream =
                reactor.core.publisher.Sinks.many().unicast().onBackpressureBuffer();

        decorator.writeWith(upstream.asFlux()).subscribe();
        // the first chunk stays under the bound and is buffered, the second one
        // crosses it: the buffered prefix must be replayed in full
        upstream.tryEmitNext(buffer("prefix"));
        upstream.tryEmitNext(buffer("-overflow"));

        // the upstream is still open: the buffered prefix and the overflowing chunk
        // must already be readable downstream without waiting for the completion
        String received = exchange.getResponse().getBody()
                .take(2)
                .map(dataBuffer -> {
                    byte[] bytes = new byte[dataBuffer.readableByteCount()];
                    dataBuffer.read(bytes);
                    org.springframework.core.io.buffer.DataBufferUtils.release(dataBuffer);
                    return new String(bytes, StandardCharsets.UTF_8);
                })
                .collect(StringBuilder::new, StringBuilder::append)
                .map(StringBuilder::toString)
                .block(java.time.Duration.ofSeconds(10));

        assertEquals("prefix-overflow", received);
        upstream.tryEmitComplete();
    }

    @Test
    public void testFailOpenSwitchReleasesPooledBuffersOfBothPrefixAndOverflowChunks() {
        // a real netty pooled buffer proves the ownership: every upstream buffer is
        // released by the plugin as soon as its content was copied, the buffers the
        // switch point emits are array backed and carry no pooled memory, so a
        // cancellation between them cannot leak direct memory
        io.netty.buffer.ByteBufAllocator allocator = io.netty.buffer.ByteBufAllocator.DEFAULT;
        final org.springframework.core.io.buffer.NettyDataBufferFactory nettyFactory =
                new org.springframework.core.io.buffer.NettyDataBufferFactory(allocator);
        io.netty.buffer.ByteBuf first = allocator.buffer();
        first.writeBytes("prefix".getBytes(StandardCharsets.UTF_8));
        io.netty.buffer.ByteBuf second = allocator.buffer();
        second.writeBytes("-overflow".getBytes(StandardCharsets.UTF_8));

        PiiDetectionHandle handle = new PiiDetectionHandle();
        handle.setMaxBodySize(10L);
        PiiDetectionRule rule = PiiDetectionRule.compile("rule-1", handle);
        MockServerWebExchange exchange = exchange(MediaType.APPLICATION_JSON);
        exchange.getResponse().getHeaders().remove(HttpHeaders.CONTENT_LENGTH);
        PiiResponseDecorator decorator = decorator(rule, exchange);
        reactor.core.publisher.Sinks.Many<org.springframework.core.io.buffer.DataBuffer> upstream =
                reactor.core.publisher.Sinks.many().unicast().onBackpressureBuffer();

        decorator.writeWith(upstream.asFlux()).subscribe();
        upstream.tryEmitNext(nettyFactory.wrap(first));
        upstream.tryEmitNext(nettyFactory.wrap(second));

        // read only the replayed prefix, then cancel: the overflow buffer was never
        // delivered downstream, the plugin must still have released it
        String replayed = exchange.getResponse().getBody()
                .take(1)
                .map(dataBuffer -> {
                    byte[] bytes = new byte[dataBuffer.readableByteCount()];
                    dataBuffer.read(bytes);
                    org.springframework.core.io.buffer.DataBufferUtils.release(dataBuffer);
                    return new String(bytes, StandardCharsets.UTF_8);
                })
                .next()
                .block(java.time.Duration.ofSeconds(10));
        assertEquals("prefix", replayed);

        org.junit.jupiter.api.Assertions.assertEquals(0, first.refCnt(),
                "the prefix chunk must be released once its content was copied");
        org.junit.jupiter.api.Assertions.assertEquals(0, second.refCnt(),
                "the overflow chunk must be released even though the downstream cancelled");
    }

    @Test
    public void testMalformedResponseIsRejectedWhenFailClosed() {
        PiiDetectionHandle handle = new PiiDetectionHandle();
        handle.setFailClosed(true);
        MockServerWebExchange exchange = exchange(MediaType.APPLICATION_JSON);
        write(PiiDetectionRule.compile("rule-1", handle), exchange,
                "{\"choices\":{},\"unscannable\":\"13800138000\"}");
        assertEquals(HttpStatus.FORBIDDEN, exchange.getResponse().getStatusCode());
        assertFalse(exchange.getResponse().getBodyAsString().block().contains("13800138000"));
    }

    @Test
    public void testMalformedResponsePassesThroughWhenFailOpen() {
        MockServerWebExchange exchange = exchange(MediaType.APPLICATION_JSON);
        String body = "{\"choices\":{},\"unscannable\":\"13800138000\"}";
        write(PiiDetectionRule.compile("rule-1", new PiiDetectionHandle()), exchange, body);
        assertEquals(body, exchange.getResponse().getBodyAsString().block());
    }

    @Test
    public void testStrictToolCallOnlyResponseRemainsUnchanged() {
        PiiDetectionHandle handle = new PiiDetectionHandle();
        handle.setFailClosed(true);
        MockServerWebExchange exchange = exchange(MediaType.APPLICATION_JSON);
        String body = "{\"choices\":[{\"message\":{\"content\":null,\"tool_calls\":[{\"type\":\"function\","
                + "\"function\":{\"name\":\"lookup\",\"arguments\":\"{\\\"phone\\\":\\\"13800138000\\\"}\"}}]}}]}";
        write(PiiDetectionRule.compile("rule-1", handle), exchange, body);
        assertEquals(body, exchange.getResponse().getBodyAsString().block());
    }

    private void write(final PiiDetectionRule rule, final MockServerWebExchange exchange, final String body) {
        decorator(rule, exchange).writeWith(Flux.just(buffer(body))).block();
    }

    private PiiResponseDecorator decorator(final PiiDetectionRule rule, final MockServerWebExchange exchange) {
        return new PiiResponseDecorator(exchange, rule, audits::add);
    }

    private MockServerWebExchange exchange(final MediaType contentType) {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/t").build());
        exchange.getResponse().getHeaders().setContentType(contentType);
        return exchange;
    }

    private DataBuffer buffer(final String body) {
        return DefaultDataBufferFactory.sharedInstance.wrap(body.getBytes(StandardCharsets.UTF_8));
    }
}
