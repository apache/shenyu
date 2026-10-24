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
