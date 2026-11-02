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

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.rule.PiiDetectionHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.ai.pii.audit.PiiAuditRecord;
import org.apache.shenyu.plugin.ai.pii.handler.PiiDetectionPluginDataHandler;
import org.apache.shenyu.plugin.ai.pii.handler.PiiDetectionRule;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.core.io.buffer.DataBufferFactory;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.codec.ServerCodecConfigurer;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.zip.GZIPOutputStream;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link PiiDetectionPlugin} on the request path.
 */
public final class PiiDetectionRequestFlowTest {

    private static final String CHAT_BODY = "{\"model\":\"gpt-4\",\"messages\":"
            + "[{\"role\":\"user\",\"content\":\"my number is 13800138000\"}]}";

    private static final String STREAMING_CHAT_BODY = "{\"model\":\"gpt-4\",\"stream\":true,\"messages\":"
            + "[{\"role\":\"user\",\"content\":\"my number is 13800138000\"}]}";

    private static final String PLAIN_CHAT_BODY = "{\"model\":\"gpt-4\",\"messages\":"
            + "[{\"role\":\"user\",\"content\":\"hello\"}]}";

    private final List<PiiAuditRecord> audits = new CopyOnWriteArrayList<>();

    private RuleData ruleData;

    @BeforeEach
    public void setUp() {
        ConfigurableApplicationContext applicationContext = mock(ConfigurableApplicationContext.class);
        when(applicationContext.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils.getInstance().setApplicationContext(applicationContext);
        ruleData = new RuleData();
        ruleData.setId("rule-1");
        ruleData.setSelectorId("selector-1");
        ruleData.setPluginName(PluginEnum.PII_DETECTION.getName());
        cacheHandle(new PiiDetectionHandle());
    }

    @AfterEach
    public void tearDown() {
        SpringBeanUtils.getInstance().setApplicationContext(null);
        PiiDetectionPluginDataHandler.CACHED_RULES.get().removeHandle(CacheKeyUtils.INST.getKey(ruleData));
    }

    @Test
    public void testRedactTheRequestBeforeTheChain() {
        StepProbe probe = new StepProbe();
        ShenyuPluginChain chain = chain(probe);
        PiiDetectionPlugin plugin = new PiiDetectionPlugin(ServerCodecConfigurer.create().getReaders(), audits::add);
        plugin.doExecute(jsonRequest(CHAT_BODY), chain, null, ruleData).block();
        assertTrue(probe.body.contains("<PHONE_CN>"), "the phone must be redacted: " + probe.body);
        assertFalse(probe.body.contains("13800138000"), "the phone must not survive");
        assertTrue(probe.body.contains("gpt-4"), "the nontext fields survive");
    }

    @Test
    public void testBlockActionRejectsTheRequest() {
        PiiDetectionHandle handle = new PiiDetectionHandle();
        handle.setActions("NATIONAL_ID_CN=BLOCK");
        cacheHandle(handle);
        ShenyuPluginChain chain = chain();
        PiiDetectionPlugin plugin = new PiiDetectionPlugin(ServerCodecConfigurer.create().getReaders(), audits::add);
        MockServerWebExchange exchange = jsonRequest("{\"messages\":[{\"role\":\"user\","
                + "\"content\":\"id 11010519491231002X here\"}]}");
        plugin.doExecute(exchange, chain, null, ruleData).block();
        verify(chain, never()).execute(any(ServerWebExchange.class));
        String body = exchange.getResponse().getBodyAsString().block();
        assertTrue(body.contains("sensitive content detected"));
        assertFalse(body.contains("11010519491231002X"));
    }

    @Test
    public void testAuditActionRecordsWithoutTheValue() {
        PiiDetectionHandle handle = new PiiDetectionHandle();
        handle.setDefaultAction("AUDIT");
        handle.setDetectors("PHONE_CN");
        cacheHandle(handle);
        ShenyuPluginChain chain = chain();
        PiiDetectionPlugin plugin = new PiiDetectionPlugin(ServerCodecConfigurer.create().getReaders(), audits::add);
        plugin.doExecute(jsonRequest(CHAT_BODY), chain, null, ruleData).block();
        assertEquals(1, audits.size());
        PiiAuditRecord record = audits.get(0);
        assertEquals("PHONE_CN", record.getPiiType());
        assertEquals("request", record.getDirection());
        assertEquals("AUDIT", record.getAction());
        assertFalse(record.toString().contains("13800138000"));
        verify(chain, atLeastOnce()).execute(any(ServerWebExchange.class));
    }

    @Test
    public void testNonJsonBodyPassesThroughUnscanned() {
        ShenyuPluginChain chain = chain();
        PiiDetectionPlugin plugin = new PiiDetectionPlugin(ServerCodecConfigurer.create().getReaders(), audits::add);
        plugin.doExecute(jsonRequest("plain text, not json"), chain, null, ruleData).block();
        verify(chain, atLeastOnce()).execute(any(ServerWebExchange.class));
        assertTrue(audits.isEmpty());
    }

    @Test
    public void testNonJsonBodyIsRejectedWhenFailClosed() {
        PiiDetectionHandle handle = new PiiDetectionHandle();
        handle.setFailClosed(true);
        cacheHandle(handle);
        ShenyuPluginChain chain = chain();
        PiiDetectionPlugin plugin = new PiiDetectionPlugin(ServerCodecConfigurer.create().getReaders(), audits::add);
        MockServerWebExchange exchange = jsonRequest("plain text, not json");
        plugin.doExecute(exchange, chain, null, ruleData).block();
        verify(chain, never()).execute(any(ServerWebExchange.class));
        assertTrue(exchange.getResponse().getBodyAsString().block().contains("sensitive content detected"));
    }

    @Test
    public void testStreamingRequestIsRejectedUpFrontWhenFailClosed() {
        PiiDetectionHandle handle = new PiiDetectionHandle();
        handle.setScope("request,response");
        handle.setFailClosed(true);
        cacheHandle(handle);
        ShenyuPluginChain chain = chain();
        PiiDetectionPlugin plugin = new PiiDetectionPlugin(ServerCodecConfigurer.create().getReaders(), audits::add);
        MockServerWebExchange exchange = jsonRequest(STREAMING_CHAT_BODY);
        plugin.doExecute(exchange, chain, null, ruleData).block();
        verify(chain, never()).execute(any(ServerWebExchange.class));
        assertTrue(exchange.getResponse().getBodyAsString().block().contains("sensitive content detected"));
    }

    @Test
    public void testOversizedBodyIsRejectedWhenFailClosed() {
        PiiDetectionHandle handle = new PiiDetectionHandle();
        handle.setMaxBodySize(10L);
        handle.setFailClosed(true);
        cacheHandle(handle);
        ShenyuPluginChain chain = chain();
        PiiDetectionPlugin plugin = new PiiDetectionPlugin(ServerCodecConfigurer.create().getReaders(), audits::add);
        plugin.doExecute(jsonRequest(CHAT_BODY), chain, null, ruleData).block();
        verify(chain, never()).execute(any(ServerWebExchange.class));
    }

    @Test
    public void testMalformedProtocolIsRejectedWhenFailClosed() {
        PiiDetectionHandle handle = new PiiDetectionHandle();
        handle.setFailClosed(true);
        cacheHandle(handle);
        ShenyuPluginChain chain = chain();
        MockServerWebExchange exchange = jsonRequest("{\"messages\":\"13800138000\"}");
        new PiiDetectionPlugin(ServerCodecConfigurer.create().getReaders(), audits::add)
                .doExecute(exchange, chain, null, ruleData).block();
        verify(chain, never()).execute(any(ServerWebExchange.class));
        assertFalse(exchange.getResponse().getBodyAsString().block().contains("13800138000"));
    }

    @Test
    public void testMalformedProtocolPassesThroughWhenFailOpen() {
        StepProbe probe = new StepProbe();
        String body = "{\"messages\":\"13800138000\"}";
        new PiiDetectionPlugin(ServerCodecConfigurer.create().getReaders(), audits::add)
                .doExecute(jsonRequest(body), chain(probe), null, ruleData).block();
        assertEquals(body, probe.body);
        assertTrue(audits.isEmpty());
    }

    @Test
    public void testConfiguredRuleWithoutCompiledPolicyIsRejected() {
        PiiDetectionPluginDataHandler.CACHED_RULES.get().removeHandle(CacheKeyUtils.INST.getKey(ruleData));
        ruleData.setHandle("{\"customPatterns\":\"configured but not compiled\"}");
        ShenyuPluginChain chain = chain();
        new PiiDetectionPlugin(ServerCodecConfigurer.create().getReaders(), audits::add)
                .doExecute(jsonRequest(CHAT_BODY), chain, null, ruleData).block();
        verify(chain, never()).execute(any(ServerWebExchange.class));
    }

    @Test
    public void testClearedRuleWithoutCompiledPolicyPassesThrough() {
        PiiDetectionPluginDataHandler.CACHED_RULES.get().removeHandle(CacheKeyUtils.INST.getKey(ruleData));
        ruleData.setHandle("null");
        ShenyuPluginChain chain = chain();
        new PiiDetectionPlugin(ServerCodecConfigurer.create().getReaders(), audits::add)
                .doExecute(jsonRequest(CHAT_BODY), chain, null, ruleData).block();
        verify(chain, atLeastOnce()).execute(any(ServerWebExchange.class));
    }

    @Test
    public void testCompressedRequestIsForwardedByteForByteWhenFailOpen() throws Exception {
        byte[] compressed = gzip(PLAIN_CHAT_BODY);
        ByteProbe probe = new ByteProbe();
        PiiDetectionPlugin plugin = new PiiDetectionPlugin(ServerCodecConfigurer.create().getReaders(), audits::add);

        plugin.doExecute(gzipRequest(compressed), chain(probe), null, ruleData).block();

        assertArrayEquals(compressed, probe.bytes,
                "a compressed request must reach the chain as the identical wire bytes");
        assertEquals("gzip", probe.contentEncoding, "the encoding header still describes the forwarded bytes");
        assertTrue(audits.isEmpty(), "the compressed body is never scanned");
    }

    @Test
    public void testCompressedRequestIsRejectedWhenFailClosed() throws Exception {
        PiiDetectionHandle handle = new PiiDetectionHandle();
        handle.setFailClosed(true);
        cacheHandle(handle);
        ShenyuPluginChain chain = chain();
        PiiDetectionPlugin plugin = new PiiDetectionPlugin(ServerCodecConfigurer.create().getReaders(), audits::add);
        MockServerWebExchange exchange = gzipRequest(gzip(PLAIN_CHAT_BODY));

        plugin.doExecute(exchange, chain, null, ruleData).block();

        verify(chain, never()).execute(any(ServerWebExchange.class));
        assertTrue(exchange.getResponse().getBodyAsString().block().contains("sensitive content detected"));
    }

    @Test
    public void testCompressedRequestIsRejectedByTheResponseScopePrecheckWhenFailClosed() throws Exception {
        // scope=response alone reads the request body only for the mode precheck, and the
        // precheck cannot look into a compressed payload either
        PiiDetectionHandle handle = new PiiDetectionHandle();
        handle.setScope("response");
        handle.setFailClosed(true);
        cacheHandle(handle);
        ShenyuPluginChain chain = chain();
        PiiDetectionPlugin plugin = new PiiDetectionPlugin(ServerCodecConfigurer.create().getReaders(), audits::add);
        MockServerWebExchange exchange = gzipRequest(gzip(PLAIN_CHAT_BODY));

        plugin.doExecute(exchange, chain, null, ruleData).block();

        verify(chain, never()).execute(any(ServerWebExchange.class));
    }

    @Test
    public void testCompressedRequestIsForwardedWhenTheRuleOnlyScansTheResponse() throws Exception {
        // the request body is not read on this path, so nothing can corrupt it
        PiiDetectionHandle handle = new PiiDetectionHandle();
        handle.setScope("response");
        cacheHandle(handle);
        ByteProbe probe = new ByteProbe();
        byte[] compressed = gzip(PLAIN_CHAT_BODY);
        PiiDetectionPlugin plugin = new PiiDetectionPlugin(ServerCodecConfigurer.create().getReaders(), audits::add);

        plugin.doExecute(gzipRequest(compressed), chain(probe), null, ruleData).block();

        assertArrayEquals(compressed, probe.bytes);
    }

    @Test
    public void testIdentityEncodingIsStillScanned() {
        StepProbe probe = new StepProbe();
        ShenyuPluginChain chain = chain(probe);
        PiiDetectionPlugin plugin = new PiiDetectionPlugin(ServerCodecConfigurer.create().getReaders(), audits::add);
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/test")
                .contentType(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.CONTENT_ENCODING, "identity")
                .body(CHAT_BODY));

        plugin.doExecute(exchange, chain, null, ruleData).block();

        assertTrue(probe.body.contains("<PHONE_CN>"), "an identity encoding is plain text: " + probe.body);
    }

    private static byte[] gzip(final String body) throws Exception {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        try (GZIPOutputStream gzip = new GZIPOutputStream(out)) {
            gzip.write(body.getBytes(StandardCharsets.UTF_8));
        }
        return out.toByteArray();
    }

    /**
     * A compressed request delivered in several chunks, like a body that does
     * not fit into a single network read.
     */
    private MockServerWebExchange gzipRequest(final byte[] compressed) {
        DataBufferFactory factory = new DefaultDataBufferFactory();
        int half = compressed.length / 2;
        byte[] head = Arrays.copyOfRange(compressed, 0, half);
        byte[] tail = Arrays.copyOfRange(compressed, half, compressed.length);
        return MockServerWebExchange.from(MockServerHttpRequest.post("/test")
                .contentType(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.CONTENT_ENCODING, "gzip")
                .body(Flux.just(factory.wrap(head), factory.wrap(tail))));
    }

    private ShenyuPluginChain chain() {
        ShenyuPluginChain chain = mock(ShenyuPluginChain.class);
        when(chain.execute(any(ServerWebExchange.class))).thenReturn(Mono.empty());
        return chain;
    }

    private ShenyuPluginChain chain(final StepProbe probe) {
        ShenyuPluginChain chain = mock(ShenyuPluginChain.class);
        when(chain.execute(any(ServerWebExchange.class)))
                .thenAnswer(invocation -> probe.capture(invocation.getArgument(0)));
        return chain;
    }

    private ShenyuPluginChain chain(final ByteProbe probe) {
        ShenyuPluginChain chain = mock(ShenyuPluginChain.class);
        when(chain.execute(any(ServerWebExchange.class)))
                .thenAnswer(invocation -> probe.capture(invocation.getArgument(0)));
        return chain;
    }

    private void cacheHandle(final PiiDetectionHandle handle) {
        PiiDetectionRule rule = PiiDetectionRule.compile(CacheKeyUtils.INST.getKey(ruleData), handle);
        PiiDetectionPluginDataHandler.CACHED_RULES.get()
                .cachedHandle(CacheKeyUtils.INST.getKey(ruleData), rule);
    }

    private MockServerWebExchange jsonRequest(final String body) {
        return MockServerWebExchange.from(MockServerHttpRequest.post("/test")
                .contentType(MediaType.APPLICATION_JSON)
                .body(body));
    }

    /**
     * Captures the bytes the chain would forward, for a payload that must not be
     * decoded as text.
     */
    private static final class ByteProbe {

        private byte[] bytes = new byte[0];

        private String contentEncoding;

        private Mono<Void> capture(final ServerWebExchange exchange) {
            this.contentEncoding = exchange.getRequest().getHeaders().getFirst(HttpHeaders.CONTENT_ENCODING);
            this.bytes = exchange.getRequest().getBody()
                    .map(buffer -> {
                        byte[] chunk = new byte[buffer.readableByteCount()];
                        buffer.read(chunk);
                        DataBufferUtils.release(buffer);
                        return chunk;
                    })
                    .collect(ByteArrayOutputStream::new, (out, chunk) -> out.write(chunk, 0, chunk.length))
                    .map(ByteArrayOutputStream::toByteArray)
                    .block();
            return Mono.empty();
        }
    }

    private static final class StepProbe {

        private String body = "";

        private Mono<Void> capture(final ServerWebExchange exchange) {
            this.body = exchange.getRequest().getBody()
                    .map(buffer -> {
                        byte[] bytes = new byte[buffer.readableByteCount()];
                        buffer.read(bytes);
                        DataBufferUtils.release(buffer);
                        return new String(bytes, StandardCharsets.UTF_8);
                    })
                    .collect(StringBuilder::new, StringBuilder::append)
                    .map(StringBuilder::toString)
                    .block();
            return Mono.empty();
        }
    }
}
