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
import org.apache.shenyu.plugin.ai.pii.handler.PiiDetectionPluginDataHandler;
import org.apache.shenyu.plugin.ai.pii.handler.PiiDetectionRule;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.MediaType;
import org.springframework.http.codec.ServerCodecConfigurer;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.nio.charset.StandardCharsets;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for the enforcement mode precheck of a strict response scope that
 * does not scan the request text: a streaming request is rejected before it
 * reaches the model, a non streaming one is forwarded with its text untouched.
 *
 * <p>This class deliberately avoids a mocking library: the plugin chain is a
 * small hand written recorder, the result factory gets its bean from a real
 * minimal application context.</p>
 */
public final class PiiModePrecheckTest {

    private static final String STREAMING_CHAT_BODY = "{\"model\":\"gpt-4\",\"stream\":true,\"messages\":"
            + "[{\"role\":\"user\",\"content\":\"my number is 13800138000\"}]}";

    private static final String CHAT_BODY = "{\"model\":\"gpt-4\",\"messages\":"
            + "[{\"role\":\"user\",\"content\":\"my number is 13800138000\"}]}";

    private AnnotationConfigApplicationContext applicationContext;

    private RuleData ruleData;

    private MockServerWebExchange latestExchange;

    @BeforeEach
    public void setUp() {
        applicationContext = new AnnotationConfigApplicationContext();
        applicationContext.registerBean(DefaultShenyuResult.class);
        applicationContext.refresh();
        SpringBeanUtils.getInstance().setApplicationContext(applicationContext);
        ruleData = new RuleData();
        ruleData.setId("rule-1");
        ruleData.setSelectorId("selector-1");
        ruleData.setPluginName(PluginEnum.PII_DETECTION.getName());
        cacheHandle(responseOnlyFailClosed());
    }

    @AfterEach
    public void tearDown() {
        PiiDetectionPluginDataHandler.CACHED_RULES.get().removeHandle(CacheKeyUtils.INST.getKey(ruleData));
        SpringBeanUtils.getInstance().setApplicationContext(null);
        applicationContext.close();
    }

    @Test
    public void testStreamingRequestIsRejectedBeforeTheModel() {
        RecordingChain chain = new RecordingChain();
        plugin().doExecute(jsonRequest(STREAMING_CHAT_BODY), chain, null, ruleData).block();

        assertEquals(0, chain.executed.size(), "a streaming request must not reach the chain");
        assertTrue(lastResponseBody().contains("sensitive content detected"));
    }

    @Test
    public void testNonStreamingRequestIsForwardedWithItsTextUntouched() {
        RecordingChain chain = new RecordingChain();
        plugin().doExecute(jsonRequest(CHAT_BODY), chain, null, ruleData).block();

        assertEquals(1, chain.executed.size(), "a non streaming request must reach the chain");
        assertTrue(chain.forwardedBody.contains("13800138000"),
                "a response only scope must not scan or rewrite the request text");
    }

    @Test
    public void testUndeclaredOverLimitChunkedRequestIsRejectedByActualLength() {
        // no Content-Length, the body is above the rule bound (128 bytes) but below
        // the codec limit: the precheck must measure the actual utf-8 length, the
        // bound cannot depend on how the request is transported
        PiiDetectionHandle handle = responseOnlyFailClosed();
        handle.setMaxBodySize(128L);
        cacheHandle(handle);
        StringBuilder payload = new StringBuilder("{\"x\":\"");
        for (int i = 0; i < 1024; i++) {
            payload.append('a');
        }
        payload.append("\"}");
        RecordingChain chain = new RecordingChain();
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/test")
                .contentType(MediaType.APPLICATION_JSON)
                .body(reactor.core.publisher.Flux.just(
                        org.springframework.core.io.buffer.DefaultDataBufferFactory.sharedInstance
                                .wrap(payload.toString().getBytes(StandardCharsets.UTF_8)))));

        plugin().doExecute(exchange, chain, null, ruleData).block();

        assertEquals(0, chain.executed.size());
        assertTrue(exchange.getResponse().getBodyAsString().block().contains("sensitive content detected"));
    }

    private PiiDetectionPlugin plugin() {
        return new PiiDetectionPlugin(ServerCodecConfigurer.create().getReaders(), record -> {
        });
    }

    @Test
    public void testDeclaredOverLimitRequestIsRejectedBeforeThePrecheck() {
        // the declared size bound applies to the precheck path too, a request that
        // announces more than maxBodySize is rejected without reading the body
        PiiDetectionHandle handle = responseOnlyFailClosed();
        handle.setMaxBodySize(10L);
        cacheHandle(handle);
        RecordingChain chain = new RecordingChain();
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/test")
                .contentType(MediaType.APPLICATION_JSON)
                .header(org.springframework.http.HttpHeaders.CONTENT_LENGTH, "100")
                .body(CHAT_BODY));

        plugin().doExecute(exchange, chain, null, ruleData).block();

        assertEquals(0, chain.executed.size());
        assertTrue(exchange.getResponse().getBodyAsString().block().contains("sensitive content detected"));
    }

    @Test
    public void testCodecLimitRequestIsRejectedNotAnInternalError() {
        // a body over the codec in memory limit (256 KiB with the default readers)
        // has been consumed by the aggregation and cannot be forwarded unchanged,
        // the gateway rejects it with a clear reason instead of an internal error
        StringBuilder big = new StringBuilder("{\"x\":\"");
        for (int i = 0; i < 300 * 1024; i++) {
            big.append('a');
        }
        big.append("\"}");
        RecordingChain chain = new RecordingChain();
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/test")
                .contentType(MediaType.APPLICATION_JSON)
                .body(reactor.core.publisher.Flux.just(
                        org.springframework.core.io.buffer.DefaultDataBufferFactory.sharedInstance
                                .wrap(big.toString().getBytes(StandardCharsets.UTF_8)))));

        plugin().doExecute(exchange, chain, null, ruleData).block();

        assertEquals(0, chain.executed.size());
        assertTrue(exchange.getResponse().getBodyAsString().block().contains("sensitive content detected"));
    }

    private PiiDetectionHandle responseOnlyFailClosed() {
        PiiDetectionHandle handle = new PiiDetectionHandle();
        handle.setScope("response");
        handle.setFailClosed(true);
        return handle;
    }

    private String lastResponseBody() {
        return latestExchange.getResponse().getBodyAsString().block();
    }

    private MockServerWebExchange jsonRequest(final String body) {
        latestExchange = MockServerWebExchange.from(MockServerHttpRequest.post("/test")
                .contentType(MediaType.APPLICATION_JSON)
                .body(body));
        return latestExchange;
    }

    private void cacheHandle(final PiiDetectionHandle handle) {
        PiiDetectionRule rule = PiiDetectionRule.compile(CacheKeyUtils.INST.getKey(ruleData), handle);
        PiiDetectionPluginDataHandler.CACHED_RULES.get()
                .cachedHandle(CacheKeyUtils.INST.getKey(ruleData), rule);
    }

    private static final class RecordingChain implements ShenyuPluginChain {

        private final List<ServerWebExchange> executed = new CopyOnWriteArrayList<>();

        private volatile String forwardedBody = "";

        @Override
        public Mono<Void> execute(final ServerWebExchange exchange) {
            executed.add(exchange);
            return exchange.getRequest().getBody()
                    .map(buffer -> {
                        byte[] bytes = new byte[buffer.readableByteCount()];
                        buffer.read(bytes);
                        DataBufferUtils.release(buffer);
                        return new String(bytes, StandardCharsets.UTF_8);
                    })
                    .collect(StringBuilder::new, StringBuilder::append)
                    .map(StringBuilder::toString)
                    .doOnNext(body -> forwardedBody = body)
                    .then();
        }
    }
}
