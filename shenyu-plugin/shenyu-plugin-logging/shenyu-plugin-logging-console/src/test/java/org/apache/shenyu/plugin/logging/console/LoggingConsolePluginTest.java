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

package org.apache.shenyu.plugin.logging.console;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.logging.desensitize.api.enums.DataDesensitizeEnum;
import org.apache.shenyu.plugin.logging.desensitize.api.factory.DataDesensitizeFactory;
import org.apache.shenyu.plugin.logging.desensitize.api.matcher.KeyWordMatch;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpResponse;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Flux;
import reactor.test.StepVerifier;

import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

/**
 * The Test Case For DebugPlugin.
 */
@ExtendWith(MockitoExtension.class)
public final class LoggingConsolePluginTest {

    private LoggingConsolePlugin loggingConsolePlugin;

    private ServerWebExchange exchange;

    private RuleData ruleData;

    private ShenyuPluginChain chain;

    private SelectorData selectorData;

    @BeforeEach
    public void setUp() {
        this.loggingConsolePlugin = new LoggingConsolePlugin();
        this.ruleData = mock(RuleData.class);
        this.chain = mock(ShenyuPluginChain.class);
        this.selectorData = mock(SelectorData.class);
        MockServerHttpRequest request = MockServerHttpRequest
                .get("localhost")
                .remoteAddress(new InetSocketAddress(8090))
                .header("X-source", "mock test")
                .queryParam("queryParam", "Hello,World")
                .build();
        this.exchange = spy(MockServerWebExchange.from(request));
    }

    @Test
    public void testDoExecute() {
        ServerWebExchange.Builder builder = mock(ServerWebExchange.Builder.class);
        when(exchange.mutate()).thenReturn(builder);
        when(builder.request(any(LoggingConsolePlugin.LoggingServerHttpRequest.class))).thenReturn(builder);
        when(builder.response(any(LoggingConsolePlugin.LoggingServerHttpResponse.class))).thenReturn(builder);
        when(builder.build()).thenReturn(exchange);
        when(chain.execute(any())).thenReturn(Mono.empty());
        Mono<Void> result = loggingConsolePlugin.doExecute(exchange, chain, selectorData, ruleData);
        // Sorry, I do not how to mock this case by an simply way, so I give up.

        StepVerifier.create(result).expectSubscription().verifyComplete();
    }

    @Test
    public void testDesensitizationAlgorithmIsIsolatedAcrossDecorators() {
        final String body = "{\"password\":\"secret\"}";
        final KeyWordMatch matcher = new KeyWordMatch(Collections.singleton("password"));
        final String md5Algorithm = DataDesensitizeEnum.MD5_ENCRYPT.getDataDesensitizeAlg();
        final String replaceAlgorithm = DataDesensitizeEnum.CHARACTER_REPLACE.getDataDesensitizeAlg();
        final String md5Value = DataDesensitizeFactory.selectDesensitize("secret", md5Algorithm);
        final String replaceValue = DataDesensitizeFactory.selectDesensitize("secret", replaceAlgorithm);
        final StringBuilder md5RequestLog = new StringBuilder();
        final StringBuilder replaceRequestLog = new StringBuilder();
        final LoggingConsolePlugin.LoggingServerHttpRequest md5Request = new LoggingConsolePlugin.LoggingServerHttpRequest(
                MockServerHttpRequest.post("/").contentType(MediaType.APPLICATION_JSON).body(body),
                md5RequestLog, true, matcher, md5Algorithm);
        final LoggingConsolePlugin.LoggingServerHttpRequest replaceRequest = new LoggingConsolePlugin.LoggingServerHttpRequest(
                MockServerHttpRequest.post("/").contentType(MediaType.APPLICATION_JSON).body(body),
                replaceRequestLog, true, matcher, replaceAlgorithm);

        Flux.merge(md5Request.getBody(), replaceRequest.getBody()).blockLast();

        assertTrue(md5RequestLog.toString().contains(md5Value));
        assertFalse(md5RequestLog.toString().contains(replaceValue));
        assertTrue(replaceRequestLog.toString().contains(replaceValue));
        assertFalse(replaceRequestLog.toString().contains(md5Value));

        final StringBuilder md5ResponseLog = new StringBuilder();
        final StringBuilder replaceResponseLog = new StringBuilder();
        final MockServerHttpResponse md5Response = new MockServerHttpResponse();
        final MockServerHttpResponse replaceResponse = new MockServerHttpResponse();
        md5Response.getHeaders().setContentType(MediaType.APPLICATION_JSON);
        replaceResponse.getHeaders().setContentType(MediaType.APPLICATION_JSON);
        final LoggingConsolePlugin.LoggingServerHttpResponse md5Decorator = loggingConsolePlugin.new LoggingServerHttpResponse(
                md5Response, md5ResponseLog, true, matcher, md5Algorithm);
        final LoggingConsolePlugin.LoggingServerHttpResponse replaceDecorator = loggingConsolePlugin.new LoggingServerHttpResponse(
                replaceResponse, replaceResponseLog, true, matcher, replaceAlgorithm);
        final DataBuffer md5Body = md5Response.bufferFactory().wrap(body.getBytes(StandardCharsets.UTF_8));
        final DataBuffer replaceBody = replaceResponse.bufferFactory().wrap(body.getBytes(StandardCharsets.UTF_8));

        Mono.when(md5Decorator.writeWith(Mono.just(md5Body)), replaceDecorator.writeWith(Mono.just(replaceBody))).block();

        assertTrue(md5ResponseLog.toString().contains(md5Value));
        assertFalse(md5ResponseLog.toString().contains(replaceValue));
        assertTrue(replaceResponseLog.toString().contains(replaceValue));
        assertFalse(replaceResponseLog.toString().contains(md5Value));
    }

    @Test
    public void testGetOrder() {
        assertEquals(loggingConsolePlugin.getOrder(), PluginEnum.LOGGING_CONSOLE.getCode());
    }

    @Test
    public void testNamed() {
        assertEquals(loggingConsolePlugin.named(), PluginEnum.LOGGING_CONSOLE.getName());
    }

    @Test
    public void testSkip() {
        assertFalse(loggingConsolePlugin.skip(exchange));
    }
}
