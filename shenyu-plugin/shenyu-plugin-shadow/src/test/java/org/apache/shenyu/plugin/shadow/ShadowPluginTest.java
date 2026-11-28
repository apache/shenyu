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

package org.apache.shenyu.plugin.shadow;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.ShadowHandle;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.shadow.config.ShadowSkipReason;
import org.apache.shenyu.plugin.shadow.dispatch.ShadowRuntime;
import org.apache.shenyu.plugin.shadow.dispatch.ShadowRuntimeRegistry;
import org.apache.shenyu.plugin.shadow.dispatch.ShadowTask;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * test case for {@link ShadowPlugin}.
 */
@ExtendWith(MockitoExtension.class)
public final class ShadowPluginTest {

    @Mock
    private ShadowRuntimeRegistry runtimeRegistry;

    @Mock
    private ShadowRuntime runtime;

    @Mock
    private ShenyuPluginChain chain;

    @Mock
    private SelectorData selector;

    private ShadowPlugin plugin;

    private ShadowHandle handle;

    @BeforeEach
    public void setUp() {
        plugin = new ShadowPlugin(runtimeRegistry);
        handle = new ShadowHandle();
        handle.setEnabled(true);
        handle.setTarget("http://shadow:8080");
        handle.setSampleRate(1D);
        lenient().when(runtimeRegistry.find(anyString())).thenReturn(runtime);
        lenient().when(runtime.getHandle()).thenReturn(handle);
        lenient().when(chain.execute(any())).thenReturn(Mono.empty());
    }

    @Test
    public void testMirrorsEligibleRequest() {
        when(runtime.mirror(any(), any())).thenReturn(true);
        ServerWebExchange exchange = exchange(MockServerHttpRequest.get("http://localhost/api/orders/42?a=1").build(), true);
        StepVerifier.create(plugin.doExecute(exchange, chain, selector, rule()))
                .expectSubscription()
                .verifyComplete();
        ArgumentCaptor<ShadowTask> captor = ArgumentCaptor.forClass(ShadowTask.class);
        verify(runtime).mirror(captor.capture(), any());
        assertEquals("http://shadow:8080/api/orders/42?a=1", captor.getValue().getUri().toString());
        assertEquals("GET", captor.getValue().getMethod());
    }

    @Test
    public void testMirrorsObservedBody() {
        handle.setMethods(Collections.singletonList("POST"));
        when(runtime.mirror(any(), any())).thenReturn(true);
        ServerWebExchange exchange = exchange(MockServerHttpRequest.post("http://localhost/api")
                .body("payload"), true);
        lenient().when(chain.execute(any())).thenAnswer(invocation -> {
            ServerWebExchange observed = invocation.getArgument(0);
            return DataBufferUtils.join(observed.getRequest().getBody()).then();
        });
        StepVerifier.create(plugin.doExecute(exchange, chain, selector, rule()))
                .expectSubscription()
                .verifyComplete();
        ArgumentCaptor<ShadowTask> captor = ArgumentCaptor.forClass(ShadowTask.class);
        verify(runtime).mirror(captor.capture(), any());
        assertEquals("payload", new String(captor.getValue().getBody(), StandardCharsets.UTF_8));
    }

    @Test
    public void testSkipsBodyOverBound() {
        handle.setMethods(Collections.singletonList("POST"));
        handle.setMaxRequestBodyBytes(2);
        ServerWebExchange exchange = exchange(MockServerHttpRequest.post("http://localhost/api")
                .body("payload"), true);
        lenient().when(chain.execute(any())).thenAnswer(invocation -> {
            ServerWebExchange observed = invocation.getArgument(0);
            return DataBufferUtils.join(observed.getRequest().getBody()).then();
        });
        StepVerifier.create(plugin.doExecute(exchange, chain, selector, rule()))
                .expectSubscription()
                .verifyComplete();
        verify(runtime).recordSkip(ShadowSkipReason.BODY_TOO_LARGE);
        verify(runtime, never()).mirror(any(), any());
    }

    @Test
    public void testSkipsIncompleteBody() {
        handle.setMethods(Collections.singletonList("POST"));
        ServerWebExchange exchange = exchange(MockServerHttpRequest.post("http://localhost/api")
                .header(HttpHeaders.CONTENT_LENGTH, "7")
                .body("payload"), true);
        StepVerifier.create(plugin.doExecute(exchange, chain, selector, rule()))
                .expectSubscription()
                .verifyComplete();
        verify(runtime).recordSkip(ShadowSkipReason.BODY_INCOMPLETE);
        verify(runtime, never()).mirror(any(), any());
    }

    @Test
    public void testRecordsCancelledPrimary() {
        when(chain.execute(any())).thenReturn(Mono.never());
        ServerWebExchange exchange = exchange(MockServerHttpRequest.get("http://localhost/api").build(), true);
        StepVerifier.create(plugin.doExecute(exchange, chain, selector, rule()))
                .thenCancel()
                .verify();
        verify(runtime).recordSkip(ShadowSkipReason.PRIMARY_CANCELLED);
        verify(runtime, never()).mirror(any(), any());
    }

    @Test
    public void testPassesThroughWithoutRuntime() {
        when(runtimeRegistry.find(anyString())).thenReturn(null);
        ServerWebExchange exchange = exchange(MockServerHttpRequest.get("http://localhost/api").build(), true);
        StepVerifier.create(plugin.doExecute(exchange, chain, selector, rule()))
                .expectSubscription()
                .verifyComplete();
        verify(chain).execute(any());
        verify(runtime, never()).mirror(any(), any());
    }

    @Test
    public void testSkipsNonHttpRequest() {
        ServerWebExchange exchange = exchange(MockServerHttpRequest.get("http://localhost/api").build(), false);
        StepVerifier.create(plugin.doExecute(exchange, chain, selector, rule()))
                .expectSubscription()
                .verifyComplete();
        verify(runtime).recordSkip(ShadowSkipReason.NOT_HTTP);
        verify(runtime, never()).mirror(any(), any());
    }

    @Test
    public void testSkipsMethodOutsideAllowlist() {
        ServerWebExchange exchange = exchange(MockServerHttpRequest.post("http://localhost/api").build(), true);
        StepVerifier.create(plugin.doExecute(exchange, chain, selector, rule()))
                .expectSubscription()
                .verifyComplete();
        verify(runtime).recordSkip(ShadowSkipReason.METHOD_NOT_ALLOWED);
        verify(runtime, never()).mirror(any(), any());
    }

    @Test
    public void testRecordsQueueOverflow() {
        when(runtime.mirror(any(), any())).thenReturn(false);
        ServerWebExchange exchange = exchange(MockServerHttpRequest.get("http://localhost/api").build(), true);
        StepVerifier.create(plugin.doExecute(exchange, chain, selector, rule()))
                .expectSubscription()
                .verifyComplete();
        verify(runtime).recordSkip(ShadowSkipReason.QUEUE_OVERFLOW);
    }

    private ServerWebExchange exchange(final MockServerHttpRequest request, final boolean http) {
        MockServerWebExchange exchange = MockServerWebExchange.from(request);
        if (http) {
            ShenyuContext context = new ShenyuContext();
            context.setRpcType(RpcTypeEnum.HTTP.getName());
            exchange.getAttributes().put(Constants.CONTEXT, context);
        }
        return exchange;
    }

    private RuleData rule() {
        RuleData rule = new RuleData();
        rule.setSelectorId("selector");
        rule.setId("rule");
        rule.setHandle("{}");
        return rule;
    }
}
