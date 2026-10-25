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

package org.apache.shenyu.plugin.tracing;

import io.opentelemetry.api.common.AttributeKey;
import io.opentelemetry.sdk.testing.exporter.InMemorySpanExporter;
import io.opentelemetry.sdk.trace.SdkTracerProvider;
import io.opentelemetry.sdk.trace.data.SpanData;
import io.opentelemetry.sdk.trace.export.SimpleSpanProcessor;
import io.opentelemetry.sdk.trace.samplers.Sampler;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentMatchers;
import org.mockito.Mockito;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.URI;
import java.util.List;
import java.util.Objects;
import java.util.Set;

/**
 * The Test Case For the attributes of the gateway span.
 */
public class TracingSpanAttributesTest {

    private static final String PATH = "/http/order/findById/123";

    private static final String ROUTE = "/http/order/**";

    private InMemorySpanExporter spanExporter;

    private SdkTracerProvider tracerProvider;

    private TracingPlugin tracingPlugin;

    private ShenyuPluginChain chain;

    @BeforeEach
    public void setUp() {
        this.spanExporter = InMemorySpanExporter.create();
        this.tracerProvider = SdkTracerProvider.builder()
                .addSpanProcessor(SimpleSpanProcessor.create(spanExporter))
                .setSampler(Sampler.parentBased(Sampler.alwaysOn()))
                .build();
        this.tracingPlugin = new TracingPlugin(tracerProvider.get(TracingConstants.INSTRUMENTATION_NAME));
        this.chain = Mockito.mock(ShenyuPluginChain.class);
    }

    @AfterEach
    public void tearDown() {
        tracerProvider.close();
    }

    @Test
    public void testRequestAttributesAreRecorded() {
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(Mono.empty());
        ShenyuContext shenyuContext = createShenyuContext();
        // the decorator of the http like protocols stores the request path in the method field
        shenyuContext.setMethod(PATH);
        ServerWebExchange exchange = createExchange(shenyuContext, null);
        StepVerifier.create(tracingPlugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        SpanData span = onlyFinishedSpan();
        Assertions.assertEquals("GET", span.getAttributes().get(AttributeKey.stringKey(TracingConstants.HTTP_REQUEST_METHOD)));
        Assertions.assertEquals("http", span.getAttributes().get(AttributeKey.stringKey(TracingConstants.RPC_TYPE)));
        Assertions.assertEquals("order-service", span.getAttributes().get(AttributeKey.stringKey(TracingConstants.ROUTE_MODULE)));
        Assertions.assertNull(span.getAttributes().get(AttributeKey.stringKey(TracingConstants.ROUTE_METHOD)));
        Assertions.assertNull(span.getAttributes().get(AttributeKey.stringKey(TracingConstants.HTTP_ROUTE)));
    }

    @Test
    public void testRouteMethodIsRecordedWhenItIsNotTheRequestPath() {
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(Mono.empty());
        ShenyuContext shenyuContext = createShenyuContext();
        shenyuContext.setRpcType("dubbo");
        shenyuContext.setMethod("com.shenyu.api.OrderService");
        ServerWebExchange exchange = createExchange(shenyuContext, null);
        StepVerifier.create(tracingPlugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        SpanData span = onlyFinishedSpan();
        Assertions.assertEquals("com.shenyu.api.OrderService",
                span.getAttributes().get(AttributeKey.stringKey(TracingConstants.ROUTE_METHOD)));
    }

    @Test
    public void testSpanNameAndRouteComeFromTheRegisteredApi() {
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(Mono.empty());
        ServerWebExchange exchange = createExchange(createShenyuContext(), MetaData.builder().path(ROUTE).build());
        StepVerifier.create(tracingPlugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        SpanData span = onlyFinishedSpan();
        // the name follows the http conventions, it must not grow with the request path
        Assertions.assertEquals("GET " + ROUTE, span.getName());
        Assertions.assertEquals(ROUTE, span.getAttributes().get(AttributeKey.stringKey(TracingConstants.HTTP_ROUTE)));
    }

    @Test
    public void testSpanNameWithoutARegisteredApiIsTheMethodOnly() {
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(Mono.empty());
        ServerWebExchange exchange = createExchange(createShenyuContext(), null);
        StepVerifier.create(tracingPlugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        Assertions.assertEquals("GET", onlyFinishedSpan().getName());
    }

    @Test
    public void testContextPathIsReadOnceTheChainHasRun() {
        ServerWebExchange exchange = createExchange(createShenyuContext(), null);
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenAnswer(invocation -> {
            // the context path plugin writes the context path while the chain runs
            exchange.getAttributes().put(Constants.CONTEXT_PATH, "/http");
            return Mono.empty();
        });
        StepVerifier.create(tracingPlugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        Assertions.assertEquals("/http",
                onlyFinishedSpan().getAttributes().get(AttributeKey.stringKey(TracingConstants.CONTEXT_PATH)));
    }

    @Test
    public void testContextPathFallsBackToTheShenyuContext() {
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(Mono.empty());
        ShenyuContext shenyuContext = createShenyuContext();
        shenyuContext.setRpcType("dubbo");
        shenyuContext.setContextPath("/dubbo");
        ServerWebExchange exchange = createExchange(shenyuContext, null);
        StepVerifier.create(tracingPlugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        Assertions.assertEquals("/dubbo",
                onlyFinishedSpan().getAttributes().get(AttributeKey.stringKey(TracingConstants.CONTEXT_PATH)));
    }

    @Test
    public void testUpstreamAndSelectorAreRecordedOnceTheRequestIsRouted() {
        ServerWebExchange exchange = createExchange(createShenyuContext(), MetaData.builder().path(ROUTE).build());
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenAnswer(invocation -> {
            exchange.getAttributes().put(Constants.CONTEXT_PATH, "/http");
            exchange.getAttributes().put(Constants.HTTP_URI, URI.create("http://upstream-host:9195" + PATH));
            exchange.getAttributes().put(Constants.DIVIDE_SELECTOR_ID, "selector-1");
            exchange.getResponse().setStatusCode(HttpStatus.OK);
            return Mono.empty();
        });
        StepVerifier.create(tracingPlugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        SpanData span = onlyFinishedSpan();
        Assertions.assertEquals("upstream-host",
                span.getAttributes().get(AttributeKey.stringKey(TracingConstants.UPSTREAM_HOST)));
        Assertions.assertEquals("selector-1",
                span.getAttributes().get(AttributeKey.stringKey(TracingConstants.SELECTOR_ID)));
        // the server of a gateway span is the gateway itself, the upstream is a gateway specific key
        Assertions.assertNull(span.getAttributes().get(AttributeKey.stringKey("server.address")));
        // the recorded attributes are a closed set, so a future change cannot silently add an
        // attribute which grows with the request path or with the query string.
        Assertions.assertEquals(Set.of(
                AttributeKey.stringKey(TracingConstants.HTTP_REQUEST_METHOD),
                AttributeKey.stringKey(TracingConstants.RPC_TYPE),
                AttributeKey.stringKey(TracingConstants.ROUTE_MODULE),
                AttributeKey.stringKey(TracingConstants.HTTP_ROUTE),
                AttributeKey.stringKey(TracingConstants.CONTEXT_PATH),
                AttributeKey.stringKey(TracingConstants.UPSTREAM_HOST),
                AttributeKey.stringKey(TracingConstants.SELECTOR_ID),
                AttributeKey.longKey(TracingConstants.HTTP_RESPONSE_STATUS_CODE)),
                span.getAttributes().asMap().keySet());
    }

    @Test
    public void testRequestPathAndQueryAreNotRecorded() {
        MockServerHttpRequest request = MockServerHttpRequest.get("http://localhost" + PATH)
                .queryParam("debug", "true")
                .build();
        ServerWebExchange exchange = MockServerWebExchange.from(request);
        exchange.getAttributes().put(Constants.CONTEXT, createShenyuContext());
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenAnswer(invocation -> {
            exchange.getResponse().setStatusCode(HttpStatus.OK);
            return Mono.empty();
        });
        StepVerifier.create(tracingPlugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        SpanData span = onlyFinishedSpan();
        // the recorded attributes are a closed set, so a future change cannot silently add an
        // attribute which grows with the request path or with the query string.
        Assertions.assertEquals(Set.of(
                AttributeKey.stringKey(TracingConstants.HTTP_REQUEST_METHOD),
                AttributeKey.stringKey(TracingConstants.RPC_TYPE),
                AttributeKey.stringKey(TracingConstants.ROUTE_MODULE),
                AttributeKey.longKey(TracingConstants.HTTP_RESPONSE_STATUS_CODE)),
                span.getAttributes().asMap().keySet());
        span.getAttributes().asMap().values().forEach(value -> {
            String recorded = String.valueOf(value);
            Assertions.assertFalse(recorded.contains("findById"), "the request path must not be an attribute value");
            Assertions.assertFalse(recorded.contains("123"), "the path parameter must not be an attribute value");
            Assertions.assertFalse(recorded.contains("debug"), "the query string must not be an attribute value");
        });
        Assertions.assertNull(span.getAttributes().get(AttributeKey.stringKey("url.path")));
        Assertions.assertNull(span.getAttributes().get(AttributeKey.stringKey("http.target")));
    }

    /**
     * The span of a request has to be ended exactly once, whatever the terminal signal is.
     *
     * @return the single finished span of the request
     */
    private SpanData onlyFinishedSpan() {
        List<SpanData> spans = spanExporter.getFinishedSpanItems();
        Assertions.assertEquals(1, spans.size());
        return spans.get(0);
    }

    private ServerWebExchange createExchange(final ShenyuContext shenyuContext, final MetaData metaData) {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost" + PATH).build());
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
        if (Objects.nonNull(metaData)) {
            exchange.getAttributes().put(Constants.META_DATA, metaData);
        }
        return exchange;
    }

    private ShenyuContext createShenyuContext() {
        ShenyuContext shenyuContext = new ShenyuContext();
        shenyuContext.setHttpMethod("GET");
        shenyuContext.setRpcType("http");
        shenyuContext.setModule("order-service");
        shenyuContext.setPath(PATH);
        return shenyuContext;
    }
}
