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
import io.opentelemetry.api.trace.StatusCode;
import io.opentelemetry.sdk.testing.exporter.InMemorySpanExporter;
import io.opentelemetry.sdk.trace.SdkTracerProvider;
import io.opentelemetry.sdk.trace.data.SpanData;
import io.opentelemetry.sdk.trace.export.SimpleSpanProcessor;
import io.opentelemetry.sdk.trace.samplers.Sampler;
import org.apache.shenyu.common.constant.Constants;
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
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.List;
import java.util.concurrent.TimeoutException;

/**
 * The Test Case For the terminal paths of the gateway span.
 */
public class TracingSpanLifecycleTest {

    private static final String PATH = "/http/order/findById";

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
    public void testSuccessPath() {
        ServerWebExchange exchange = createExchange();
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenAnswer(invocation -> {
            exchange.getResponse().setStatusCode(HttpStatus.OK);
            return Mono.empty();
        });
        StepVerifier.create(tracingPlugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        SpanData span = onlyFinishedSpan();
        Assertions.assertEquals(StatusCode.UNSET, span.getStatus().getStatusCode());
        Assertions.assertEquals(Long.valueOf(200L), span.getAttributes().get(AttributeKey.longKey(TracingConstants.HTTP_RESPONSE_STATUS_CODE)));
        Assertions.assertTrue(span.getEvents().isEmpty());
        Assertions.assertNull(span.getAttributes().get(AttributeKey.stringKey(TracingConstants.ERROR_TYPE)));
    }

    @Test
    public void testClientErrorPath() {
        ServerWebExchange exchange = createExchange();
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenAnswer(invocation -> {
            exchange.getResponse().setStatusCode(HttpStatus.NOT_FOUND);
            return Mono.empty();
        });
        StepVerifier.create(tracingPlugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        SpanData span = onlyFinishedSpan();
        // a client error is not a server error, as the http conventions ask for
        Assertions.assertEquals(StatusCode.UNSET, span.getStatus().getStatusCode());
        Assertions.assertEquals(Long.valueOf(404L), span.getAttributes().get(AttributeKey.longKey(TracingConstants.HTTP_RESPONSE_STATUS_CODE)));
    }

    @Test
    public void testServerErrorPath() {
        ServerWebExchange exchange = createExchange();
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenAnswer(invocation -> {
            exchange.getResponse().setStatusCode(HttpStatus.SERVICE_UNAVAILABLE);
            return Mono.empty();
        });
        StepVerifier.create(tracingPlugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        SpanData span = onlyFinishedSpan();
        Assertions.assertEquals(StatusCode.ERROR, span.getStatus().getStatusCode());
        Assertions.assertEquals("HTTP 503", span.getStatus().getDescription());
        Assertions.assertEquals(Long.valueOf(503L), span.getAttributes().get(AttributeKey.longKey(TracingConstants.HTTP_RESPONSE_STATUS_CODE)));
    }

    @Test
    public void testClientErrorExceptionPath() {
        ServerWebExchange exchange = createExchange();
        // the local dispatcher refuses a request with a 403 exception
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(Mono.error(
                new ResponseStatusException(HttpStatus.FORBIDDEN, "The key is not correct.")));
        StepVerifier.create(tracingPlugin.execute(exchange, chain))
                .expectSubscription()
                .expectError(ResponseStatusException.class)
                .verify();
        SpanData span = onlyFinishedSpan();
        // a client error is not an error span, whether it is answered by a response or by an exception
        Assertions.assertEquals(StatusCode.UNSET, span.getStatus().getStatusCode());
        Assertions.assertEquals(Long.valueOf(403L),
                span.getAttributes().get(AttributeKey.longKey(TracingConstants.HTTP_RESPONSE_STATUS_CODE)));
        // the error type is recorded for an error span only, the type of a client error stays in the
        // exception event of the span.
        Assertions.assertNull(span.getAttributes().get(AttributeKey.stringKey(TracingConstants.ERROR_TYPE)));
        Assertions.assertTrue(exceptionEvent(span), "the span has to carry the exception event");
    }

    @Test
    public void testThrownExceptionPath() {
        ServerWebExchange exchange = createExchange();
        Mockito.when(chain.execute(ArgumentMatchers.any()))
                .thenReturn(Mono.error(new IllegalStateException("upstream failed")));
        StepVerifier.create(tracingPlugin.execute(exchange, chain))
                .expectSubscription()
                .expectError(IllegalStateException.class)
                .verify();
        SpanData span = onlyFinishedSpan();
        Assertions.assertEquals(StatusCode.ERROR, span.getStatus().getStatusCode());
        // only the type of the failure is used, its message can carry request data
        Assertions.assertEquals("IllegalStateException", span.getStatus().getDescription());
        Assertions.assertEquals("IllegalStateException",
                span.getAttributes().get(AttributeKey.stringKey(TracingConstants.ERROR_TYPE)));
        Assertions.assertTrue(exceptionEvent(span), "the span has to carry the exception event");
    }

    @Test
    public void testTimeoutPath() {
        ServerWebExchange exchange = createExchange();
        Mockito.when(chain.execute(ArgumentMatchers.any()))
                .thenReturn(Mono.error(new TimeoutException("read timeout")));
        StepVerifier.create(tracingPlugin.execute(exchange, chain))
                .expectSubscription()
                .expectError(TimeoutException.class)
                .verify();
        SpanData span = onlyFinishedSpan();
        Assertions.assertEquals(StatusCode.ERROR, span.getStatus().getStatusCode());
        Assertions.assertEquals(TracingConstants.ERROR_TYPE_TIMEOUT,
                span.getAttributes().get(AttributeKey.stringKey(TracingConstants.ERROR_TYPE)));
    }

    @Test
    public void testProtocolTimeoutPath() {
        ServerWebExchange exchange = createExchange();
        // the http client plugin throws its own timeout exception, which does not extend TimeoutException
        Mockito.when(chain.execute(ArgumentMatchers.any()))
                .thenReturn(Mono.error(new RequestTimeoutException()));
        StepVerifier.create(tracingPlugin.execute(exchange, chain))
                .expectSubscription()
                .expectError(RequestTimeoutException.class)
                .verify();
        SpanData span = onlyFinishedSpan();
        Assertions.assertEquals(TracingConstants.ERROR_TYPE_TIMEOUT,
                span.getAttributes().get(AttributeKey.stringKey(TracingConstants.ERROR_TYPE)));
    }

    @Test
    public void testRetriedOutRequestPath() {
        ServerWebExchange exchange = createExchange();
        // the http client retry strategy answers a stale request with 408 and wraps the timeout
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(Mono.error(new ResponseStatusException(
                HttpStatus.REQUEST_TIMEOUT, "the retry gave up", new RequestTimeoutException())));
        StepVerifier.create(tracingPlugin.execute(exchange, chain))
                .expectSubscription()
                .expectError(ResponseStatusException.class)
                .verify();
        SpanData span = onlyFinishedSpan();
        Assertions.assertEquals(StatusCode.ERROR, span.getStatus().getStatusCode());
        // the status the gateway decided is recorded, the error handler writes its own status later
        Assertions.assertEquals(Long.valueOf(408L),
                span.getAttributes().get(AttributeKey.longKey(TracingConstants.HTTP_RESPONSE_STATUS_CODE)));
        Assertions.assertEquals(TracingConstants.ERROR_TYPE_TIMEOUT,
                span.getAttributes().get(AttributeKey.stringKey(TracingConstants.ERROR_TYPE)));
    }

    @Test
    public void testWrappedTimeoutPath() {
        ServerWebExchange exchange = createExchange();
        // the http client plugin maps a timeout of the upstream call to a 504 which wraps it
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(Mono.error(new ResponseStatusException(
                HttpStatus.GATEWAY_TIMEOUT, "upstream timeout", new TimeoutException("read timeout"))));
        StepVerifier.create(tracingPlugin.execute(exchange, chain))
                .expectSubscription()
                .expectError(ResponseStatusException.class)
                .verify();
        SpanData span = onlyFinishedSpan();
        Assertions.assertEquals(StatusCode.ERROR, span.getStatus().getStatusCode());
        Assertions.assertEquals(Long.valueOf(504L),
                span.getAttributes().get(AttributeKey.longKey(TracingConstants.HTTP_RESPONSE_STATUS_CODE)));
        Assertions.assertEquals(TracingConstants.ERROR_TYPE_TIMEOUT,
                span.getAttributes().get(AttributeKey.stringKey(TracingConstants.ERROR_TYPE)));
    }

    @Test
    public void testTimeoutResponsePath() {
        ServerWebExchange exchange = createExchange();
        // the fallback of a circuit breaker answers the timeout with a response and completes
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenAnswer(invocation -> {
            exchange.getResponse().setStatusCode(HttpStatus.GATEWAY_TIMEOUT);
            return Mono.empty();
        });
        StepVerifier.create(tracingPlugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        SpanData span = onlyFinishedSpan();
        Assertions.assertEquals(StatusCode.ERROR, span.getStatus().getStatusCode());
        Assertions.assertEquals(TracingConstants.ERROR_TYPE_TIMEOUT,
                span.getAttributes().get(AttributeKey.stringKey(TracingConstants.ERROR_TYPE)));
    }

    @Test
    public void testRequestTimeoutResponsePath() {
        ServerWebExchange exchange = createExchange();
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenAnswer(invocation -> {
            exchange.getResponse().setStatusCode(HttpStatus.REQUEST_TIMEOUT);
            return Mono.empty();
        });
        StepVerifier.create(tracingPlugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        SpanData span = onlyFinishedSpan();
        // a timeout is an error span even when it is reported with a client status code
        Assertions.assertEquals(StatusCode.ERROR, span.getStatus().getStatusCode());
        Assertions.assertEquals(TracingConstants.ERROR_TYPE_TIMEOUT,
                span.getAttributes().get(AttributeKey.stringKey(TracingConstants.ERROR_TYPE)));
    }

    @Test
    public void testCauseChainTimeoutPath() {
        ServerWebExchange exchange = createExchange();
        // a 500 status is not a timeout by itself, only the wrapped exception tells it apart
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(Mono.error(new ResponseStatusException(
                HttpStatus.INTERNAL_SERVER_ERROR, "upstream failed", new TimeoutException("read timeout"))));
        StepVerifier.create(tracingPlugin.execute(exchange, chain))
                .expectSubscription()
                .expectError(ResponseStatusException.class)
                .verify();
        SpanData span = onlyFinishedSpan();
        Assertions.assertEquals(StatusCode.ERROR, span.getStatus().getStatusCode());
        Assertions.assertEquals(Long.valueOf(500L),
                span.getAttributes().get(AttributeKey.longKey(TracingConstants.HTTP_RESPONSE_STATUS_CODE)));
        Assertions.assertEquals(TracingConstants.ERROR_TYPE_TIMEOUT,
                span.getAttributes().get(AttributeKey.stringKey(TracingConstants.ERROR_TYPE)));
    }

    @Test
    public void testCauseChainClientErrorPath() {
        ServerWebExchange exchange = createExchange();
        // the status is wrapped, only the cause chain of the failure carries it
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(
                Mono.error(new RuntimeException(new ResponseStatusException(HttpStatus.PAYLOAD_TOO_LARGE, "too large"))));
        StepVerifier.create(tracingPlugin.execute(exchange, chain))
                .expectSubscription()
                .expectError(RuntimeException.class)
                .verify();
        SpanData span = onlyFinishedSpan();
        Assertions.assertEquals(StatusCode.UNSET, span.getStatus().getStatusCode());
        Assertions.assertEquals(Long.valueOf(413L),
                span.getAttributes().get(AttributeKey.longKey(TracingConstants.HTTP_RESPONSE_STATUS_CODE)));
        Assertions.assertNull(span.getAttributes().get(AttributeKey.stringKey(TracingConstants.ERROR_TYPE)));
    }

    @Test
    public void testCancellationPath() {
        ServerWebExchange exchange = createExchange();
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(Mono.never());
        StepVerifier.create(tracingPlugin.execute(exchange, chain)).expectSubscription().thenCancel().verify();
        SpanData span = onlyFinishedSpan();
        Assertions.assertEquals(StatusCode.UNSET, span.getStatus().getStatusCode());
        Assertions.assertEquals(Boolean.TRUE, span.getAttributes().get(AttributeKey.booleanKey(TracingConstants.CANCELLED)));
    }

    /**
     * The span of a request has to be ended exactly once, whatever the terminal signal is, therefore
     * the exporter must only see a single span for the request.
     *
     * @return the single finished span of the request
     */
    private SpanData onlyFinishedSpan() {
        List<SpanData> spans = spanExporter.getFinishedSpanItems();
        Assertions.assertEquals(1, spans.size());
        return spans.get(0);
    }

    private boolean exceptionEvent(final SpanData span) {
        return span.getEvents().stream().anyMatch(event -> "exception".equals(event.getName()));
    }

    private ServerWebExchange createExchange() {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost" + PATH).build());
        ShenyuContext shenyuContext = new ShenyuContext();
        shenyuContext.setHttpMethod("GET");
        shenyuContext.setPath(PATH);
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
        return exchange;
    }

    /**
     * A timeout exception of a protocol plugin, it does not extend {@link TimeoutException}.
     */
    private static final class RequestTimeoutException extends RuntimeException {

        private static final long serialVersionUID = -6123954847415409614L;
    }
}
