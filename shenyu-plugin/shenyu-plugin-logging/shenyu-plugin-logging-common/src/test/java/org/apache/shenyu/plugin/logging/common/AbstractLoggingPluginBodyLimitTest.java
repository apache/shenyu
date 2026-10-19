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

package org.apache.shenyu.plugin.logging.common;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.RemoteAddressResolver;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.logging.common.body.LoggingServerHttpRequest;
import org.apache.shenyu.plugin.logging.common.body.LoggingServerHttpResponse;
import org.apache.shenyu.plugin.logging.common.collector.LogCollector;
import org.apache.shenyu.plugin.logging.common.config.GenericApiConfig;
import org.apache.shenyu.plugin.logging.common.config.GenericGlobalConfig;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.common.handler.AbstractLogPluginDataHandler;
import org.apache.shenyu.plugin.logging.desensitize.api.matcher.KeyWordMatch;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.GenericApplicationContext;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests body limits through the logging plugin and its request and response decorators.
 */
public final class AbstractLoggingPluginBodyLimitTest {

    private static final String PLUGIN_ID = "body-limit-test";

    private static final String OTHER_PLUGIN_ID = "other-body-limit-test";

    private final RecordingLogCollector collector = new RecordingLogCollector();

    private final TestLoggingPlugin plugin = new TestLoggingPlugin(collector);

    private final TestLogPluginDataHandler dataHandler = new TestLogPluginDataHandler(collector);

    private ApplicationContext previousContext;

    private GenericApplicationContext applicationContext;

    @BeforeEach
    public void setUp() {
        previousContext = SpringBeanUtils.getInstance().getApplicationContext();
        applicationContext = new GenericApplicationContext();
        applicationContext.registerBean(RemoteAddressResolver.class, () -> new RemoteAddressResolver() {
        });
        applicationContext.registerBean(DefaultShenyuResult.class);
        applicationContext.refresh();
        SpringBeanUtils.getInstance().setApplicationContext(applicationContext);
    }

    @AfterEach
    public void tearDown() {
        AbstractLogPluginDataHandler.getPluginGlobalConfigMap().remove(PLUGIN_ID);
        AbstractLogPluginDataHandler.getPluginGlobalConfigMap().remove(OTHER_PLUGIN_ID);
        SpringBeanUtils.getInstance().setApplicationContext(previousContext);
        applicationContext.close();
    }

    @Test
    public void testConfiguredRequestLimitAndResponseBoundary() {
        updatePluginConfig(PLUGIN_ID, 4, 8);
        ShenyuRequestLog log = execute(PLUGIN_ID, "hello", "response");
        assertNull(log.getRequestBody());
        assertEquals("response", log.getResponseBody());
    }

    @Test
    public void testConfiguredResponseLimitAndRequestBoundary() {
        updatePluginConfig(PLUGIN_ID, 8, 4);
        ShenyuRequestLog log = execute(PLUGIN_ID, "request!", "hello");
        assertEquals("request!", log.getRequestBody());
        assertNull(log.getResponseBody());
    }

    @Test
    public void testLimitsLargerThanDefault() {
        updatePluginConfig(PLUGIN_ID, 1048576, 1048576);
        String body = "x".repeat(524289);
        ShenyuRequestLog log = execute(PLUGIN_ID, body, body);
        assertTrue(body.equals(log.getRequestBody()), "Request body below the configured limit should be logged");
        assertTrue(body.equals(log.getResponseBody()), "Response body below the configured limit should be logged");
    }

    @Test
    public void testDefaultLimitsWhenConfigurationIsMissing() {
        String body = "x".repeat(524288);
        ShenyuRequestLog atLimit = execute(PLUGIN_ID, body, body);
        assertEquals(body, atLimit.getRequestBody());
        assertEquals(body, atLimit.getResponseBody());
        ShenyuRequestLog overLimit = execute(PLUGIN_ID, body + "x", body + "x");
        assertNull(overLimit.getRequestBody());
        assertNull(overLimit.getResponseBody());
    }

    @ParameterizedTest
    @ValueSource(strings = {"{}", "{\"maxRequestBody\":null,\"maxResponseBody\":null}"})
    public void testJsonDefaultsReplacePreviousLimits(final String configJson) {
        updatePluginConfig(PLUGIN_ID, 4, 4);
        updatePluginConfig(PLUGIN_ID, configJson);
        String body = "x".repeat(524288);
        ShenyuRequestLog atLimit = execute(PLUGIN_ID, body, body);
        assertEquals(body, atLimit.getRequestBody());
        assertEquals(body, atLimit.getResponseBody());
        ShenyuRequestLog overLimit = execute(PLUGIN_ID, body + "x", body + "x");
        assertNull(overLimit.getRequestBody());
        assertNull(overLimit.getResponseBody());
    }

    @Test
    public void testMissingJsonRequestLimitUsesDefault() {
        updatePluginConfig(PLUGIN_ID, 4, 4);
        updatePluginConfig(PLUGIN_ID, "{\"maxResponseBody\":4}");
        String body = "x".repeat(524288);
        ShenyuRequestLog atLimit = execute(PLUGIN_ID, body, "hello");
        assertEquals(body, atLimit.getRequestBody());
        assertNull(atLimit.getResponseBody());
        assertNull(execute(PLUGIN_ID, body + "x", "hello").getRequestBody());
    }

    @Test
    public void testMissingJsonResponseLimitUsesDefault() {
        updatePluginConfig(PLUGIN_ID, 4, 4);
        updatePluginConfig(PLUGIN_ID, "{\"maxRequestBody\":4}");
        String body = "x".repeat(524288);
        ShenyuRequestLog atLimit = execute(PLUGIN_ID, "hello", body);
        assertNull(atLimit.getRequestBody());
        assertEquals(body, atLimit.getResponseBody());
        assertNull(execute(PLUGIN_ID, "hello", body + "x").getResponseBody());
    }

    @ParameterizedTest
    @ValueSource(ints = {524288, 524289})
    public void testLegacyRequestConstructorUsesDefaultLimit(final int bodySize) {
        String body = "x".repeat(bodySize);
        ShenyuRequestLog log = new ShenyuRequestLog();
        LoggingServerHttpRequest<ShenyuRequestLog> request = new LoggingServerHttpRequest<>(createExchange(body).getRequest(), log);
        request.getBody().doOnNext(DataBufferUtils::release).blockLast();
        if (bodySize > 524288) {
            assertNull(log.getRequestBody());
        } else {
            assertEquals(body, log.getRequestBody());
        }
    }

    @ParameterizedTest
    @ValueSource(ints = {524288, 524289})
    public void testLegacyResponseConstructorUsesDefaultLimit(final int bodySize) {
        String body = "x".repeat(bodySize);
        MockServerWebExchange exchange = createExchange("hello");
        ShenyuRequestLog log = new ShenyuRequestLog();
        LoggingServerHttpResponse<ShenyuRequestLog> response = new LoggingServerHttpResponse<>(exchange.getResponse(),
                log, collector, false, Collections.emptySet(), "md5");
        response.setExchange(exchange);
        response.writeWith(buffers(body)).block();
        assertEquals(body, exchange.getResponse().getBodyAsString().block());
        if (bodySize > 524288) {
            assertNull(log.getResponseBody());
        } else {
            assertEquals(body, log.getResponseBody());
        }
    }

    @Test
    public void testPluginLimitsAreIsolated() {
        updatePluginConfig(PLUGIN_ID, 4, 4);
        updatePluginConfig(OTHER_PLUGIN_ID, 8, 8);
        ShenyuRequestLog small = execute(PLUGIN_ID, "hello", "hello");
        ShenyuRequestLog large = execute(OTHER_PLUGIN_ID, "hello", "hello");
        final ShenyuRequestLog smallAgain = execute(PLUGIN_ID, "hello", "hello");
        assertNull(small.getRequestBody());
        assertNull(small.getResponseBody());
        assertEquals("hello", large.getRequestBody());
        assertEquals("hello", large.getResponseBody());
        assertNull(smallAgain.getRequestBody());
        assertNull(smallAgain.getResponseBody());
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    public void testNestedPluginsKeepIndependentLimits(final boolean smallerLimitFirst) {
        updatePluginConfig(PLUGIN_ID, smallerLimitFirst ? 4 : 8, smallerLimitFirst ? 4 : 8);
        updatePluginConfig(OTHER_PLUGIN_ID, smallerLimitFirst ? 8 : 4, smallerLimitFirst ? 8 : 4);
        RecordingLogCollector otherCollector = new RecordingLogCollector();
        TestLoggingPlugin otherPlugin = new TestLoggingPlugin(otherCollector);
        MockServerWebExchange exchange = createExchange("hello");
        AtomicReference<String> forwardedRequest = new AtomicReference<>();
        plugin.doExecute(exchange, outer -> otherPlugin.doExecute(outer,
                inner -> forward(inner, forwardedRequest, "hello"), selector(OTHER_PLUGIN_ID), rule(OTHER_PLUGIN_ID)),
                selector(PLUGIN_ID), rule(PLUGIN_ID)).block();
        assertEquals("hello", forwardedRequest.get());
        assertEquals("hello", exchange.getResponse().getBodyAsString().block());
        ShenyuRequestLog outerLog = collector.log.get();
        assertNotNull(outerLog);
        assertEquals(smallerLimitFirst ? null : "hello", outerLog.getRequestBody());
        assertEquals(smallerLimitFirst ? null : "hello", outerLog.getResponseBody());
        assertEquals(HttpStatus.CREATED.value(), outerLog.getStatus());
        assertEquals(5, outerLog.getResponseContentLength());
        ShenyuRequestLog innerLog = otherCollector.log.get();
        assertNotNull(innerLog);
        assertEquals(smallerLimitFirst ? "hello" : null, innerLog.getRequestBody());
        assertEquals(smallerLimitFirst ? "hello" : null, innerLog.getResponseBody());
        assertEquals(HttpStatus.CREATED.value(), innerLog.getStatus());
        assertEquals(5, innerLog.getResponseContentLength());
    }

    @Test
    public void testConfigUpdateAppliesToNextRequest() {
        updatePluginConfig(PLUGIN_ID, 8, 8);
        MockServerWebExchange exchange = createExchange("hello");
        AtomicReference<String> forwardedRequest = new AtomicReference<>();
        Mono<Void> inFlight = plugin.doExecute(exchange, decorated -> forward(decorated, forwardedRequest, "hello"),
                selector(PLUGIN_ID), rule(PLUGIN_ID));
        updatePluginConfig(PLUGIN_ID, 4, 4);
        inFlight.block();
        assertEquals("hello", collector.log.get().getRequestBody());
        assertEquals("hello", collector.log.get().getResponseBody());
        ShenyuRequestLog nextRequest = execute(PLUGIN_ID, "hello", "hello");
        assertNull(nextRequest.getRequestBody());
        assertNull(nextRequest.getResponseBody());
        updatePluginConfig(PLUGIN_ID, 8, 8);
        ShenyuRequestLog updatedRequest = execute(PLUGIN_ID, "hello", "hello");
        assertEquals("hello", updatedRequest.getRequestBody());
        assertEquals("hello", updatedRequest.getResponseBody());
    }

    @Test
    public void testUtf8BytesAreAccumulatedAcrossChunks() {
        // two 3-byte UTF-8 characters, 6 bytes in total
        final String first = Character.toString(0x20AC);
        final String second = Character.toString(0x2122);
        final String body = first + second;
        updatePluginConfig(PLUGIN_ID, 5, 6);
        MockServerWebExchange exchange = createExchange(first, second);
        ShenyuRequestLog log = execute(PLUGIN_ID, exchange, body, first, second);
        assertNull(log.getRequestBody());
        assertEquals(body, log.getResponseBody());
        updatePluginConfig(PLUGIN_ID, 6, 5);
        log = execute(PLUGIN_ID, createExchange(first, second), body, first, second);
        assertEquals(body, log.getRequestBody());
        assertNull(log.getResponseBody());
    }

    @ParameterizedTest
    @ValueSource(ints = {0, -1})
    public void testNonPositiveLimitsOmitNonEmptyBodies(final int limit) {
        updatePluginConfig(PLUGIN_ID, limit, limit);
        ShenyuRequestLog log = execute(PLUGIN_ID, "hello", "hello");
        assertNull(log.getRequestBody());
        assertNull(log.getResponseBody());
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    public void testErrorResponseUsesConfiguredLimit(final boolean synchronous) {
        updatePluginConfig(PLUGIN_ID, 8, 1);
        ShenyuRequestLog small = executeError(synchronous);
        assertEquals(500, small.getStatus());
        assertTrue(small.getResponseContentLength() > 1);
        assertNull(small.getResponseBody());
        updatePluginConfig(PLUGIN_ID, 8, 1024);
        ShenyuRequestLog large = executeError(synchronous);
        assertEquals(500, large.getStatus());
        assertNotNull(large.getResponseBody());
        assertTrue(large.getResponseBody().contains("Internal Server Error"));
    }

    private ShenyuRequestLog executeError(final boolean synchronous) {
        IllegalStateException failure = new IllegalStateException("failure");
        assertSame(failure, assertThrows(IllegalStateException.class, () -> plugin.doExecute(createExchange("hello"), exchange -> {
            if (synchronous) {
                throw failure;
            }
            return Mono.error(failure);
        }, selector(PLUGIN_ID), rule(PLUGIN_ID)).block()));
        return collector.log.get();
    }

    private ShenyuRequestLog execute(final String pluginId, final String requestBody, final String responseBody) {
        return execute(pluginId, createExchange(requestBody), requestBody, responseBody);
    }

    private ShenyuRequestLog execute(final String pluginId, final MockServerWebExchange exchange,
                                     final String requestBody, final String... responseChunks) {
        AtomicReference<String> forwardedRequest = new AtomicReference<>();
        plugin.doExecute(exchange, decorated -> forward(decorated, forwardedRequest, responseChunks), selector(pluginId), rule(pluginId)).block();
        String responseBody = String.join("", responseChunks);
        assertEquals(requestBody, forwardedRequest.get());
        assertEquals(responseBody, exchange.getResponse().getBodyAsString().block());
        ShenyuRequestLog log = collector.log.get();
        assertNotNull(log);
        assertEquals(HttpStatus.CREATED.value(), log.getStatus());
        assertEquals(responseBody.getBytes(StandardCharsets.UTF_8).length, log.getResponseContentLength());
        return log;
    }

    private Mono<Void> forward(final ServerWebExchange exchange, final AtomicReference<String> forwardedRequest,
                               final String... responseChunks) {
        return DataBufferUtils.join(exchange.getRequest().getBody()).doOnNext(buffer -> {
            forwardedRequest.set(buffer.toString(StandardCharsets.UTF_8));
            DataBufferUtils.release(buffer);
        }).then(Mono.defer(() -> {
            exchange.getResponse().setStatusCode(HttpStatus.CREATED);
            exchange.getResponse().getHeaders().setContentType(MediaType.TEXT_PLAIN);
            return exchange.getResponse().writeWith(buffers(responseChunks));
        }));
    }

    private MockServerWebExchange createExchange(final String... chunks) {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("http://localhost/body-limit")
                .remoteAddress(new InetSocketAddress("127.0.0.1", 8000)).contentType(MediaType.TEXT_PLAIN).body(buffers(chunks)));
        ShenyuContext context = new ShenyuContext();
        context.setStartDateTime(LocalDateTime.now());
        exchange.getAttributes().put(Constants.CONTEXT, context);
        return exchange;
    }

    private Flux<DataBuffer> buffers(final String... chunks) {
        return Flux.fromArray(chunks).map(chunk -> DefaultDataBufferFactory.sharedInstance.wrap(chunk.getBytes(StandardCharsets.UTF_8)));
    }

    private void updatePluginConfig(final String pluginId, final int requestLimit, final int responseLimit) {
        updatePluginConfig(pluginId, "{\"maxRequestBody\":" + requestLimit + ",\"maxResponseBody\":" + responseLimit + "}");
    }

    private void updatePluginConfig(final String pluginId, final String configJson) {
        PluginData pluginData = new PluginData();
        pluginData.setId(pluginId);
        pluginData.setName(PluginEnum.LOGGING_KAFKA.getName());
        pluginData.setEnabled(true);
        pluginData.setConfig(configJson);
        dataHandler.handlerPlugin(pluginData);
    }

    private SelectorData selector(final String pluginId) {
        SelectorData selector = new SelectorData();
        selector.setId(pluginId + "-selector");
        selector.setPluginId(pluginId);
        return selector;
    }

    private RuleData rule(final String pluginId) {
        RuleData rule = new RuleData();
        rule.setId(pluginId + "-rule");
        rule.setSelectorId(pluginId + "-selector");
        return rule;
    }

    // Keep the handler's Singleton entry separate from production configuration classes.
    private static final class TestGlobalConfig extends GenericGlobalConfig {
    }

    private static final class TestLogPluginDataHandler extends AbstractLogPluginDataHandler<TestGlobalConfig, GenericApiConfig> {

        private final LogCollector<ShenyuRequestLog> collector;

        private TestLogPluginDataHandler(final LogCollector<ShenyuRequestLog> collector) {
            this.collector = collector;
        }

        @Override
        protected LogCollector<ShenyuRequestLog> logCollector() {
            return collector;
        }

        @Override
        protected void doRefreshConfig(final TestGlobalConfig globalLogConfig) {
        }

        @Override
        public String pluginNamed() {
            return PluginEnum.LOGGING_KAFKA.getName();
        }
    }

    private static final class TestLoggingPlugin extends AbstractLoggingPlugin<ShenyuRequestLog> {

        private final LogCollector<ShenyuRequestLog> collector;

        private TestLoggingPlugin(final LogCollector<ShenyuRequestLog> collector) {
            this.collector = collector;
        }

        @Override
        protected LogCollector<ShenyuRequestLog> logCollector() {
            return collector;
        }

        @Override
        protected PluginEnum pluginEnum() {
            return PluginEnum.LOGGING_KAFKA;
        }

        @Override
        protected ShenyuRequestLog doLogExecute(final ServerWebExchange exchange, final SelectorData selector, final RuleData rule) {
            return new ShenyuRequestLog();
        }
    }

    private static final class RecordingLogCollector implements LogCollector<ShenyuRequestLog> {

        private final AtomicReference<ShenyuRequestLog> log = new AtomicReference<>();

        @Override
        public void start() {
        }

        @Override
        public void close() {
        }

        @Override
        public void desensitize(final ShenyuRequestLog requestLog, final KeyWordMatch keyWordMatch, final String desensitizeAlg) {
        }

        @Override
        public void collect(final ShenyuRequestLog requestLog) {
            log.set(requestLog);
        }
    }
}
