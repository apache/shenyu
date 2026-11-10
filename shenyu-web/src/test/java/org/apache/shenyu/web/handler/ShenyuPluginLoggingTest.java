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

package org.apache.shenyu.web.handler;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.slf4j.LoggerFactory;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/** Tests the observable contract of default plugin timing logs. */
class ShenyuPluginLoggingTest {

    private static final String KEY = Constants.PLUGIN_START_TIME + "logging-test";

    private final Logger logger = (Logger) LoggerFactory.getLogger(ShenyuPlugin.class);

    private final ListAppender<ILoggingEvent> appender = new ListAppender<>();

    private final ShenyuPlugin plugin = new LoggingPlugin();

    private Level previousLevel;

    @BeforeEach
    void setUp() {
        previousLevel = logger.getLevel();
        appender.start();
        logger.addAppender(appender);
    }

    @AfterEach
    void tearDown() {
        logger.detachAppender(appender);
        appender.stop();
        logger.setLevel(previousLevel);
    }

    @Test
    void debugIncludesTraceAndCostAndCleansTiming() {
        logger.setLevel(Level.DEBUG);
        ServerWebExchange exchange = exchange();
        exchange.getAttributes().put(Constants.SHENYU_AGENT_TRACE_ID, "span-123");
        plugin.before(exchange);
        assertTrue(exchange.getAttributes().get(KEY) instanceof Long);
        plugin.after(exchange);
        assertFalse(exchange.getAttributes().containsKey(KEY));
        assertEquals(1, appender.list.size());
        assertTrue(appender.list.get(0).getFormattedMessage().matches("shenyu traceId:span-123, plugin named:logging-test, cost:[0-9]+"));
    }

    @Test
    void debugUsesRequestPrefixWithoutSpan() {
        logger.setLevel(Level.DEBUG);
        ServerWebExchange exchange = exchange();
        final String prefix = exchange.getLogPrefix();
        plugin.before(exchange);
        plugin.after(exchange);
        assertEquals(1, appender.list.size());
        assertTrue(appender.list.get(0).getFormattedMessage().startsWith("shenyu traceId:" + prefix + ", plugin named:logging-test, cost:"));
        assertFalse(exchange.getAttributes().containsKey(KEY));
    }

    @Test
    void infoRetainsTimingScopeAndCleanup() {
        checkDisabled(Level.INFO);
    }

    @Test
    void offRetainsTimingScopeAndCleanup() {
        checkDisabled(Level.OFF);
    }

    @Test
    void enablingDebugAfterBeforeWritesLog() {
        logger.setLevel(Level.OFF);
        ServerWebExchange exchange = exchange();
        plugin.before(exchange);
        logger.setLevel(Level.DEBUG);
        plugin.after(exchange);
        assertEquals(1, appender.list.size());
        assertFalse(exchange.getAttributes().containsKey(KEY));
    }

    @Test
    void disablingDebugAfterBeforeCleansWithoutLog() {
        logger.setLevel(Level.DEBUG);
        ServerWebExchange exchange = exchange();
        plugin.before(exchange);
        logger.setLevel(Level.OFF);
        plugin.after(exchange);
        assertTrue(appender.list.isEmpty());
        assertFalse(exchange.getAttributes().containsKey(KEY));
    }

    @Test
    void missingStartStillFailsWhenDebugIsOff() {
        logger.setLevel(Level.OFF);
        assertThrows(NullPointerException.class, () -> plugin.after(exchange()));
    }

    private void checkDisabled(final Level level) {
        logger.setLevel(level);
        ServerWebExchange exchange = exchange();
        plugin.before(exchange);
        assertNotNull(exchange.getAttributes().get(KEY));
        plugin.after(exchange);
        assertFalse(exchange.getAttributes().containsKey(KEY));
        assertTrue(appender.list.isEmpty());
    }

    private ServerWebExchange exchange() {
        return MockServerWebExchange.from(MockServerHttpRequest.get("/test").build());
    }

    private static final class LoggingPlugin implements ShenyuPlugin {

        @Override
        public int getOrder() {
            return 0;
        }

        @Override
        public String named() {
            return "logging-test";
        }

        @Override
        public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
            return Mono.empty();
        }
    }
}
