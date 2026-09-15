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

package org.apache.shenyu.plugin.logging.common.body;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.logging.common.collector.LogCollector;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.desensitize.api.matcher.KeyWordMatch;
import org.junit.jupiter.api.Test;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.GenericApplicationContext;
import org.springframework.http.HttpStatusCode;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.server.ServerWebExchange;

import java.time.LocalDateTime;
import java.util.Collections;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for status handling in {@link LoggingServerHttpResponse}.
 */
public final class LoggingServerHttpResponseStatusTest {

    @Test
    public void testLogErrorWithNonStandardStatusCode() {
        ApplicationContext previousContext = SpringBeanUtils.getInstance().getApplicationContext();
        try (GenericApplicationContext applicationContext = new GenericApplicationContext()) {
            applicationContext.registerBean(DefaultShenyuResult.class);
            applicationContext.refresh();
            SpringBeanUtils.getInstance().setApplicationContext(applicationContext);

            ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/").build());
            ShenyuContext shenyuContext = new ShenyuContext();
            shenyuContext.setStartDateTime(LocalDateTime.now());
            exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);

            AtomicReference<ShenyuRequestLog> collectedLog = new AtomicReference<>();
            LogCollector<ShenyuRequestLog> logCollector = new RecordingLogCollector(collectedLog);
            LoggingServerHttpResponse<ShenyuRequestLog> response = new LoggingServerHttpResponse<>(exchange.getResponse(),
                    new ShenyuRequestLog(), logCollector, false, Collections.emptySet(), "dataMaskByCharReplace");
            response.setExchange(exchange);

            response.logError(new ResponseStatusException(HttpStatusCode.valueOf(599), "error"));

            ShenyuRequestLog requestLog = collectedLog.get();
            assertNotNull(requestLog);
            assertEquals(599, requestLog.getStatus());
            assertTrue(requestLog.getResponseBody().contains("\"message\":\"599\""));
        } finally {
            SpringBeanUtils.getInstance().setApplicationContext(previousContext);
        }
    }

    private static final class RecordingLogCollector implements LogCollector<ShenyuRequestLog> {

        private final AtomicReference<ShenyuRequestLog> collectedLog;

        private RecordingLogCollector(final AtomicReference<ShenyuRequestLog> collectedLog) {
            this.collectedLog = collectedLog;
        }

        @Override
        public void start() {
        }

        @Override
        public void desensitize(final ShenyuRequestLog log, final KeyWordMatch keyWordMatch,
                                final String desensitizeAlg) {
        }

        @Override
        public void collect(final ShenyuRequestLog log) {
            collectedLog.set(log);
        }

        @Override
        public void close() {
        }
    }
}
