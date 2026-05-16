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

package org.apache.shenyu.plugin.ai.proxy.enhanced.service;

import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.springframework.http.HttpStatus;
import org.springframework.web.reactive.function.client.WebClientResponseException;

import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

class UpstreamErrorLoggerTest {

    @Test
    void testLogWithWebClientResponseException() {
        Logger log = mock(Logger.class);
        WebClientResponseException ex = WebClientResponseException.create(
                429, "Too Many Requests", null, "rate limited".getBytes(StandardCharsets.UTF_8), StandardCharsets.UTF_8);

        UpstreamErrorLogger.logUpstreamError(log, ex, "stream");

        verify(log).error("[AiProxy] {} failed, status={}, upstreamBody={}",
                "stream", HttpStatus.TOO_MANY_REQUESTS, "rate limited", ex);
    }

    @Test
    void testLogWithWrappedWebClientResponseException() {
        Logger log = mock(Logger.class);
        WebClientResponseException inner = WebClientResponseException.create(
                500, "Internal Server Error", null, "server error".getBytes(StandardCharsets.UTF_8), StandardCharsets.UTF_8);
        RuntimeException wrapped = new RuntimeException("call failed", inner);

        UpstreamErrorLogger.logUpstreamError(log, wrapped, "non-stream");

        verify(log).error("[AiProxy] {} failed, status={}, upstreamBody={}",
                "non-stream", HttpStatus.INTERNAL_SERVER_ERROR, "server error", wrapped);
    }

    @Test
    void testLogWithGenericException() {
        Logger log = mock(Logger.class);
        RuntimeException ex = new RuntimeException("generic error");

        UpstreamErrorLogger.logUpstreamError(log, ex, "stream");

        verify(log).error("[AiProxy] {} failed", "stream", ex);
    }

    @Test
    void testLogWithNullExceptionDoesNotThrow() {
        Logger log = mock(Logger.class);
        assertDoesNotThrow(() -> UpstreamErrorLogger.logUpstreamError(log, null, "stream"));
    }
}
