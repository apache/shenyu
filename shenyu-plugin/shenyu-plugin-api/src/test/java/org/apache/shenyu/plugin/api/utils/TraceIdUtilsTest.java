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

package org.apache.shenyu.plugin.api.utils;

import org.apache.shenyu.common.constant.Constants;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

/**
 * The Test Case For TraceIdUtils.
 */
public class TraceIdUtilsTest {

    private static final String TRACE_ID = "4bf92f3577b34da6a3ce929d0e0e4736";

    @Test
    public void testTraceIdOfATracedRequest() {
        ServerWebExchange exchange = createExchange();
        exchange.getAttributes().put(Constants.SHENYU_AGENT_TRACE_ID, TRACE_ID);
        Assertions.assertEquals(TRACE_ID, TraceIdUtils.traceId(exchange));
    }

    @Test
    public void testTraceIdFallsBackToTheLogPrefixWithoutATracedRequest() {
        ServerWebExchange exchange = createExchange();
        String traceId = TraceIdUtils.traceId(exchange);
        Assertions.assertNotNull(traceId);
        Assertions.assertEquals(exchange.getLogPrefix(), traceId);
    }

    private ServerWebExchange createExchange() {
        return MockServerWebExchange.from(
                MockServerHttpRequest.get("http://localhost/http/order/findById").build());
    }
}
