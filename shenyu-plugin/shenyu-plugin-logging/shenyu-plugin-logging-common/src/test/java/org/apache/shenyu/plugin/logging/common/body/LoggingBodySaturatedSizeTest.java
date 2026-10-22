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
import org.apache.shenyu.plugin.logging.common.collector.LogCollector;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.desensitize.api.matcher.KeyWordMatch;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.core.publisher.Flux;

import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.util.Collections;

import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.when;

/**
 * Callers must rely on the writer's overflow state, because {@link BodyWriter#size()} saturates at
 * {@link Integer#MAX_VALUE} and cannot be compared against a limit of {@link Integer#MAX_VALUE}.
 */
public final class LoggingBodySaturatedSizeTest {

    @Test
    public void testRequestBodyStaysNullWhenSaturatedWriterOverflows() {
        ShenyuRequestLog log = new ShenyuRequestLog();
        MockServerWebExchange exchange = createExchange();
        try (MockedConstruction<BodyWriter> ignored = mockConstruction(BodyWriter.class, (writer, context) -> stubSaturatedOverflow(writer))) {
            LoggingServerHttpRequest<ShenyuRequestLog> request = new LoggingServerHttpRequest<>(exchange.getRequest(), log, Integer.MAX_VALUE);
            request.getBody().blockLast();
        }
        Assertions.assertNull(log.getRequestBody());
    }

    @Test
    public void testResponseBodyStaysNullWhenSaturatedWriterOverflows() {
        ShenyuRequestLog log = new ShenyuRequestLog();
        MockServerWebExchange exchange = createExchange();
        exchange.getResponse().getHeaders().setContentType(MediaType.TEXT_PLAIN);
        try (MockedConstruction<BodyWriter> ignored = mockConstruction(BodyWriter.class, (writer, context) -> stubSaturatedOverflow(writer))) {
            final LogCollector<ShenyuRequestLog> noCollector = null;
            LoggingServerHttpResponse<ShenyuRequestLog> response = new LoggingServerHttpResponse<>(exchange.getResponse(), log,
                    noCollector, false, new KeyWordMatch(Collections.emptySet()), "md5", Integer.MAX_VALUE);
            response.setExchange(exchange);
            response.writeWith(buffers()).block();
        }
        Assertions.assertNull(log.getResponseBody());
        Assertions.assertEquals(Integer.MAX_VALUE, log.getResponseContentLength());
    }

    private static void stubSaturatedOverflow(final BodyWriter writer) {
        when(writer.size()).thenReturn(Integer.MAX_VALUE);
        when(writer.isEmpty()).thenReturn(false);
        when(writer.isLimitExceeded()).thenReturn(true);
        when(writer.output()).thenReturn("");
    }

    private static MockServerWebExchange createExchange() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("http://localhost/saturated")
                .contentType(MediaType.TEXT_PLAIN).body(buffers()));
        ShenyuContext context = new ShenyuContext();
        context.setStartDateTime(LocalDateTime.now());
        exchange.getAttributes().put(Constants.CONTEXT, context);
        return exchange;
    }

    private static Flux<DataBuffer> buffers() {
        return Flux.just(DefaultDataBufferFactory.sharedInstance.wrap("hello".getBytes(StandardCharsets.UTF_8)));
    }
}
