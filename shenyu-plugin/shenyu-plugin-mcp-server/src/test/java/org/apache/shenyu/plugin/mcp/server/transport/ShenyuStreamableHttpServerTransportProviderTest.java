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

package org.apache.shenyu.plugin.mcp.server.transport;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.function.server.HandlerStrategies;
import org.springframework.web.reactive.function.server.ServerRequest;
import org.springframework.web.reactive.function.server.ServerResponse;
import reactor.test.StepVerifier;

import java.util.Locale;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test case for {@link ShenyuStreamableHttpServerTransportProvider}.
 */
class ShenyuStreamableHttpServerTransportProviderTest {

    @Test
    void testPreflightUsesConfiguredHeadersAndMethods() {
        ShenyuStreamableHttpServerTransportProvider provider =
                new ShenyuStreamableHttpServerTransportProvider(new ObjectMapper(),
                        "/mcp/streamablehttp", "Content-Type, XRequest");
        ServerRequest request = createRequest(MockServerHttpRequest.options("/mcp/streamablehttp")
                .header("Origin", "http://localhost:6274")
                .header("Access-Control-Request-Headers", "xrequest, authorization")
                .build());

        StepVerifier.create(provider.handleUnifiedEndpoint(request))
                .assertNext(response -> {
                    assertEquals(HttpStatus.OK, response.statusCode());
                    assertEquals("POST, OPTIONS",
                            response.headers().getFirst("Access-Control-Allow-Methods"));
                    assertEquals("xrequest",
                            response.headers().getFirst("Access-Control-Allow-Headers"));
                    assertTrue(response.headers().getVary().contains("Origin"));
                    assertTrue(response.headers().getVary().contains("Access-Control-Request-Headers"));
                })
                .verifyComplete();
    }

    @Test
    void testPreflightUsesFallbackHeaders() {
        ShenyuStreamableHttpServerTransportProvider provider =
                new ShenyuStreamableHttpServerTransportProvider(new ObjectMapper(),
                        "/mcp/streamablehttp");
        ServerRequest request = createRequest(MockServerHttpRequest.options("/mcp/streamablehttp")
                .header("Origin", "http://localhost:6274")
                .header("Access-Control-Request-Headers", "xrequest")
                .build());

        ServerResponse response = provider.handleUnifiedEndpoint(request).block();
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.statusCode());
        String allowHeaders = response.headers().getFirst("Access-Control-Allow-Headers");
        assertTrue(allowHeaders.toLowerCase(Locale.ROOT).contains("xrequest"));
    }

    private ServerRequest createRequest(final MockServerHttpRequest request) {
        return ServerRequest.create(
                MockServerWebExchange.from(request),
                HandlerStrategies.withDefaults().messageReaders()
        );
    }
}
