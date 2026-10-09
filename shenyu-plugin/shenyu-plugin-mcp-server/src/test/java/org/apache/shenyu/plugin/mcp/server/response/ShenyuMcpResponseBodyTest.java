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

package org.apache.shenyu.plugin.mcp.server.response;

import org.junit.jupiter.api.Test;
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.mock.http.server.reactive.MockServerHttpResponse;
import reactor.core.publisher.Mono;

import java.nio.charset.StandardCharsets;
import java.util.concurrent.CompletableFuture;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test cases for preserving the MCP response body.
 */
public final class ShenyuMcpResponseBodyTest {

    @Test
    public void testCapturedBodyIsStillForwarded() {
        MockServerHttpResponse response = new MockServerHttpResponse();
        ShenyuMcpResponseDecorator decorator = new ShenyuMcpResponseDecorator(response, "session", new CompletableFuture<>(), null);

        decorator.writeWith(Mono.just(DefaultDataBufferFactory.sharedInstance.wrap("payload".getBytes(StandardCharsets.UTF_8)))).block();

        assertEquals("payload", response.getBodyAsString().block());
    }
}
