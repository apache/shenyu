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

package org.apache.shenyu.plugin.shadow.config;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.convert.rule.ShadowHandle;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.junit.jupiter.api.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * test case for {@link ShadowEligibility}.
 */
public final class ShadowEligibilityTest {

    @Test
    public void testRejectsNonHttpRequest() {
        ShadowHandle handle = new ShadowHandle();
        assertEquals(Optional.of(ShadowSkipReason.NOT_HTTP),
                ShadowEligibility.check(exchange(MockServerHttpRequest.get("/x").build(), false), handle));
    }

    @Test
    public void testRejectsMethodOutsideAllowlist() {
        ShadowHandle handle = new ShadowHandle();
        assertEquals(Optional.of(ShadowSkipReason.METHOD_NOT_ALLOWED),
                ShadowEligibility.check(exchange(MockServerHttpRequest.post("/x").build(), true), handle));
    }

    @Test
    public void testRejectsWebSocketUpgrade() {
        ShadowHandle handle = new ShadowHandle();
        MockServerHttpRequest request = MockServerHttpRequest.get("/x").header("Upgrade", "websocket").build();
        assertEquals(Optional.of(ShadowSkipReason.STREAMING_REQUEST),
                ShadowEligibility.check(exchange(request, true), handle));
    }

    @Test
    public void testRejectsEventStream() {
        ShadowHandle handle = new ShadowHandle();
        MockServerHttpRequest request = MockServerHttpRequest.get("/x").header("Accept", "text/event-stream").build();
        assertEquals(Optional.of(ShadowSkipReason.STREAMING_REQUEST),
                ShadowEligibility.check(exchange(request, true), handle));
    }

    @Test
    public void testAcceptsEligibleRequest() {
        ShadowHandle handle = new ShadowHandle();
        assertTrue(ShadowEligibility.check(exchange(MockServerHttpRequest.get("/x").build(), true), handle).isEmpty());
    }

    private ServerWebExchange exchange(final MockServerHttpRequest request, final boolean http) {
        MockServerWebExchange exchange = MockServerWebExchange.from(request);
        if (http) {
            ShenyuContext context = new ShenyuContext();
            context.setRpcType(RpcTypeEnum.HTTP.getName());
            exchange.getAttributes().put(Constants.CONTEXT, context);
        }
        return exchange;
    }
}
