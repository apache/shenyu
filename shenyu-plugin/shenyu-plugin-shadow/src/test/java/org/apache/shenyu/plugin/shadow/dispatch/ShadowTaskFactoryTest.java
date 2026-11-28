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

package org.apache.shenyu.plugin.shadow.dispatch;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.convert.rule.ShadowHandle;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.utils.RequestUrlUtils;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.net.URI;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * test case for {@link ShadowTaskFactory}.
 */
public final class ShadowTaskFactoryTest {

    @Test
    public void testPreservesRawPathAndQuery() {
        ShadowTask task = ShadowTaskFactory.create(exchange(), handle(), new byte[] {1, 2}).orElseThrow();
        assertEquals("GET", task.getMethod());
        assertEquals("http://shadow:8080/base/api/orders/42?status=open&id=1%2F2", task.getUri().toString());
    }

    @Test
    public void testForwardsOnlyConfiguredHeaders() {
        ShadowTask task = ShadowTaskFactory.create(exchange(), handle(), new byte[0]).orElseThrow();
        assertEquals(List.of("application/json"), task.getHeaders().get(HttpHeaders.ACCEPT));
        assertEquals(List.of("application/json"), task.getHeaders().get(HttpHeaders.CONTENT_TYPE));
        assertNull(task.getHeaders().get(HttpHeaders.AUTHORIZATION));
        assertNull(task.getHeaders().get(HttpHeaders.COOKIE));
        assertEquals("true", task.getHeaders().getFirst(ShadowTaskFactory.SHADOW_HEADER));
    }

    @Test
    public void testAddsConfiguredExtraHeaders() {
        ShadowHandle handle = handle();
        handle.setExtraHeaders(Map.of("X-Shadow-Token", "s3cret"));
        ShadowTask task = ShadowTaskFactory.create(exchange(), handle, new byte[0]).orElseThrow();
        assertEquals("s3cret", task.getHeaders().getFirst("X-Shadow-Token"));
    }

    @Test
    public void testKeepsAttemptBudget() {
        ShadowTask task = ShadowTaskFactory.create(exchange(), handle(), new byte[0]).orElseThrow();
        assertEquals(1000, task.getTimeoutMillis());
    }

    private ServerWebExchange exchange() {
        MockServerHttpRequest request = MockServerHttpRequest
                .method(HttpMethod.GET, URI.create("http://localhost:9195/api/orders/42?status=open&id=1%2F2"))
                .header(HttpHeaders.ACCEPT, "application/json")
                .header(HttpHeaders.CONTENT_TYPE, "application/json")
                .header(HttpHeaders.AUTHORIZATION, "Bearer production-secret")
                .header(HttpHeaders.COOKIE, "session=production")
                .build();
        return MockServerWebExchange.from(request);
    }

    private ShadowHandle handle() {
        ShadowHandle handle = new ShadowHandle();
        handle.setEnabled(true);
        handle.setTarget("http://shadow:8080/base");
        return handle;
    }

    @Test
    public void testFallsBackToTheRewrittenPathWhenNoPrimaryUriIsResolved() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest
                .get("http://localhost:9195/original/orders/42").build());
        ShenyuContext context = new ShenyuContext();
        context.setRealUrl("/original/orders/42");
        exchange.getAttributes().put(Constants.CONTEXT, context);
        exchange.getAttributes().put(Constants.REWRITE_CONTEXT_PATH, "/rewritten");
        exchange.getAttributes().put(Constants.REWRITE_URI, "/orders/42");
        ShadowTask task = ShadowTaskFactory.create(exchange, handle(), new byte[0]).orElseThrow();
        assertEquals("http://shadow:8080/base/rewritten/orders/42", task.getUri().toString());
    }

    @Test
    public void testUsesThePrimaryUriForARewriteOnlyRule() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest
                .get("http://localhost:9195/public/orders?a=1").build());
        ShenyuContext context = new ShenyuContext();
        context.setRealUrl("/public/orders");
        exchange.getAttributes().put(Constants.CONTEXT, context);
        exchange.getAttributes().put(Constants.REWRITE_URI, "/internal/orders");
        exchange.getAttributes().put(Constants.HTTP_URI,
                RequestUrlUtils.buildRequestUri(exchange, "http://upstream:8080"));
        ShadowTask task = ShadowTaskFactory.create(exchange, handle(), new byte[0]).orElseThrow();
        assertEquals("http://shadow:8080/base/internal/orders?a=1", task.getUri().toString());
    }
}
