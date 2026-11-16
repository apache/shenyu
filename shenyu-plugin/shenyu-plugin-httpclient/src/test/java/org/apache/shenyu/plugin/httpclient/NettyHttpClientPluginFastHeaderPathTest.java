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

package org.apache.shenyu.plugin.httpclient;

import io.netty.buffer.PooledByteBufAllocator;
import io.netty.handler.codec.http.DefaultHttpHeaders;
import io.netty.handler.codec.http.HttpHeaders;
import org.apache.shenyu.common.enums.UniqueHeaderEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.core.io.buffer.DataBufferFactory;
import org.springframework.core.io.buffer.NettyDataBufferFactory;
import org.springframework.http.server.reactive.AbstractServerHttpResponse;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.mock.http.server.reactive.MockServerHttpResponse;
import org.springframework.web.server.ServerWebExchange;
import reactor.netty.http.server.HttpServerResponse;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The native header-copy fast path of {@link NettyHttpClientPlugin} (issue
 * #8435): the four guards and the equivalence of writing the Netty headers
 * directly with the putAll semantics of the intermediate-map path.
 */
public class NettyHttpClientPluginFastHeaderPathTest {

    private static final String SHARED_NAME = "x-upstream-shared";

    private static final String ONLY_NAME = "x-upstream-only";

    private static final String NETTY_ONLY_NAME = "x-netty-only";

    private NettyHttpClientPlugin plugin;

    private ServerWebExchange exchange;

    @BeforeEach
    public void setUp() {
        plugin = new NettyHttpClientPlugin(reactor.netty.http.client.HttpClient.create(), org.apache.shenyu.common.constant.Constants.BYTES_PER_MB);
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/fast-path").build());
    }

    @Test
    public void fastPathReplacesSharedNamesAddsUpstreamOnlyAndKeepsNettyOnly() throws Exception {
        final HttpHeaders nettyHeaders = new DefaultHttpHeaders();
        nettyHeaders.add(SHARED_NAME, "old");
        nettyHeaders.add(NETTY_ONLY_NAME, "kept");
        final HttpHeaders upstream = new DefaultHttpHeaders();
        upstream.add(SHARED_NAME, "new");
        upstream.add(ONLY_NAME, "added");
        final AbstractServerHttpResponse response = reactorResponse(mockNettyResponse(nettyHeaders));

        final boolean taken = plugin.copyUpstreamHeadersFast(exchange, response, mockUpstream(upstream));

        assertTrue(taken);
        // the shared name is replaced, not appended
        assertEquals("new", nettyHeaders.get(SHARED_NAME));
        assertEquals(1, nettyHeaders.getAll(SHARED_NAME).size());
        assertEquals("added", nettyHeaders.get(ONLY_NAME));
        assertEquals("kept", nettyHeaders.get(NETTY_ONLY_NAME));
        // Spring's live view sees exactly the same values — the core equivalence
        assertEquals("new", response.getHeaders().getFirst(SHARED_NAME));
        assertEquals("added", response.getHeaders().getFirst(ONLY_NAME));
    }

    @Test
    public void fastPathKeepsEveryValueOfMultiValuedUpstreamHeaders() throws Exception {
        final HttpHeaders nettyHeaders = new DefaultHttpHeaders();
        nettyHeaders.add("x-shared", "stale");
        final HttpHeaders upstream = new DefaultHttpHeaders();
        upstream.add("set-cookie", "a=1");
        upstream.add("set-cookie", "b=2");
        upstream.add("x-shared", "fresh");
        final AbstractServerHttpResponse response = reactorResponse(mockNettyResponse(nettyHeaders));

        final boolean taken = plugin.copyUpstreamHeadersFast(exchange, response, mockUpstream(upstream));

        assertTrue(taken);
        // putAll semantics: both upstream values survive, the stale one is gone
        assertEquals(2, nettyHeaders.getAll("set-cookie").size());
        assertTrue(nettyHeaders.getAll("set-cookie").contains("a=1"));
        assertTrue(nettyHeaders.getAll("set-cookie").contains("b=2"));
        assertEquals("fresh", nettyHeaders.get("x-shared"));
        assertEquals(2, response.getHeaders().get("set-cookie").size());
    }

    @Test
    public void mockResponseFallsBackToTheSlowPath() {
        final ServerHttpResponse response = new MockServerHttpResponse();
        final boolean taken = plugin.copyUpstreamHeadersFast(exchange, response, mockUpstream(new DefaultHttpHeaders()));
        assertFalse(taken);
    }

    @Test
    public void configuredUniqueHeadersFallBackToTheSlowPath() throws Exception {
        final AbstractServerHttpResponse response = reactorResponse(mockNettyResponse(new DefaultHttpHeaders()));
        exchange.getAttributes().put(UniqueHeaderEnum.RESP_UNIQUE_HEADER.getName(), SHARED_NAME);
        final boolean taken = plugin.copyUpstreamHeadersFast(exchange, response, mockUpstream(new DefaultHttpHeaders()));
        assertFalse(taken);
    }

    @Test
    public void subclassedPluginFallsBackToTheSlowPath() throws Exception {
        final AbstractServerHttpResponse response = reactorResponse(mockNettyResponse(new DefaultHttpHeaders()));
        final NettyHttpClientPlugin subclass = new NettyHttpClientPlugin(reactor.netty.http.client.HttpClient.create(), org.apache.shenyu.common.constant.Constants.BYTES_PER_MB) {
        };
        final boolean taken = subclass.copyUpstreamHeadersFast(exchange, response, mockUpstream(new DefaultHttpHeaders()));
        assertFalse(taken);
    }

    @Test
    public void committedResponseFallsBackToTheSlowPath() throws Exception {
        final AbstractServerHttpResponse response = reactorResponse(mockNettyResponse(new DefaultHttpHeaders()));
        markCommitted(response);
        assertTrue(response.isCommitted());
        final boolean taken = plugin.copyUpstreamHeadersFast(exchange, response, mockUpstream(new DefaultHttpHeaders()));
        assertFalse(taken);
    }

    private HttpServerResponse mockNettyResponse(final HttpHeaders nettyHeaders) {
        final HttpServerResponse nettyResponse = mock(HttpServerResponse.class);
        when(nettyResponse.responseHeaders()).thenReturn(nettyHeaders);
        when(nettyResponse.status()).thenReturn(io.netty.handler.codec.http.HttpResponseStatus.OK);
        return nettyResponse;
    }

    private reactor.netty.http.client.HttpClientResponse mockUpstream(final HttpHeaders upstream) {
        final reactor.netty.http.client.HttpClientResponse response = mock(reactor.netty.http.client.HttpClientResponse.class);
        when(response.responseHeaders()).thenReturn(upstream);
        return response;
    }

    /**
     * Build a real package-private {@code ReactorServerHttpResponse} around a
     * mocked Netty response, so the class-name guard passes exactly as it would
     * inside a running gateway.
     */
    private AbstractServerHttpResponse reactorResponse(final HttpServerResponse nettyResponse) throws Exception {
        final Class<?> type = Class.forName("org.springframework.http.server.reactive.ReactorServerHttpResponse");
        final Constructor<?> constructor = type.getDeclaredConstructor(HttpServerResponse.class, DataBufferFactory.class);
        constructor.setAccessible(true);
        return (AbstractServerHttpResponse) constructor.newInstance(nettyResponse,
                new NettyDataBufferFactory(PooledByteBufAllocator.DEFAULT));
    }


    private void markCommitted(final AbstractServerHttpResponse response) throws Exception {
        final Field state = AbstractServerHttpResponse.class.getDeclaredField("state");
        state.setAccessible(true);
        final AtomicReference<?> reference = (AtomicReference<?>) state.get(response);
        final Class<?> stateType = reference.get().getClass();
        final Method valueOf = stateType.getDeclaredMethod("valueOf", String.class);
        valueOf.setAccessible(true);
        @SuppressWarnings("unchecked")
        final AtomicReference<Object> typed = (AtomicReference<Object>) state.get(response);
        typed.set(valueOf.invoke(null, "COMMITTED"));
    }
}
