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

package org.apache.shenyu.web.forward;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.server.ServerWebExchange;

import java.net.InetSocketAddress;
import java.util.Collections;

import static org.apache.shenyu.web.forward.ForwardedRemoteAddressResolver.X_FORWARDED_FOR;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test cases for ForwardedRemoteAddressResolver.
 *
 */
@ExtendWith(MockitoExtension.class)
public final class ForwardedRemoteAddressResolverTest {

    @Test
    public void testNewInstance() {
        try {
            ForwardedRemoteAddressResolver.maxTrustedIndex(0);
        } catch (Exception e) {
            assertEquals(e.getMessage(), "An index greater than 0 is required");
        }

        ForwardedRemoteAddressResolver instance = ForwardedRemoteAddressResolver.maxTrustedIndex(5);
        int maxTrustedIndex = (int) ReflectionTestUtils.getField(instance, "maxTrustedIndex");
        assertEquals(maxTrustedIndex, 5);

        ForwardedRemoteAddressResolver all = ForwardedRemoteAddressResolver.trustAll();
        maxTrustedIndex = (int) ReflectionTestUtils.getField(all, "maxTrustedIndex");
        assertEquals(maxTrustedIndex, Integer.MAX_VALUE);
    }

    @Test
    public void testResolver() {
        ForwardedRemoteAddressResolver instance = ForwardedRemoteAddressResolver.maxTrustedIndex(1);
        InetSocketAddress remoteAddress = new InetSocketAddress("192.0.2.10", 8080);
        final ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("localhost")
                .remoteAddress(remoteAddress)
                .build());
        final ServerWebExchange emptyForwardExchange = MockServerWebExchange.from(MockServerHttpRequest.post("localhost")
                .header("X-Forwarded-For", "")
                .remoteAddress(remoteAddress)
                .build());
        final ServerWebExchange forwardExchange = MockServerWebExchange.from(MockServerHttpRequest.post("localhost")
                .header("X-Forwarded-For", "127.0.0.1")
                .remoteAddress(remoteAddress)
                .build());
        final ServerWebExchange multiForwardExchangeError = MockServerWebExchange.from(MockServerHttpRequest.post("localhost")
                .header("X-Forwarded-For", "127.0.0.1", "127.0.0.2")
                .remoteAddress(remoteAddress)
                .build());
        final ServerWebExchange multiForwardExchange = MockServerWebExchange.from(MockServerHttpRequest.post("localhost")
                .header("X-Forwarded-For", "127.0.0.1, 127.0.0.2")
                .remoteAddress(remoteAddress)
                .build());
        final ServerWebExchange ipv6ForwardExchange = MockServerWebExchange.from(MockServerHttpRequest.post("localhost")
                .header("X-Forwarded-For", "2001:db8::1")
                .remoteAddress(remoteAddress)
                .build());
        final ServerWebExchange hostnameForwardExchange = MockServerWebExchange.from(MockServerHttpRequest.post("localhost")
                .header("X-Forwarded-For", "attacker.example")
                .remoteAddress(remoteAddress)
                .build());

        assertEquals(remoteAddress, instance.resolve(exchange));
        assertEquals(remoteAddress, instance.resolve(emptyForwardExchange));
        assertEquals("127.0.0.1", instance.resolve(forwardExchange).getAddress().getHostAddress());
        assertEquals(remoteAddress, instance.resolve(multiForwardExchangeError));
        assertEquals("127.0.0.1", instance.resolve(multiForwardExchange).getAddress().getHostAddress());
        assertEquals("2001:db8:0:0:0:0:0:1", instance.resolve(ipv6ForwardExchange).getAddress().getHostAddress());
        assertEquals(remoteAddress, instance.resolve(hostnameForwardExchange));

        ServerWebExchange headerEmptyExchange = mock(ServerWebExchange.class);
        ServerHttpRequest headerEmptyServerHttpRequest = mock(ServerHttpRequest.class);
        HttpHeaders headerEmptyHttpHeaders = mock(HttpHeaders.class);
        when(headerEmptyExchange.getRequest()).thenReturn(headerEmptyServerHttpRequest);
        when(headerEmptyServerHttpRequest.getHeaders()).thenReturn(headerEmptyHttpHeaders);
        when(headerEmptyHttpHeaders.get(X_FORWARDED_FOR)).thenReturn(Collections.emptyList());
        instance.resolve(headerEmptyExchange);
    }

}
