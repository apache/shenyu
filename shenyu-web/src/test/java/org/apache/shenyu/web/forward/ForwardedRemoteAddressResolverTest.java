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
        final ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("localhost")
                .build());
        final ServerWebExchange emptyForwardExchange = MockServerWebExchange.from(MockServerHttpRequest.post("localhost")
                .header("X-Forwarded-For", "")
                .build());
        final ServerWebExchange forwardExchange = MockServerWebExchange.from(MockServerHttpRequest.post("localhost")
                .header("X-Forwarded-For", "127.0.0.1")
                .build());
        final ServerWebExchange multiForwardExchangeError = MockServerWebExchange.from(MockServerHttpRequest.post("localhost")
                .header("X-Forwarded-For", "127.0.0.1", "127.0.0.2")
                .build());
        final ServerWebExchange multiForwardExchange = MockServerWebExchange.from(MockServerHttpRequest.post("localhost")
                .header("X-Forwarded-For", "127.0.0.1, 127.0.0.2")
                .build());

        instance.resolve(exchange);
        instance.resolve(emptyForwardExchange);
        instance.resolve(forwardExchange);
        instance.resolve(multiForwardExchangeError);
        instance.resolve(multiForwardExchange);

        ServerWebExchange headerEmptyExchange = mock(ServerWebExchange.class);
        ServerHttpRequest headerEmptyServerHttpRequest = mock(ServerHttpRequest.class);
        HttpHeaders headerEmptyHttpHeaders = mock(HttpHeaders.class);
        when(headerEmptyExchange.getRequest()).thenReturn(headerEmptyServerHttpRequest);
        when(headerEmptyServerHttpRequest.getHeaders()).thenReturn(headerEmptyHttpHeaders);
        when(headerEmptyHttpHeaders.get(X_FORWARDED_FOR)).thenReturn(Collections.emptyList());
        instance.resolve(headerEmptyExchange);
    }

}
