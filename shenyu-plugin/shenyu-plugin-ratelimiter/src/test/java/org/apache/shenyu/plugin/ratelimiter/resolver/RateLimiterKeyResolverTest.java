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

package org.apache.shenyu.plugin.ratelimiter.resolver;

import org.apache.commons.collections4.ListUtils;
import org.apache.shenyu.plugin.ratelimiter.algorithm.RateLimiterAlgorithm;
import org.apache.shenyu.plugin.ratelimiter.algorithm.TokenBucketRateLimiterAlgorithm;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.UnknownHostException;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(MockitoExtension.class)
public class RateLimiterKeyResolverTest {

    private RateLimiterAlgorithm<?> rateLimiterAlgorithm;

    private ServerWebExchange firstExchange;

    private ServerWebExchange secondExchange;

    @BeforeEach
    public void setUp() throws UnknownHostException {
        rateLimiterAlgorithm = new TokenBucketRateLimiterAlgorithm();
        firstExchange = MockServerWebExchange.from(MockServerHttpRequest
                .get("localhost")
                .remoteAddress(new InetSocketAddress(InetAddress.getByAddress(new byte[]{1, 1, 1, 1}), 8080))
                .build());
        secondExchange = MockServerWebExchange.from(MockServerHttpRequest
                .get("localhost")
                .remoteAddress(new InetSocketAddress(InetAddress.getByAddress(new byte[]{1, 1, 1, 2}), 8080))
                .build());
    }

    @Test
    public void wholeResolveTest() {
        RateLimiterKeyResolver keyResolver = new WholeKeyResolver();
        List<String> firstKeys = rateLimiterAlgorithm.getKeys(keyResolver.resolve(firstExchange));
        List<String> secondKeys = rateLimiterAlgorithm.getKeys(keyResolver.resolve(secondExchange));
        assert ListUtils.isEqualList(firstKeys, secondKeys);
    }

    @Test
    public void remoteAddrResolveTest() {
        RateLimiterKeyResolver keyResolver = new RemoteAddrKeyResolver();
        List<String> firstKeys = rateLimiterAlgorithm.getKeys(keyResolver.resolve(firstExchange));
        List<String> secondKeys = rateLimiterAlgorithm.getKeys(keyResolver.resolve(secondExchange));
        assert !ListUtils.isEqualList(firstKeys, secondKeys);
    }

    @Test
    public void wholeGetKeyResolverNameTest() {
        String keyResolverName = new WholeKeyResolver().getKeyResolverName();
        assertEquals("WHOLE_KEY_RESOLVER", keyResolverName);
    }

    @Test
    public void remoteAddrGetKeyResolverNameTest() {
        String keyResolverName = new RemoteAddrKeyResolver().getKeyResolverName();
        assertEquals("REMOTE_ADDRESS_KEY_RESOLVER", keyResolverName);
    }
}
