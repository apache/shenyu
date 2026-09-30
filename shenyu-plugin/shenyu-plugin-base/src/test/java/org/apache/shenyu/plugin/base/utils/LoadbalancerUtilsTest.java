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

package org.apache.shenyu.plugin.base.utils;

import org.apache.shenyu.common.enums.LoadBalanceEnum;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.web.server.ServerWebExchange;

import java.net.URI;
import java.util.Collections;
import java.util.HashMap;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test cases for LoadbalancerUtils.
 */
public final class LoadbalancerUtilsTest {

    @Test
    public void testMissingRemoteAddressAndMethodUseDefaults() {
        ServerHttpRequest request = mock(ServerHttpRequest.class);
        when(request.getURI()).thenReturn(URI.create("http://localhost/test"));
        when(request.getHeaders()).thenReturn(HttpHeaders.EMPTY);
        when(request.getCookies()).thenReturn(new LinkedMultiValueMap<>());
        when(request.getQueryParams()).thenReturn(new LinkedMultiValueMap<>());
        ServerWebExchange exchange = mock(ServerWebExchange.class);
        when(exchange.getRequest()).thenReturn(request);
        when(exchange.getAttributes()).thenReturn(new HashMap<>());
        Upstream upstream = Upstream.builder().url("http://localhost:8080").build();

        Upstream selected = LoadbalancerUtils.getForExchange(Collections.singletonList(upstream),
                LoadBalanceEnum.RANDOM.getName(), exchange);

        assertSame(upstream, selected);
    }
}
