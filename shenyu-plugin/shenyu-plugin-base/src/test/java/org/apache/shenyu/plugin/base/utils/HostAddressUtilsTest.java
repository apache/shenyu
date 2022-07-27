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

import org.apache.shenyu.plugin.api.RemoteAddressResolver;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.net.InetSocketAddress;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test cases for HostAddressUtils.
 */
@ExtendWith(MockitoExtension.class)
public final class HostAddressUtilsTest {

    private ServerWebExchange exchange;

    @BeforeEach
    public void setUp() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        RemoteAddressResolver remoteAddressResolver = new RemoteAddressResolver() {
        };
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost")
                .remoteAddress(new InetSocketAddress(8090))
                .build());
        when(context.getBean(RemoteAddressResolver.class)).thenReturn(remoteAddressResolver);
    }

    /**
     * The acquire host tests.
     */
    @Test
    public void acquireHostTest() {
        assertEquals("0.0.0.0", HostAddressUtils.acquireHost(exchange));
    }

    /**
     * The acquire ip tests.
     */
    @Test
    public void acquireIpTest() {
        assertEquals("0.0.0.0", HostAddressUtils.acquireIp(exchange));
    }
}
