/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License,  Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,  software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,  either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.plugin.base.utils;

import org.apache.shenyu.plugin.api.RemoteAddressResolver;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.net.InetSocketAddress;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test cases for HostAddressUtils.
 */
@RunWith(MockitoJUnitRunner.class)
public final class HostAddressUtilsTest {

    private ServerWebExchange exchange;

    private RemoteAddressResolver remoteAddressResolver;

    @Before
    public void setUp() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        this.remoteAddressResolver = new RemoteAddressResolver() {
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
        Assert.assertEquals(HostAddressUtils.acquireHost(exchange), "0.0.0.0");
    }

    /**
     * The acquire ip tests.
     */
    @Test
    public void acquireIpTest() {
        Assert.assertEquals(HostAddressUtils.acquireIp(exchange), "0.0.0.0");
    }
}
