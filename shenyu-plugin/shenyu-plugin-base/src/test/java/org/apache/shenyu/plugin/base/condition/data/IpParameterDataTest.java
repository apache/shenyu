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

package org.apache.shenyu.plugin.base.condition.data;

import org.apache.shenyu.common.utils.UUIDUtils;
import org.apache.shenyu.plugin.api.RemoteAddressResolver;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.net.InetSocketAddress;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link IpParameterDataTest}.
 */
public class IpParameterDataTest {

    private ServerWebExchange exchange;

    private IpParameterData ipParameterData;

    private RemoteAddressResolver remoteAddressResolver;

    private final String testHost = "127.0.0.1";

    @Before
    public void setUp() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        this.remoteAddressResolver = new RemoteAddressResolver() {
        };

        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/http")
                .remoteAddress(new InetSocketAddress("localhost", 8080))
                .build());
        this.ipParameterData = new IpParameterData();

        when(context.getBean(RemoteAddressResolver.class)).thenReturn(remoteAddressResolver);
    }

    @Test
    public void testBuilderWithNullParamName() {
        Assert.assertEquals(testHost, ipParameterData.builder(null, exchange));
    }

    @Test
    public void testBuilderWithAnyParamName() {
        Assert.assertEquals(testHost, ipParameterData.builder(UUIDUtils.getInstance().generateShortUuid(), exchange));
    }
}
