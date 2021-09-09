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

package org.apache.shenyu.plugin.request.id;

import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.request.id.DefaultRequestIdGenerator;
import org.apache.shenyu.plugin.api.request.id.RequestIdGenerator;
import org.apache.shenyu.plugin.api.request.id.ShenyuRequestIdWrap;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.net.InetSocketAddress;

import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

/**
 * The Test Case For RequestId plugin.
 */
@RunWith(MockitoJUnitRunner.class)
public class RequestIdPluginTest {

    private RequestIdPlugin requestIdPlugin;

    private ServerWebExchange exchange;

    private ShenyuPluginChain chain;

    @Before
    public void setUp() {
        this.requestIdPlugin = new RequestIdPlugin();
        this.chain = mock(ShenyuPluginChain.class);
        MockServerHttpRequest request = MockServerHttpRequest
                .get("localhost")
                .remoteAddress(new InetSocketAddress(8090))
                .header("X-source", "mock test")
                .queryParam("queryParam", "Hello,World")
                .build();
        this.exchange = spy(MockServerWebExchange.from(request));

        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);

        when(context.getBean(RequestIdGenerator.class)).thenReturn(new DefaultRequestIdGenerator());
    }

    @Test
    public void testDoExecute() {
        this.requestIdPlugin.execute(this.exchange, this.chain);
        assertNotNull(ShenyuRequestIdWrap.getRequestId());
    }
}
