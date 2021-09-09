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

package org.apache.shenyu.web.handler;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.List;

import static org.mockito.Mockito.mock;

/**
 * test for ShenyuWebHandler.
 *
 */
public class ShenyuWebHandlerTest {

    private ShenyuWebHandler shenyuWebHandler;
    
    private List<ShenyuPlugin> listPlugins = new ArrayList<>();

    @Before
    public void setUp() {
        final ShenyuPlugin plugins = mock(ShenyuPlugin.class);
        listPlugins.add(plugins);
        shenyuWebHandler = new ShenyuWebHandler(listPlugins, new ShenyuConfig());
    }

    @Test
    public void handle() {
        final ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost")
                .remoteAddress(new InetSocketAddress(8090))
                .build());
        exchange.getAttributes().put(Constants.CONTEXT, mock(ShenyuContext.class));
        exchange.getAttributes().put(Constants.PARAM_TRANSFORM, "{key:value}");
        Mono<Void> handle = shenyuWebHandler.handle(exchange);
        Assert.assertNotNull(handle);
    }
}
