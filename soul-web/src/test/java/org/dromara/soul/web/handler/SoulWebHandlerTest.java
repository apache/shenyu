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

package org.dromara.soul.web.handler;

import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.plugin.api.SoulPlugin;
import org.dromara.soul.plugin.api.context.SoulContext;
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
 * test for SoulWebHandler.
 *
 * @author FocusZhouGD
 */
public class SoulWebHandlerTest {

    private SoulWebHandler soulWebHandler;

    private SoulPlugin plugins;

    private List<SoulPlugin> listPlugins = new ArrayList<>();

    @Before
    public void setUp() {
        plugins = mock(SoulPlugin.class);
        listPlugins.add(plugins);
        soulWebHandler = new SoulWebHandler(listPlugins);
    }

    @Test
    public void handle() {
        final ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost")
                .remoteAddress(new InetSocketAddress(8090))
                .build());
        exchange.getAttributes().put(Constants.CONTEXT, mock(SoulContext.class));
        exchange.getAttributes().put(Constants.DUBBO_PARAMS, "{key:value}");
        Mono<Void> handle = soulWebHandler.handle(exchange);
        Assert.assertNotNull(handle);
    }
}
