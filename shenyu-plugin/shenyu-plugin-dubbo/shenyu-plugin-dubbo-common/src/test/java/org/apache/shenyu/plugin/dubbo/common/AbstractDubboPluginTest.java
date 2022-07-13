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

package org.apache.shenyu.plugin.dubbo.common;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.net.InetSocketAddress;
import java.util.Map;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * AbstractDubboPluginDataHandler test.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class AbstractDubboPluginTest {

    private AbstractDubboPlugin plugin;

    @Mock
    private ShenyuPluginChain chain;

    private ServerWebExchange exchange;

    private RuleData ruleData;

    @BeforeEach
    public void setUp() {
        this.ruleData = new RuleData();
        this.ruleData.setSelectorId("test");
        this.ruleData.setName("test-dubbo-plugin");
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        plugin = new AbstractDubboPlugin() {

            @Override
            protected Mono<Void> doDubboInvoker(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                                final SelectorData selector, final RuleData rule, final MetaData metaData, final String param) {
                return Mono.empty();
            }

            @Override
            protected void transmitRpcContext(final Map<String, String> rpcContext) {

            }
        };

        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost")
                .remoteAddress(new InetSocketAddress(8090))
                .build());
    }

    @Test
    public void abstractDubboPluginTest() {
        assertThat(plugin.getOrder(), is(PluginEnum.DUBBO.getCode()));
        assertThat(plugin.named(), is(PluginEnum.DUBBO.getName()));

        ShenyuContext shenyuContext = new ShenyuContext();
        shenyuContext.setRpcType(RpcTypeEnum.DUBBO.getName());
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
        plugin.skip(exchange);
        SelectorData selectorData = mock(SelectorData.class);
        plugin.doExecute(exchange, chain, selectorData, ruleData);

        MetaData metaData = new MetaData();
        metaData.setMethodName("test");
        metaData.setServiceName("dubboService");
        exchange.getAttributes().put(Constants.META_DATA, metaData);
        plugin.doExecute(exchange, chain, selectorData, ruleData);

        metaData.setParameterTypes("parameterTypes");
        plugin.doExecute(exchange, chain, selectorData, ruleData);

        plugin.handleSelectorIfNull(PluginEnum.DUBBO.getName(), exchange, chain);
        plugin.handleRuleIfNull(PluginEnum.DUBBO.getName(), exchange, chain);
    }
}
