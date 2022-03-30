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

package org.apache.shenyu.plugin.logging;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.RemoteAddressResolver;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.logging.config.LogCollectConfig;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentMatchers;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.InetSocketAddress;
import java.util.Properties;

/**
 * The Test Case For LoggingRocketMQPlugin.
 */
@ExtendWith(MockitoExtension.class)
public final class LoggingRocketMQPluginTest {

    private static LoggingRocketMQPlugin loggingRocketMQPlugin;

    private static ServerWebExchange exchange;

    private static RuleData ruleData;

    private static ShenyuPluginChain chain;

    private static SelectorData selectorData;

    private static Properties rocketmqProps = new Properties();

    private static LogCollectConfig logCollectConfig = new LogCollectConfig();

    @BeforeAll
    public static void setUp() {
        rocketmqProps.setProperty("topic", "shenyu-access-logging");
        rocketmqProps.setProperty("namesrvAddr", "localhost:9876");
        logCollectConfig.setRocketmqProps(rocketmqProps);
        loggingRocketMQPlugin = new LoggingRocketMQPlugin(logCollectConfig);
        ruleData = Mockito.mock(RuleData.class);
        chain = Mockito.mock(ShenyuPluginChain.class);
        selectorData = Mockito.mock(SelectorData.class);
        MockServerHttpRequest request = MockServerHttpRequest
                .get("localhost")
                .remoteAddress(new InetSocketAddress(8090))
                .header("X-source", "mock test")
                .queryParam("queryParam", "Hello,World")
                .build();
        ConfigurableApplicationContext context = Mockito.mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        RemoteAddressResolver remoteAddressResolver = new RemoteAddressResolver() {
        };
        Mockito.lenient().when(context.getBean(RemoteAddressResolver.class)).thenReturn(remoteAddressResolver);
        exchange = Mockito.spy(MockServerWebExchange.from(request));
        ShenyuContext shenyuContext = Mockito.mock(ShenyuContext.class);
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
    }

    @Test
    public void testDoExecute() {
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(Mono.empty());
        Mono<Void> result = loggingRocketMQPlugin.doExecute(exchange, chain, selectorData, ruleData);
        StepVerifier.create(result).expectSubscription().verifyComplete();
    }

    @Test
    public void testGetOrder() {
        Assertions.assertEquals(loggingRocketMQPlugin.getOrder(), PluginEnum.LOGGING_ROCKETMQ.getCode());
    }

    @Test
    public void testNamed() {
        Assertions.assertEquals(loggingRocketMQPlugin.named(), PluginEnum.LOGGING_ROCKETMQ.getName());
    }
}
