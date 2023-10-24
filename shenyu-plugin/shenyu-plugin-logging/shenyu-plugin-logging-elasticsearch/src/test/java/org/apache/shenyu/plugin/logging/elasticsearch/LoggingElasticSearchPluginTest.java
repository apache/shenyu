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

package org.apache.shenyu.plugin.logging.elasticsearch;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.RemoteAddressResolver;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentMatchers;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.InetSocketAddress;

/**
 * The Test Case For LoggingElasticSearchPlugin.
 */
@ExtendWith(MockitoExtension.class)
public final class LoggingElasticSearchPluginTest {

    private LoggingElasticSearchPlugin loggingElasticSearchPlugin;

    private ServerWebExchange exchange;

    private RuleData ruleData;

    private ShenyuPluginChain chain;

    private SelectorData selectorData;

    private ServerHttpRequest request;

    private ShenyuRequestLog requestLog;

    @BeforeEach
    public void setUp() {
        this.loggingElasticSearchPlugin = new LoggingElasticSearchPlugin();
        this.ruleData = Mockito.mock(RuleData.class);
        this.chain = Mockito.mock(ShenyuPluginChain.class);
        this.selectorData = Mockito.mock(SelectorData.class);
        this.request = Mockito.mock(ServerHttpRequest.class);
        this.requestLog = new ShenyuRequestLog();
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
        this.exchange = Mockito.spy(MockServerWebExchange.from(request));
        ShenyuContext shenyuContext = Mockito.mock(ShenyuContext.class);
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
    }

    @Test
    public void testDoExecute() {
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(Mono.empty());
        Mono<Void> result = loggingElasticSearchPlugin.doExecute(exchange, chain, selectorData, ruleData);
        StepVerifier.create(result).expectSubscription().verifyComplete();
    }

    @Test
    public void testGetOrder() {
        Assertions.assertEquals(loggingElasticSearchPlugin.getOrder(), PluginEnum.LOGGING_ELASTIC_SEARCH.getCode());
    }

    @Test
    public void testNamed() {
        Assertions.assertEquals(loggingElasticSearchPlugin.named(), PluginEnum.LOGGING_ELASTIC_SEARCH.getName());
    }
}
