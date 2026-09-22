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

package org.apache.shenyu.plugin.metrics;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.RemoteAddressResolver;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.metrics.constant.LabelNames;
import org.apache.shenyu.plugin.metrics.reporter.MetricsReporter;
import org.apache.shenyu.plugin.metrics.spi.MetricsRegister;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentMatchers;
import org.mockito.Mockito;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.InetSocketAddress;

/**
 * The Test Case For MetricsPlugin.
 */
public class MetricsPluginTest {

    private MetricsPlugin metricsPlugin;

    private ServerWebExchange exchange;

    private ShenyuPluginChain chain;

    @BeforeEach
    public void setUp() {
        this.metricsPlugin = new MetricsPlugin();
        this.chain = Mockito.mock(ShenyuPluginChain.class);
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
        Mono<Void> result = metricsPlugin.execute(exchange, chain);
        StepVerifier.create(result).expectSubscription().verifyComplete();
    }

    @Test
    public void testGetOrder() {
        Assertions.assertEquals(metricsPlugin.getOrder(), PluginEnum.METRICS.getCode());
    }

    @Test
    public void testNamed() {
        Assertions.assertEquals(metricsPlugin.named(), PluginEnum.METRICS.getName());
    }

    @Test
    public void testRequestTypeTotalIsNotLabelledByRawPath() {
        MetricsRegister metricsRegister = Mockito.mock(MetricsRegister.class);
        MetricsReporter.register(metricsRegister);
        try {
            Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(Mono.empty());
            String rpcType = RpcTypeEnum.HTTP.getName();
            ShenyuContext shenyuContext = Mockito.mock(ShenyuContext.class);
            Mockito.lenient().when(shenyuContext.getRpcType()).thenReturn(rpcType);
            StepVerifier.create(metricsPlugin.execute(createExchange("/api/user/123", shenyuContext), chain))
                    .expectSubscription().verifyComplete();
            StepVerifier.create(metricsPlugin.execute(createExchange("/api/order/456", shenyuContext), chain))
                    .expectSubscription().verifyComplete();
            // the raw path must not be used as label value, otherwise the prometheus client keeps
            // one child series per distinct path and its children map grows without bound.
            Mockito.verify(metricsRegister, Mockito.times(2))
                    .counterIncrement(LabelNames.REQUEST_TYPE_TOTAL, new String[]{rpcType}, 1L);
        } finally {
            MetricsReporter.clean();
        }
    }

    private ServerWebExchange createExchange(final String path, final ShenyuContext shenyuContext) {
        ServerWebExchange result = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost" + path).build());
        result.getAttributes().put(Constants.CONTEXT, shenyuContext);
        return result;
    }
}
