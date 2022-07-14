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

package org.apache.shenyu.plugin.base.condition.strategy;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.plugin.api.RemoteAddressResolver;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.net.InetSocketAddress;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test cases for AbstractMatchStrategy.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class AbstractMatchStrategyTest {

    private ConditionData conditionData;

    private ServerWebExchange exchange;

    private AbstractMatchStrategy abstractMatchStrategy;

    @BeforeEach
    public void setUp() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        when(context.getBean(RemoteAddressResolver.class)).thenReturn(new RemoteAddressResolver() {
        });
        conditionData = new ConditionData();
        conditionData.setParamName("shenyu");
        conditionData.setParamType("uri");
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/http")
                .remoteAddress(new InetSocketAddress("localhost", 8080))
                .header("shenyu", "shenyuHeader")
                .queryParam("shenyu", "shenyuQueryParam")
                .build());
        ShenyuContext shenyuContext = new ShenyuContext();
        shenyuContext.setMethod("testMethod");
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
        abstractMatchStrategy = new TestMatchStrategy();
    }

    @Test
    public void testBuildRealDataHeaderBranch() {
        conditionData.setParamType(ParamTypeEnum.HEADER.getName());
        assertEquals("shenyuHeader", abstractMatchStrategy.buildRealData(conditionData, exchange));
    }

    @Test
    public void testBuildRealDataUriBranch() {
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        assertEquals("/http", abstractMatchStrategy.buildRealData(conditionData, exchange));
    }

    @Test
    public void testBuildRealDataQueryBranch() {
        conditionData.setParamType(ParamTypeEnum.QUERY.getName());
        assertEquals("shenyuQueryParam", abstractMatchStrategy.buildRealData(conditionData, exchange));
    }

    @Test
    public void testBuildRealDataHostBranch() {
        conditionData.setParamType(ParamTypeEnum.HOST.getName());
        assertEquals("localhost", abstractMatchStrategy.buildRealData(conditionData, exchange));
    }

    @Test
    public void testBuildRealDataIpBranch() {
        conditionData.setParamType(ParamTypeEnum.IP.getName());
        assertEquals("127.0.0.1", abstractMatchStrategy.buildRealData(conditionData, exchange));
    }

    @Test
    public void testBuildRealDataPostBranch() {
        conditionData.setParamType(ParamTypeEnum.POST.getName());
        assertNull(abstractMatchStrategy.buildRealData(conditionData, exchange));
        conditionData.setParamName("method");
        assertEquals("testMethod", abstractMatchStrategy.buildRealData(conditionData, exchange));
    }

    private static class TestMatchStrategy extends AbstractMatchStrategy {

    }
}
