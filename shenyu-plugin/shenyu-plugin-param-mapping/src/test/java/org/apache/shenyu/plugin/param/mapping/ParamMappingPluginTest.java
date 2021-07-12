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

package org.apache.shenyu.plugin.param.mapping;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.param.mapping.handler.ParamMappingPluginDataHandler;
import org.apache.shenyu.plugin.param.mapping.strategy.DefaultOperator;
import org.apache.shenyu.plugin.param.mapping.strategy.FormDataOperator;
import org.apache.shenyu.plugin.param.mapping.strategy.JsonOperator;
import org.apache.shenyu.plugin.param.mapping.strategy.Operator;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.HashMap;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link ParamMappingPlugin}.
 */
@RunWith(MockitoJUnitRunner.class)
public class ParamMappingPluginTest {

    private RuleData ruleData;

    @Mock
    private ShenyuPluginChain chain;

    private ServerWebExchange exchange;

    private ParamMappingPlugin paramMappingPlugin;

    private ParamMappingPluginDataHandler paramMappingPluginDataHandler;

    @Before
    public void setUp() {
        this.ruleData = new RuleData();
        this.ruleData.setSelectorId("test");
        this.ruleData.setName("test-param-mappin-plugin");
        this.ruleData.setHandle("{\"removeParameterKeys\":[\"$.age\"]}");
        this.paramMappingPluginDataHandler = new ParamMappingPluginDataHandler();
        Map<String, Operator> operatorMap = new HashMap<>(4);
        operatorMap.put(Constants.DEFAULT, new DefaultOperator());
        operatorMap.put(MediaType.APPLICATION_JSON.toString(), new JsonOperator());
        operatorMap.put(MediaType.APPLICATION_FORM_URLENCODED.toString(), new FormDataOperator());
        this.paramMappingPlugin = new ParamMappingPlugin(operatorMap);
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest
                .method(HttpMethod.POST, "localhost")
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .body("{\"name\":\"shenyu\",\"age\":\"18\"}"));
    }

    @Test
    public void testDoExecute() {
        SelectorData selectorData = mock(SelectorData.class);
        when(this.chain.execute(any())).thenReturn(Mono.empty());
        paramMappingPluginDataHandler.handlerRule(ruleData);
        StepVerifier.create(paramMappingPlugin.doExecute(this.exchange, this.chain, selectorData, this.ruleData)).expectSubscription().verifyComplete();
    }

    @Test
    public void testGetOrder() {
        assertEquals(this.paramMappingPlugin.getOrder(), PluginEnum.PARAM_MAPPING.getCode());
    }

    @Test
    public void tesNamed() {
        assertEquals(this.paramMappingPlugin.named(), PluginEnum.PARAM_MAPPING.getName());
    }
}
