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

package org.apache.shenyu.plugin.general.context;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.GeneralContextHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.general.context.handler.GeneralContextPluginDataHandler;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.when;

/**
 * Request plugin test.
 */
@ExtendWith(MockitoExtension.class)
public class GeneralContextPluginTest {
    @Mock
    private ShenyuPluginChain chain;

    private ServerWebExchange exchange;

    private GeneralContextPlugin generalContextPlugin;

    private RuleData ruleData;

    @BeforeEach
    public void setup() {
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest
                .get("localhost")
                .header("shenyuTestHeaderKey", "shenyuTestHeaderValue")
                .build());
        this.generalContextPlugin = new GeneralContextPlugin();
        this.ruleData = new RuleData();
        this.ruleData.setSelectorId("test-selectorId");
        this.ruleData.setName("test-general-context-plugin");
        Map<String, List<GeneralContextHandle>> generalContextHandleMap = new HashMap<>();
        List<GeneralContextHandle> contextHandles = new ArrayList<>();
        GeneralContextHandle addGeneralContextHandle = new GeneralContextHandle(Constants.ADD_GENERAL_CONTEXT_TYPE, "addGeneralContextKey", "addGeneralContextValue");
        contextHandles.add(addGeneralContextHandle);

        GeneralContextHandle transmitGeneralContextHandle = new GeneralContextHandle(Constants.TRANSMIT_HEADER_TO_GENERAL_CONTEXT_TYPE, "shenyuTestHeaderKey", "shenyuTestHeaderNewKey");
        contextHandles.add(transmitGeneralContextHandle);

        generalContextHandleMap.put(PluginEnum.DUBBO.getName(), contextHandles);
        GeneralContextPluginDataHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(this.ruleData), generalContextHandleMap);
    }

    @Test
    public void testDoExecute() {
        SelectorData selectorData = mock(SelectorData.class);
        when(this.chain.execute(any())).thenReturn(Mono.empty());

        StepVerifier.create(generalContextPlugin.doExecute(this.exchange, this.chain, selectorData, this.ruleData)).expectSubscription().verifyComplete();

        ArgumentCaptor<ServerWebExchange> newExchange = ArgumentCaptor.forClass(ServerWebExchange.class);
        Mockito.verify(this.chain, times(1)).execute(newExchange.capture());

        Map<String, String> shenyuGeneralContext = ((Map<String, Map<String, String>>) newExchange.getValue().getAttributes().get(Constants.GENERAL_CONTEXT)).get(PluginEnum.DUBBO.getName());

        assertTrue(shenyuGeneralContext.containsKey("addGeneralContextKey"));
        assertTrue(shenyuGeneralContext.containsKey("shenyuTestHeaderNewKey"));

        assertEquals(shenyuGeneralContext.get("addGeneralContextKey"), "addGeneralContextValue");
        assertEquals(shenyuGeneralContext.get("shenyuTestHeaderNewKey"), "shenyuTestHeaderValue");
        shenyuGeneralContext = ((Map<String, Map<String, String>>) newExchange.getValue().getAttributes().get(Constants.GENERAL_CONTEXT)).get(PluginEnum.SOFA.getName());
        assertNull(shenyuGeneralContext);
    }

    @Test
    public void testGetOrder() {
        assertEquals(this.generalContextPlugin.getOrder(), PluginEnum.GENERAL_CONTEXT.getCode());
    }

    @Test
    public void tesNamed() {
        assertEquals(this.generalContextPlugin.named(), PluginEnum.GENERAL_CONTEXT.getName());
    }
}
