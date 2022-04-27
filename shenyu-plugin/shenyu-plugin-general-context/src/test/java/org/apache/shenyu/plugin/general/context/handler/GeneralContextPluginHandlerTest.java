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

package org.apache.shenyu.plugin.general.context.handler;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * request plugin handler test.
 */
@ExtendWith(MockitoExtension.class)
public class GeneralContextPluginHandlerTest {

    private GeneralContextPluginDataHandler generalContextPluginDataHandler;

    private RuleData ruleData;

    @BeforeEach
    public void setUp() {
        this.generalContextPluginDataHandler = new GeneralContextPluginDataHandler();
        this.ruleData = new RuleData();
        this.ruleData.setSelectorId("test-selectorId");
        this.ruleData.setName("test-rpc-context-plugin");
        this.ruleData.setHandle("{\"dubbo\":[{\"getContextType\":\"addRpcContext\",\"rpcContextKey\":\"testHeader\",\"rpcContextValue\":\"testHeaderValue\"}"
                + ",{\"rpcContextType\":\"transmitHeaderToRpcContext\",\"rpcContextKey\":\"shenyuHeaderKey\",\"rpcContextValue\":\"shenyuHeaderNewKey\"}]}");
    }

    @Test
    public void testHandlerRule() {
        this.generalContextPluginDataHandler.handlerRule(this.ruleData);
        assertNotNull(GeneralContextPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(this.ruleData)));
    }

    @Test
    public void testRemoveRule() {
        this.generalContextPluginDataHandler.handlerRule(this.ruleData);
        RuleData ruleData = new RuleData();
        ruleData.setSelectorId("test");
        ruleData.setName("test");
        this.generalContextPluginDataHandler.removeRule(this.ruleData);
        assertNull(GeneralContextPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(this.ruleData)));
        this.generalContextPluginDataHandler.removeRule(ruleData);
        assertNull(GeneralContextPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(ruleData)));
        this.generalContextPluginDataHandler.removeRule(null);
    }

    @Test
    public void testPluginNamed() {
        assertEquals(this.generalContextPluginDataHandler.pluginNamed(), PluginEnum.GENERAL_CONTEXT.getName());
    }
}
