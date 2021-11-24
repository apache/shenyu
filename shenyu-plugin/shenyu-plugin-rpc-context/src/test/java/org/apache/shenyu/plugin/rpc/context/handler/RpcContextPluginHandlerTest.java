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

package org.apache.shenyu.plugin.rpc.context.handler;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

import static junit.framework.Assert.assertNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

/**
 * request plugin handler test.
 */
@RunWith(MockitoJUnitRunner.class)
public class RpcContextPluginHandlerTest {

    private RpcContextPluginDataHandler rpcContextPluginDataHandler;

    private RuleData ruleData;

    @Before
    public void setUp() {
        this.rpcContextPluginDataHandler = new RpcContextPluginDataHandler();
        this.ruleData = new RuleData();
        this.ruleData.setSelectorId("test-selectorId");
        this.ruleData.setName("test-rpc-context-plugin");
        this.ruleData.setHandle("{\"addRpcContext\":{\"addRpcContextKey\":\"addRpcContextValue\"},\"transmitHeaderToRpcContext\":{\"transmitHeaderToRpcContextKey\":\"\"}}");
    }

    @Test
    public void testHandlerRule() {
        this.rpcContextPluginDataHandler.handlerRule(this.ruleData);
        assertNotNull(RpcContextPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(this.ruleData)));
    }

    @Test
    public void testRemoveRule() {
        this.rpcContextPluginDataHandler.handlerRule(this.ruleData);
        RuleData ruleData = new RuleData();
        ruleData.setSelectorId("test");
        ruleData.setName("test");
        this.rpcContextPluginDataHandler.removeRule(this.ruleData);
        assertNull(RpcContextPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(this.ruleData)));
        this.rpcContextPluginDataHandler.removeRule(ruleData);
        assertNull(RpcContextPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(ruleData)));
        this.rpcContextPluginDataHandler.removeRule(null);
    }

    @Test
    public void testPluginNamed() {
        assertEquals(this.rpcContextPluginDataHandler.pluginNamed(), PluginEnum.RPC_CONTEXT.getName());
    }
}
