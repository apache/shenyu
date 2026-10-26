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

package org.apache.shenyu.plugin.modify.response.handler;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.rule.impl.ModifyResponseRuleHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EmptySource;
import org.junit.jupiter.params.provider.ValueSource;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The Test Case For ModifyResponsePluginDataHandler.
 */
public final class ModifyResponsePluginDataHandlerTest {

    private RuleData ruleData;

    private ModifyResponsePluginDataHandler modifyResponsePluginDataHandler;

    @BeforeEach
    public void setUp() {
        this.modifyResponsePluginDataHandler = new ModifyResponsePluginDataHandler();
        this.ruleData = mock(RuleData.class);
        when(ruleData.getSelectorId()).thenReturn("1");
        when(ruleData.getName()).thenReturn("modify");
        ModifyResponseRuleHandle modifyResponseRuleHandle = new ModifyResponseRuleHandle();
        modifyResponseRuleHandle.setStatusCode(400);
        when(ruleData.getHandle()).thenReturn(GsonUtils.getGson().toJson(modifyResponseRuleHandle));
    }

    /**
     * Handler selector test.
     */
    @Test
    public void handlerSelectorTest() {
        modifyResponsePluginDataHandler.handlerRule(ruleData);
        ModifyResponseRuleHandle modifyResponseRuleHandle = ModifyResponsePluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(ruleData));
        assertEquals(400, modifyResponseRuleHandle.getStatusCode());
    }

    /**
     * Remove selector test.
     */
    @Test
    public void removeSelectorTest() {
        modifyResponsePluginDataHandler.handlerRule(ruleData);
        when(ruleData.getHandle()).thenReturn(null);
        modifyResponsePluginDataHandler.removeRule(ruleData);
        ModifyResponseRuleHandle modifyResponseRuleHandle = ModifyResponsePluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(ruleData));
        assertNull(modifyResponseRuleHandle);
    }

    /**
     * Plugin named test.
     */
    @Test
    public void pluginNamedTest() {
        assertEquals(modifyResponsePluginDataHandler.pluginNamed(), PluginEnum.MODIFY_RESPONSE.getName());
    }

    @ParameterizedTest
    @EmptySource
    @ValueSource(strings = {" ", "\t\r\n"})
    public void testBlankHandleClearsCachedRule(final String handle) {
        modifyResponsePluginDataHandler.handlerRule(ruleData);
        String key = CacheKeyUtils.INST.getKey(ruleData);
        assertNotNull(ModifyResponsePluginDataHandler.CACHED_HANDLE.get().obtainHandle(key));

        when(ruleData.getHandle()).thenReturn(handle);
        modifyResponsePluginDataHandler.handlerRule(ruleData);
        assertNull(ModifyResponsePluginDataHandler.CACHED_HANDLE.get().obtainHandle(key));

        modifyResponsePluginDataHandler.handlerRule(ruleData);
        assertNull(ModifyResponsePluginDataHandler.CACHED_HANDLE.get().obtainHandle(key));
    }

    @Test
    public void testNullHandleRetainsCachedRule() {
        modifyResponsePluginDataHandler.handlerRule(ruleData);
        String key = CacheKeyUtils.INST.getKey(ruleData);
        ModifyResponseRuleHandle cached = ModifyResponsePluginDataHandler.CACHED_HANDLE.get().obtainHandle(key);

        when(ruleData.getHandle()).thenReturn(null);
        modifyResponsePluginDataHandler.handlerRule(ruleData);

        assertSame(cached, ModifyResponsePluginDataHandler.CACHED_HANDLE.get().obtainHandle(key));
    }

    @Test
    public void testEmptyObjectReplacesCachedRule() {
        modifyResponsePluginDataHandler.handlerRule(ruleData);
        when(ruleData.getHandle()).thenReturn("{}");

        modifyResponsePluginDataHandler.handlerRule(ruleData);

        ModifyResponseRuleHandle cached = ModifyResponsePluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(ruleData));
        assertNotNull(cached);
        assertEquals(0, cached.getStatusCode());
    }
}
