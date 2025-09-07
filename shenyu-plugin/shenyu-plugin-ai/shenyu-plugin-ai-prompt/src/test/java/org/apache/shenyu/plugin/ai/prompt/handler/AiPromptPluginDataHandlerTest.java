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

package org.apache.shenyu.plugin.ai.prompt.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.plugin.AiPromptConfig;
import org.apache.shenyu.common.dto.convert.rule.AiPromptHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class AiPromptPluginDataHandlerTest {

    private AiPromptPluginDataHandler handler;

    @BeforeEach
    void setUp() {
        handler = new AiPromptPluginDataHandler();
    }

    @Test
    void testHandlerPluginWithValidData() {
        PluginData pluginData = mock(PluginData.class);
        when(pluginData.getEnabled()).thenReturn(true);
        when(pluginData.getConfig()).thenReturn("{\"prepend\":\"testPrepend\"}");

        handler.handlerPlugin(pluginData);

        AiPromptConfig config = Singleton.INST.get(AiPromptConfig.class);
        assertNotNull(config);
        assertEquals("testPrepend", config.getPrepend());
    }

    @Test
    void testHandlerPluginWithNullData() {
        handler.handlerPlugin(null);
        AiPromptConfig config = Singleton.INST.get(AiPromptConfig.class);
        assertNull(config);
    }

    @Test
    void testHandlerRuleWithNullData() {
        assertThrows(NullPointerException.class, () -> handler.handlerRule(null));
    }

    @Test
    void testRemoveRule() {
        RuleData ruleData = mock(RuleData.class);
        when(ruleData.getHandle()).thenReturn("{\"append\":\"testAppend\"}");
        when(CacheKeyUtils.INST.getKey(ruleData)).thenReturn("testKey");

        AiPromptPluginDataHandler.CACHED_HANDLE.get().cachedHandle("testKey", new AiPromptHandle());
        handler.removeRule(ruleData);

        AiPromptHandle handle = AiPromptPluginDataHandler.CACHED_HANDLE.get().obtainHandle("testKey");
        assertEquals("AiPromptConfig{prepend='null', preRole='null', append='null', postRole='null'}", handle.toString());
    }

    @Test
    void testPluginNamed() {
        assertEquals(PluginEnum.AI_PROMPT.getName(), handler.pluginNamed());
    }
}
