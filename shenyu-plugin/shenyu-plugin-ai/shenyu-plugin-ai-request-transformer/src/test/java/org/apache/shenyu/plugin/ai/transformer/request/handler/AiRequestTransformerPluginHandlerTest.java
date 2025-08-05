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

package org.apache.shenyu.plugin.ai.transformer.request.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.rule.AiRequestTransformerHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.ai.common.spring.ai.registry.AiModelFactoryRegistry;
import org.apache.shenyu.plugin.ai.transformer.request.cache.ChatClientCache;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verifyNoInteractions;

class AiRequestTransformerPluginHandlerTest {

    private AiModelFactoryRegistry aiModelFactoryRegistry;

    private ChatClientCache chatClientCache;

    private AiRequestTransformerPluginHandler pluginHandler;

    @BeforeEach
    void setUp() {
        chatClientCache = ChatClientCache.getInstance();
        aiModelFactoryRegistry = mock(AiModelFactoryRegistry.class);
        pluginHandler = new AiRequestTransformerPluginHandler(aiModelFactoryRegistry);
    }

    @Test
    void testHandlerPluginWithInvalidData() {

        PluginData pluginData = new PluginData();
        pluginData.setEnabled(true);
        pluginData.setConfig(null);

        pluginHandler.handlerPlugin(pluginData);

        verifyNoInteractions(aiModelFactoryRegistry);
    }

    @Test
    void testHandlerRule() {

        RuleData ruleData = new RuleData();
        ruleData.setId("rule1");
        AiRequestTransformerHandle handle = new AiRequestTransformerHandle();
        handle.setProvider("TEST_PROVIDER");
        ruleData.setHandle(GsonUtils.getInstance().toJson(handle));

        pluginHandler.handlerRule(ruleData);

        CommonHandleCache<String, AiRequestTransformerHandle> cache = AiRequestTransformerPluginHandler.CACHED_HANDLE.get();
        assertNotNull(cache.obtainHandle(CacheKeyUtils.INST.getKey(ruleData)));
    }

    @Test
    void testRemoveRule() {

        RuleData ruleData = new RuleData();
        ruleData.setId("rule1");
        AiRequestTransformerHandle handle = new AiRequestTransformerHandle();
        handle.setProvider("TEST_PROVIDER");
        ruleData.setHandle(GsonUtils.getInstance().toJson(handle));

        pluginHandler.removeRule(ruleData);

        CommonHandleCache<String, AiRequestTransformerHandle> cache = AiRequestTransformerPluginHandler.CACHED_HANDLE.get();
        assertNull(cache.obtainHandle(CacheKeyUtils.INST.getKey(ruleData)));
    }

    @Test
    void testPluginNamed() {
        assertEquals(PluginEnum.AI_REQUEST_TRANSFORMER.getName(), pluginHandler.pluginNamed());
    }
}
