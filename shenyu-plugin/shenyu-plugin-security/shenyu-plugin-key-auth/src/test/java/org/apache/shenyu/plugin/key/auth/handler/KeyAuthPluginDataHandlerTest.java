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

package org.apache.shenyu.plugin.key.auth.handler;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.rule.impl.KeyAuthRuleHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link KeyAuthPluginDataHandler}.
 */
public class KeyAuthPluginDataHandlerTest {

    private KeyAuthPluginDataHandler keyAuthPluginDataHandler;

    private String handleString;

    @BeforeEach
    public void setUp() {
        keyAuthPluginDataHandler = new KeyAuthPluginDataHandler();
        handleString = "{\"keyName\":\"apiKey\""
                + ",\"key\":\"key\"}";
    }

    @Test
    public void testHandlerRule() {
        RuleData ruleData = new RuleData();
        ruleData.setId("keyAuthRule");
        ruleData.setSelectorId("keyAuth");
        ruleData.setHandle(handleString);
        keyAuthPluginDataHandler.handlerRule(ruleData);

        KeyAuthRuleHandle testRuleHandle = KeyAuthPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(ruleData));
        assertTrue(StringUtils.equals(testRuleHandle.getKeyName(), "apiKey"));
        assertTrue(StringUtils.equals(testRuleHandle.getKey(), "key"));
    }

    @Test
    public void testPluginNamed() {
        assertEquals(keyAuthPluginDataHandler.pluginNamed(), PluginEnum.KEY_AUTH.getName());
    }

}
