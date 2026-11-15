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

package org.apache.shenyu.plugin.ai.requestid.handler;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.ai.requestid.handle.AiRequestIdRule;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AiRequestIdPluginDataHandlerTest {

    private final AiRequestIdPluginDataHandler handler = new AiRequestIdPluginDataHandler();

    @AfterEach
    void clearCache() {
        AiRequestIdPluginDataHandler.CACHED_HANDLE.get().getAllCache().clear();
    }

    @Test
    void shouldExposePluginName() {
        assertEquals(PluginEnum.AI_REQUEST_ID.getName(), handler.pluginNamed());
    }

    @Test
    void shouldCacheAndRemoveRuleHandle() {
        RuleData rule = rule("rule-1", "{\"responseHeaderEnabled\":false}");
        handler.handlerRule(rule);

        AiRequestIdRule cached = AiRequestIdPluginDataHandler.CACHED_HANDLE.get().obtainHandle("selector-1_rule-1");
        assertNotNull(cached);
        assertTrue(cached.isValid());
        assertFalse(cached.isResponseHeaderEnabled());

        handler.removeRule(rule);
        assertNull(AiRequestIdPluginDataHandler.CACHED_HANDLE.get().obtainHandle("selector-1_rule-1"));
    }

    @Test
    void shouldSeedDefaultRuleForNonContinuedSelector() {
        handler.handlerSelector(SelectorData.builder().id("selector-1").continued(false).build());

        AiRequestIdRule cached = AiRequestIdPluginDataHandler.CACHED_HANDLE.get()
                .obtainHandle("selector-1_" + Constants.DEFAULT_RULE);
        assertNotNull(cached);
        assertTrue(cached.isValid());
        assertTrue(cached.isResponseHeaderEnabled());
    }

    @Test
    void shouldNotSeedDefaultRuleForContinuedSelector() {
        handler.handlerSelector(SelectorData.builder().id("selector-1").continued(true).build());

        assertNull(AiRequestIdPluginDataHandler.CACHED_HANDLE.get()
                .obtainHandle("selector-1_" + Constants.DEFAULT_RULE));
    }

    @Test
    void shouldRemoveAllRulesOfSelector() {
        handler.handlerRule(rule("rule-1", "{}"));
        handler.handlerRule(rule("rule-2", "{}"));
        handler.handlerRule(RuleData.builder().id("other-rule").selectorId("selector-2").handle("{}").build());

        handler.removeSelector(SelectorData.builder().id("selector-1").continued(true).build());

        assertNull(AiRequestIdPluginDataHandler.CACHED_HANDLE.get().obtainHandle("selector-1_rule-1"));
        assertNull(AiRequestIdPluginDataHandler.CACHED_HANDLE.get().obtainHandle("selector-1_rule-2"));
        assertNotNull(AiRequestIdPluginDataHandler.CACHED_HANDLE.get().obtainHandle("selector-2_other-rule"));
    }

    @Test
    void shouldClearCacheWhenPluginIsRemoved() {
        handler.handlerRule(rule("rule-1", "{}"));

        handler.removePlugin(new PluginData());

        assertTrue(AiRequestIdPluginDataHandler.CACHED_HANDLE.get().getAllCache().isEmpty());
    }

    @Test
    void shouldIgnoreNullData() {
        handler.handlerRule(null);
        handler.removeRule(null);
        handler.handlerSelector(null);
        handler.removeSelector(null);
        handler.removePlugin(null);

        assertTrue(AiRequestIdPluginDataHandler.CACHED_HANDLE.get().getAllCache().isEmpty());
    }

    private RuleData rule(final String id, final String handle) {
        return RuleData.builder().id(id).selectorId("selector-1").handle(handle).build();
    }
}
