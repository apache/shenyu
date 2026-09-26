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

package org.apache.shenyu.plugin.agent.gateway.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.agent.gateway.handle.AgentGatewayRuleHandle;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AgentGatewayPluginDataHandlerTest {

    private final AgentGatewayPluginDataHandler handler = new AgentGatewayPluginDataHandler();

    @AfterEach
    void clearCache() {
        AgentGatewayPluginDataHandler.CACHED_HANDLE.get().getAllCache().clear();
    }

    @Test
    void shouldExposePluginName() {
        assertEquals(PluginEnum.AGENT_GATEWAY.getName(), handler.pluginNamed());
    }

    @Test
    void shouldCacheValidAndInvalidRuleSnapshots() {
        RuleData validRule = rule("selector-1", "rule-1", "{\"trafficType\":\"LLM\"}");
        RuleData invalidRule = rule("selector-1", "rule-2", "{\"trafficType\":\"MCP\"}");

        handler.handlerRule(validRule);
        handler.handlerRule(invalidRule);

        AgentGatewayRuleHandle valid = AgentGatewayPluginDataHandler.CACHED_HANDLE.get()
                .obtainHandle(CacheKeyUtils.INST.getKey(validRule));
        AgentGatewayRuleHandle invalid = AgentGatewayPluginDataHandler.CACHED_HANDLE.get()
                .obtainHandle(CacheKeyUtils.INST.getKey(invalidRule));
        assertTrue(valid.isValid());
        assertFalse(invalid.isValid());
    }

    @Test
    void shouldRemoveAllRulesBelongingToSelector() {
        RuleData first = rule("selector-1", "rule-1", "{\"trafficType\":\"LLM\"}");
        RuleData second = rule("selector-1", "rule-2", "{\"trafficType\":\"LLM\"}");
        RuleData other = rule("selector-2", "rule-1", "{\"trafficType\":\"LLM\"}");
        handler.handlerRule(first);
        handler.handlerRule(second);
        handler.handlerRule(other);

        handler.removeSelector(SelectorData.builder().id("selector-1").build());

        assertEquals(1, AgentGatewayPluginDataHandler.CACHED_HANDLE.get().getAllCache().size());
        assertTrue(Objects.nonNull(AgentGatewayPluginDataHandler.CACHED_HANDLE.get()
                .obtainHandle(CacheKeyUtils.INST.getKey(other))));
    }

    @Test
    void shouldRemoveAllRulesWhenPluginIsRemoved() {
        handler.handlerRule(rule("selector-1", "rule-1", "{\"trafficType\":\"LLM\"}"));

        handler.removePlugin(new PluginData());

        assertTrue(AgentGatewayPluginDataHandler.CACHED_HANDLE.get().getAllCache().isEmpty());
    }

    @Test
    void shouldRemoveOneRuleWithoutTouchingOtherSelectors() {
        RuleData first = rule("selector-1", "rule-1", "{\"trafficType\":\"LLM\"}");
        RuleData other = rule("selector-1", "rule-2", "{\"trafficType\":\"LLM\"}");
        handler.handlerRule(first);
        handler.handlerRule(other);

        handler.removeRule(first);

        assertTrue(Objects.isNull(AgentGatewayPluginDataHandler.CACHED_HANDLE.get()
                .obtainHandle(CacheKeyUtils.INST.getKey(first))));
        assertTrue(Objects.nonNull(AgentGatewayPluginDataHandler.CACHED_HANDLE.get()
                .obtainHandle(CacheKeyUtils.INST.getKey(other))));
    }

    private RuleData rule(final String selectorId, final String ruleId, final String handleValue) {
        return RuleData.builder().selectorId(selectorId).id(ruleId).handle(handleValue).build();
    }
}
