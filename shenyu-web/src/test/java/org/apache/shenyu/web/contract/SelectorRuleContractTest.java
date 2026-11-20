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

package org.apache.shenyu.web.contract;

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.junit.jupiter.api.Test;
import org.springframework.web.server.ServerWebExchange;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Tests the selector/rule profile against the real {@code AbstractShenyuPlugin} matching path.
 */
public class SelectorRuleContractTest {

    @Test
    public void matchingSelectorAndRuleReachThePluginBodyAndMissFallsBack() {
        final PluginData pluginData = PluginData.builder()
                .name("selector-rule")
                .enabled(true)
                .build();
        final ConditionData condition = new ConditionData();
        condition.setOperator("=");
        condition.setParamType("uri");
        condition.setParamName("/contract");
        condition.setParamValue("/contract");
        final SelectorData selectorData = SelectorData.builder()
                .id("selector-1")
                .pluginName("selector-rule")
                .enabled(true)
                .matchRestful(false)
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                .build();
        selectorData.setConditionList(List.of(condition));
        selectorData.setMatchMode(0);
        selectorData.setSort(1);
        selectorData.setLogged(true);
        final RuleData ruleData = RuleData.builder()
                .id("rule-1")
                .pluginName("selector-rule")
                .selectorId("selector-1")
                .enabled(true)
                .matchRestful(false)
                .build();
        ruleData.setConditionDataList(List.of(condition));
        ruleData.setMatchMode(0);
        ruleData.setSort(1);
        ruleData.setLogged(true);

        try (SelectorRuleFixture fixture = new SelectorRuleFixture()) {
            fixture.cachePluginData(pluginData);
            fixture.cacheSelectorData(selectorData);
            fixture.cacheRuleData(ruleData);
            final SelectorRulePlugin plugin = new SelectorRulePlugin("selector-rule", 1);
            final ContractProbe probe = ContractProbe.around(plugin, "selector-rule", 1,
                    PluginContractProfile.SELECTOR_RULE);
            final RecordingPlugin terminal = new RecordingPlugin("terminal", 2,
                    RecordingPlugin.Role.PASS_THROUGH);
            final LegacyPluginRuntime runtime = new LegacyPluginRuntime(probe, terminal);

            PluginContractAssertions.assertCompletes(runtime, LegacyPluginRuntime.newExchange());
            assertEquals(1, plugin.getDoExecuteCount());
            assertEquals("selector-1", plugin.getMatchedSelector().getId());
            assertEquals("rule-1", plugin.getMatchedRule().getId());
            PluginContractAssertions.assertContinuation(probe);
            assertEquals(1, terminal.getExecuteCount());

            fixture.removeRuleData(ruleData);
            final ServerWebExchange missExchange = LegacyPluginRuntime.newExchange();
            PluginContractAssertions.assertCompletes(runtime, missExchange);
            assertEquals(1, plugin.getDoExecuteCount(),
                    "the plugin body must not run again after the rule is removed");
            assertEquals(2, terminal.getExecuteCount(),
                    "a rule miss must continue to the downstream plugin");
            PluginContractAssertions.assertContinuation(probe);
        }
    }
}
