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

package org.dromara.soul.plugin.sentinel.handler;

import com.alibaba.csp.sentinel.slots.block.degrade.DegradeRuleManager;
import com.alibaba.csp.sentinel.slots.block.flow.FlowRuleManager;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.enums.PluginEnum;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.mockito.junit.MockitoJUnitRunner;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertEquals;

@RunWith(MockitoJUnitRunner.class)
public class SentinelRuleHandleTest {

    private SentinelRuleHandle sentinelRuleHandle;

    @Before
    public void setUp() {
        sentinelRuleHandle = new SentinelRuleHandle();
    }

    @Test
    public void handlerRuleTest() {
        testFlowRuleEnable();
        testDegradeRuleManager();
    }

    @Test
    public void pluginNamedTest() {
        assertEquals(sentinelRuleHandle.pluginNamed(), PluginEnum.SENTINEL.getName());
    }

    @Test
    public void removeRule() {
        RuleData ruleData = buildRuleData("");
        sentinelRuleHandle.removeRule(ruleData);
        assertTrue(FlowRuleManager.getRules().isEmpty());
    }

    private void testFlowRuleEnable() {
        RuleData ruleData = buildRuleData("{\"flowRuleEnable\":\"1\",\"flowRuleCount\":\"1\",\"degradeRuleEnable\":\"0\",\"degradeRuleCount\":\"0\"}");
        sentinelRuleHandle.handlerRule(ruleData);
        assertFalse(FlowRuleManager.getRules().isEmpty());
    }

    private void testDegradeRuleManager() {
        RuleData ruleData = buildRuleData("{\"flowRuleEnable\":\"0\",\"flowRuleCount\":\"0\",\"degradeRuleEnable\":\"1\",\"degradeRuleCount\":\"1\",\"degradeRuleTimeWindow\":\"1\"}");
        sentinelRuleHandle.handlerRule(ruleData);
        assertFalse(DegradeRuleManager.getRules().isEmpty());
    }

    private RuleData buildRuleData(final String handler) {
        RuleData ruleData = new RuleData();
        ruleData.setId(null);
        ruleData.setEnabled(true);
        ruleData.setHandle(handler);
        ruleData.setLoged(true);
        ruleData.setMatchMode(1);
        ruleData.setName("sentinel");
        ruleData.setPluginName("sentinel");
        ruleData.setSelectorId("1");
        ruleData.setSort(1);
        return ruleData;
    }
}
