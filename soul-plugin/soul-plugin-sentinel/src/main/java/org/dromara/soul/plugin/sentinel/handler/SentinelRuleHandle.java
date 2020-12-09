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

import com.alibaba.csp.sentinel.slots.block.degrade.DegradeRule;
import com.alibaba.csp.sentinel.slots.block.degrade.DegradeRuleManager;
import com.alibaba.csp.sentinel.slots.block.flow.FlowRule;
import com.alibaba.csp.sentinel.slots.block.flow.FlowRuleManager;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.convert.SentinelHandle;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.plugin.base.handler.PluginDataHandler;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Sentinel rule handle.
 *
 * @author tydhot
 */
public class SentinelRuleHandle implements PluginDataHandler {

    @Override
    public void handlerRule(final RuleData ruleData) {
        SentinelHandle sentinelHandle = GsonUtils.getInstance().fromJson(ruleData.getHandle(), SentinelHandle.class);

        List<FlowRule> flowRules = FlowRuleManager.getRules()
                .stream()
                .filter(r -> !r.getResource().equals(getResourceName(ruleData)))
                .collect(Collectors.toList());
        if (sentinelHandle.getFlowRuleEnable() == Constants.SENTINEL_ENABLE_FLOW_RULE) {
            FlowRule rule = new FlowRule(getResourceName(ruleData));
            rule.setCount(sentinelHandle.getFlowRuleCount());
            rule.setGrade(sentinelHandle.getFlowRuleGrade());
            rule.setControlBehavior(sentinelHandle.getFlowRuleControlBehavior());
            flowRules.add(rule);
        }
        FlowRuleManager.loadRules(flowRules);

        List<DegradeRule> degradeRules = DegradeRuleManager.getRules()
                .stream()
                .filter(r -> !r.getResource().equals(getResourceName(ruleData)))
                .collect(Collectors.toList());
        if (sentinelHandle.getDegradeRuleEnable() == Constants.SENTINEL_ENABLE_DEGRADE_RULE) {
            DegradeRule rule = new DegradeRule(getResourceName(ruleData));
            rule.setCount(sentinelHandle.getDegradeRuleCount());
            rule.setGrade(sentinelHandle.getDegradeRuleGrade());
            rule.setTimeWindow(sentinelHandle.getDegradeRuleTimeWindow());
            degradeRules.add(rule);
        }
        DegradeRuleManager.loadRules(degradeRules);
    }

    @Override
    public void removeRule(final RuleData ruleData) {
        FlowRuleManager.loadRules(FlowRuleManager.getRules()
                .stream()
                .filter(r -> !r.getResource().equals(getResourceName(ruleData)))
                .collect(Collectors.toList()));
        DegradeRuleManager.loadRules(DegradeRuleManager.getRules()
                .stream()
                .filter(r -> !r.getResource().equals(getResourceName(ruleData)))
                .collect(Collectors.toList()));
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.SENTINEL.getName();
    }

    /**
     * return sentinel resource name.
     *
     * @param ruleData ruleData
     * @return string string
     */
    public static String getResourceName(final RuleData ruleData) {
        return ruleData.getSelectorId() + "_" + ruleData.getName();
    }

}
