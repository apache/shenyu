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

package org.apache.shenyu.plugin.sentinel.handler;

import com.alibaba.csp.sentinel.slots.block.degrade.DegradeRule;
import com.alibaba.csp.sentinel.slots.block.degrade.DegradeRuleManager;
import com.alibaba.csp.sentinel.slots.block.flow.FlowRule;
import com.alibaba.csp.sentinel.slots.block.flow.FlowRuleManager;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.rule.SentinelHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Sentinel rule handle.
 */
public class SentinelRuleHandle implements PluginDataHandler {

    @Override
    public void handlerRule(final RuleData ruleData) {
        SentinelHandle sentinelHandle = GsonUtils.getInstance().fromJson(ruleData.getHandle(), SentinelHandle.class);
        sentinelHandle.checkData(sentinelHandle);
        String key = CacheKeyUtils.INST.getKey(ruleData);
        List<FlowRule> flowRules = FlowRuleManager.getRules()
                .stream()
                .filter(r -> !r.getResource().equals(key))
                .collect(Collectors.toList());
        if (sentinelHandle.getFlowRuleEnable() == Constants.SENTINEL_ENABLE_FLOW_RULE) {
            FlowRule rule = new FlowRule(key);
            rule.setCount(sentinelHandle.getFlowRuleCount());
            rule.setGrade(sentinelHandle.getFlowRuleGrade());
            rule.setControlBehavior(sentinelHandle.getFlowRuleControlBehavior());
            rule.setMaxQueueingTimeMs(sentinelHandle.getFlowRuleMaxQueueingTimeMs());
            rule.setWarmUpPeriodSec(sentinelHandle.getFlowRuleWarmUpPeriodSec());
            flowRules.add(rule);
        }
        FlowRuleManager.loadRules(flowRules);

        List<DegradeRule> degradeRules = DegradeRuleManager.getRules()
                .stream()
                .filter(r -> !r.getResource().equals(key))
                .collect(Collectors.toList());
        if (sentinelHandle.getDegradeRuleEnable() == Constants.SENTINEL_ENABLE_DEGRADE_RULE) {
            DegradeRule rule = new DegradeRule(key);
            rule.setCount(sentinelHandle.getDegradeRuleCount());
            rule.setGrade(sentinelHandle.getDegradeRuleGrade());
            rule.setTimeWindow(sentinelHandle.getDegradeRuleTimeWindow());
            rule.setStatIntervalMs(sentinelHandle.getDegradeRuleStatIntervals() * 1000);
            rule.setMinRequestAmount(sentinelHandle.getDegradeRuleMinRequestAmount());
            rule.setSlowRatioThreshold(sentinelHandle.getDegradeRuleSlowRatioThreshold());
            degradeRules.add(rule);
        }
        DegradeRuleManager.loadRules(degradeRules);
    }

    @Override
    public void removeRule(final RuleData ruleData) {
        String key = CacheKeyUtils.INST.getKey(ruleData);
        FlowRuleManager.loadRules(FlowRuleManager.getRules()
                .stream()
                .filter(r -> !r.getResource().equals(key))
                .collect(Collectors.toList()));
        DegradeRuleManager.loadRules(DegradeRuleManager.getRules()
                .stream()
                .filter(r -> !r.getResource().equals(key))
                .collect(Collectors.toList()));
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.SENTINEL.getName();
    }
}
