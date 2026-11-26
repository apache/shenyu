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
import com.alibaba.csp.sentinel.slots.block.flow.param.ParamFlowRule;
import com.alibaba.csp.sentinel.slots.block.flow.param.ParamFlowRuleManager;
import com.google.gson.JsonParseException;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.SentinelHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.base.utils.BeanHolder;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Objects;
import java.util.function.Supplier;
import java.util.stream.Collectors;

/**
 * Sentinel rule handle.
 */
public class SentinelRuleHandle implements PluginDataHandler {
    
    public static final Supplier<CommonHandleCache<String, SentinelHandle>> CACHED_HANDLE = new BeanHolder<>(CommonHandleCache::new);

    private static final Logger LOG = LoggerFactory.getLogger(SentinelRuleHandle.class);

    private static final int MILLIS_PER_SECOND = 1000;

    private static final Object RULE_UPDATE_LOCK = new Object();
    
    @Override
    public void handlerSelector(final SelectorData selectorData) {
        if (!selectorData.getContinued()) {
            CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(selectorData.getId(), Constants.DEFAULT_RULE), SentinelHandle.newDefaultInstance());
        }
    }
    
    @Override
    public void removeSelector(final SelectorData selectorData) {
        CACHED_HANDLE.get().removeHandle(CacheKeyUtils.INST.getKey(selectorData.getId(), Constants.DEFAULT_RULE));
    }

    @Override
    public void handlerRule(final RuleData ruleData) {
        if (Boolean.FALSE.equals(ruleData.getEnabled()) || StringUtils.isBlank(ruleData.getHandle())) {
            // a cleared (null or blank) handle must evict the cached rule and
            // its flow/degrade enforcement, not crash or go stale (#7914)
            removeRule(ruleData);
            return;
        }
        String key = CacheKeyUtils.INST.getKey(ruleData);
        final SentinelHandle sentinelHandle;
        try {
            sentinelHandle = GsonUtils.getInstance().fromJson(ruleData.getHandle(), SentinelHandle.class);
        } catch (JsonParseException ex) {
            LOG.warn("Ignoring invalid Sentinel rule handle for resource {}", key);
            return;
        }
        if (Objects.isNull(sentinelHandle)) {
            LOG.warn("Ignoring empty Sentinel rule handle for resource {}", key);
            return;
        }
        try {
            sentinelHandle.validateParamFlow();
            sentinelHandle.checkData();
        } catch (IllegalArgumentException ex) {
            LOG.warn("Ignoring Sentinel rule update for resource {}: {}", key, ex.getMessage());
            return;
        }
        synchronized (RULE_UPDATE_LOCK) {
            boolean flowEnabled = sentinelHandle.getFlowRuleEnable() == Constants.FLOW_RULE_ENABLE_ON;
            if (flowEnabled && sentinelHandle.isParamFlowEnabled()) {
                replaceParamRule(key, new ParamFlowRule(key).setParamIdx(0).setCount(sentinelHandle.getFlowRuleCount()));
                CACHED_HANDLE.get().cachedHandle(key, sentinelHandle);
                replaceFlowRule(key, null);
            } else {
                replaceFlowRule(key, flowEnabled ? ordinaryFlowRule(key, sentinelHandle) : null);
                CACHED_HANDLE.get().cachedHandle(key, sentinelHandle);
                replaceParamRule(key, null);
            }
            replaceDegradeRule(key, sentinelHandle);
        }
    }

    private FlowRule ordinaryFlowRule(final String key, final SentinelHandle handle) {
        FlowRule rule = new FlowRule(key);
        rule.setCount(handle.getFlowRuleCount());
        rule.setGrade(handle.getFlowRuleGrade());
        rule.setControlBehavior(handle.getFlowRuleControlBehavior());
        rule.setMaxQueueingTimeMs(handle.getFlowRuleMaxQueueingTimeMs());
        rule.setWarmUpPeriodSec(handle.getFlowRuleWarmUpPeriodSec());
        return rule;
    }

    private void replaceFlowRule(final String key, final FlowRule replacement) {
        List<FlowRule> rules = FlowRuleManager.getRules().stream().filter(rule -> !key.equals(rule.getResource())).collect(Collectors.toList());
        if (Objects.nonNull(replacement)) {
            rules.add(replacement);
        }
        FlowRuleManager.loadRules(rules);
    }

    private void replaceParamRule(final String key, final ParamFlowRule replacement) {
        List<ParamFlowRule> current = ParamFlowRuleManager.getRules();
        List<ParamFlowRule> rules = current.stream().filter(rule -> !key.equals(rule.getResource())).collect(Collectors.toList());
        if (Objects.nonNull(replacement)) {
            rules.add(replacement);
        } else if (rules.size() == current.size()) {
            return;
        }
        ParamFlowRuleManager.loadRules(rules);
    }

    private void replaceDegradeRule(final String key, final SentinelHandle handle) {
        List<DegradeRule> rules = DegradeRuleManager.getRules().stream().filter(rule -> !key.equals(rule.getResource())).collect(Collectors.toList());
        if (Objects.nonNull(handle) && handle.getDegradeRuleEnable() == Constants.DEGRADE_RULE_ENABLE_ON) {
            DegradeRule rule = new DegradeRule(key);
            rule.setCount(handle.getDegradeRuleCount());
            rule.setGrade(handle.getDegradeRuleGrade());
            rule.setTimeWindow(handle.getDegradeRuleTimeWindow());
            rule.setStatIntervalMs(handle.getDegradeRuleStatIntervals() * MILLIS_PER_SECOND);
            rule.setMinRequestAmount(handle.getDegradeRuleMinRequestAmount());
            rule.setSlowRatioThreshold(handle.getDegradeRuleSlowRatioThreshold());
            rules.add(rule);
        }
        DegradeRuleManager.loadRules(rules);
    }

    @Override
    public void removeRule(final RuleData ruleData) {
        String key = CacheKeyUtils.INST.getKey(ruleData);
        synchronized (RULE_UPDATE_LOCK) {
            CACHED_HANDLE.get().removeHandle(key);
            replaceFlowRule(key, null);
            replaceParamRule(key, null);
            replaceDegradeRule(key, null);
        }
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.SENTINEL.getName();
    }
}
