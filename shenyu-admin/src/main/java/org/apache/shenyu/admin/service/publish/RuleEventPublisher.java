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

package org.apache.shenyu.admin.service.publish;

import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.RuleConditionMapper;
import org.apache.shenyu.admin.mapper.RuleMapper;
import org.apache.shenyu.admin.model.dto.RuleConditionDTO;
import org.apache.shenyu.admin.model.entity.BaseDO;
import org.apache.shenyu.admin.model.entity.RuleConditionDO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.apache.shenyu.admin.model.event.AdminDataModelChangedEvent;
import org.apache.shenyu.admin.model.event.rule.BatchRuleDeletedEvent;
import org.apache.shenyu.admin.model.event.rule.RuleChangedEvent;
import org.apache.shenyu.admin.model.event.rule.RuleCreatedEvent;
import org.apache.shenyu.admin.model.event.rule.RuleUpdatedEvent;
import org.apache.shenyu.admin.model.event.selector.BatchSelectorDeletedEvent;
import org.apache.shenyu.admin.transfer.ConditionTransfer;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Collection;
import java.util.Map;
import java.util.Optional;
import java.util.Collections;
import java.util.stream.Collectors;

import static org.apache.shenyu.admin.utils.ListUtil.groupBy;
import static org.apache.shenyu.admin.utils.ListUtil.map;

/**
 * RuleEventPublisher.
 */
@Component
public class RuleEventPublisher implements AdminDataModelChangedEventPublisher<RuleDO> {
    
    private final ApplicationEventPublisher publisher;
    
    private final RuleConditionMapper ruleConditionMapper;
    
    private final RuleMapper ruleMapper;
    
    public RuleEventPublisher(final ApplicationEventPublisher publisher,
                              final RuleConditionMapper ruleConditionMapper,
                              final PluginMapper pluginMapper,
                              final RuleMapper ruleMapper) {
        this.publisher = publisher;
        this.ruleConditionMapper = ruleConditionMapper;
        this.ruleMapper = ruleMapper;
    }
    
    /**
     * on rule created.
     *
     * @param rule rule
     */
    @Override
    public void onCreated(final RuleDO rule) {
        publish(new RuleCreatedEvent(rule, SessionUtil.visitorName()));
    }
    
    /**
     * on rule created.
     *
     * @param rule      rule
     * @param condition condition
     */
    public void onCreated(final RuleDO rule, final List<RuleConditionDTO> condition) {
        publish(new RuleCreatedEvent(rule, SessionUtil.visitorName()));
        publishEvent(rule, condition);
    }
    
    /**
     * on rule updated.
     *
     * @param rule      rule
     * @param before    before rule
     * @param condition condition
     * @param beforeCondition beforeCondition
     */
    public void onUpdated(final RuleDO rule, final RuleDO before, final List<RuleConditionDTO> condition, final List<RuleConditionDO> beforeCondition) {
        publish(new RuleUpdatedEvent(rule, before, SessionUtil.visitorName()));
        publishEvent(rule, condition, beforeCondition);
    }

    /**
     * on rule updated.
     *
     * @param rule      rule
     * @param before    before rule
     * @param condition condition
     */
    public void onUpdated(final RuleDO rule, final RuleDO before, final List<RuleConditionDTO> condition) {
        onUpdated(rule, before, condition, Collections.emptyList());
    }

    /**
     * on rule updated.
     *
     * @param rule   rule
     * @param before before rule
     */
    @Override
    public void onUpdated(final RuleDO rule, final RuleDO before) {
        publish(new RuleUpdatedEvent(rule, before, SessionUtil.visitorName()));
    }
    
    /**
     * on rule deleted.
     *
     * @param rule rule
     */
    @Override
    public void onDeleted(final RuleDO rule) {
        publish(new RuleChangedEvent(rule, null, EventTypeEnum.RULE_DELETE, SessionUtil.visitorName()));
    }
    
    /**
     * rule delete.
     *
     * @param rules data
     */
    @Override
    public void onDeleted(final Collection<RuleDO> rules) {
        publish(new BatchRuleDeletedEvent(rules, SessionUtil.visitorName(), null));
        final List<RuleConditionDO> condition = ruleConditionMapper.selectByRuleIdSet(rules.stream().map(BaseDO::getId).collect(Collectors.toSet()));
        final Map<String, List<RuleConditionDO>> conditionsRuleGroup = groupBy(condition, RuleConditionDO::getRuleId);
        final List<RuleData> ruleData = map(rules, r -> RuleDO.transFrom(r, ruleMapper.getPluginNameBySelectorId(r.getSelectorId()),
                map(conditionsRuleGroup.get(r.getId()), ConditionTransfer.INSTANCE::mapToRuleDO)));
        publisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.RULE, DataEventTypeEnum.DELETE, ruleData));
    }

    /**
     * rule delete.
     *
     * @param rules data
     * @param event BatchSelectorDeletedEvent
     */
    public void onDeleted(final List<RuleDO> rules, final BatchSelectorDeletedEvent event) {
        publish(new BatchRuleDeletedEvent(rules, SessionUtil.visitorName(), null));
        final List<RuleConditionDO> condition = ruleConditionMapper.selectByRuleIdSet(rules.stream().map(BaseDO::getId).collect(Collectors.toSet()));
        final Map<String, List<RuleConditionDO>> conditionsRuleGroup = groupBy(condition, RuleConditionDO::getRuleId);
        final List<RuleData> ruleData = map(rules, r -> RuleDO.transFrom(r,
                Optional.ofNullable(event.findPluginBySelectorId(r.getSelectorId())).map(PluginDO::getName).orElse(null),
                map(conditionsRuleGroup.get(r.getId()), ConditionTransfer.INSTANCE::mapToRuleDO)));
        publisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.RULE, DataEventTypeEnum.DELETE, ruleData));
    }

    /**
     * register.
     *
     * @param rule      rule
     * @param condition condition
     */
    public void onRegister(final RuleDO rule, final List<RuleConditionDTO> condition) {
        publishEvent(rule, condition);
    }
    
    /**
     * event.
     *
     * @param event event.
     */
    @Override
    public void publish(final AdminDataModelChangedEvent event) {
        publisher.publishEvent(event);
    }

    private void publishEvent(final RuleDO ruleDO, final List<RuleConditionDTO> condition) {
        // publish change event.
        final RuleData rule = RuleDO.transFrom(ruleDO,
                ruleMapper.getPluginNameBySelectorId(ruleDO.getSelectorId()),
                map(condition, ConditionTransfer.INSTANCE::mapToRuleDTO));
        publisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.RULE, DataEventTypeEnum.UPDATE, Collections.singletonList(rule)));
    }

    private void publishEvent(final RuleDO ruleDO, final List<RuleConditionDTO> condition, final List<RuleConditionDO> beforeCondition) {
        // publish change event.
        final RuleData rule = RuleDO.transFrom(ruleDO,
                ruleMapper.getPluginNameBySelectorId(ruleDO.getSelectorId()),
                map(condition, ConditionTransfer.INSTANCE::mapToRuleDTO),
                map(beforeCondition, ConditionTransfer.INSTANCE::mapToRuleDO)
        );
        publisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.RULE, DataEventTypeEnum.UPDATE, Collections.singletonList(rule)));
    }
}
