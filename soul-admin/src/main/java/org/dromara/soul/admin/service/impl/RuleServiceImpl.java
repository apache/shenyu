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

package org.dromara.soul.admin.service.impl;

import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.admin.model.dto.RuleConditionDTO;
import org.dromara.soul.admin.model.dto.RuleDTO;
import org.dromara.soul.admin.model.entity.PluginDO;
import org.dromara.soul.admin.model.entity.RuleConditionDO;
import org.dromara.soul.admin.model.entity.RuleDO;
import org.dromara.soul.admin.model.entity.SelectorDO;
import org.dromara.soul.admin.listener.DataChangedEvent;
import org.dromara.soul.admin.mapper.PluginMapper;
import org.dromara.soul.admin.mapper.RuleConditionMapper;
import org.dromara.soul.admin.mapper.RuleMapper;
import org.dromara.soul.admin.mapper.SelectorMapper;
import org.dromara.soul.admin.model.page.CommonPager;
import org.dromara.soul.admin.model.page.PageResultUtils;
import org.dromara.soul.admin.model.query.RuleConditionQuery;
import org.dromara.soul.admin.model.query.RuleQuery;
import org.dromara.soul.admin.service.RuleService;
import org.dromara.soul.admin.transfer.ConditionTransfer;
import org.dromara.soul.admin.model.vo.RuleConditionVO;
import org.dromara.soul.admin.model.vo.RuleVO;
import org.dromara.soul.common.dto.ConditionData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.enums.ConfigGroupEnum;
import org.dromara.soul.common.enums.DataEventTypeEnum;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * RuleServiceImpl.
 *
 * @author jiangxiaofeng(Nicholas)
 * @author xiaoyu
 */
@Service("ruleService")
public class RuleServiceImpl implements RuleService {

    private final RuleMapper ruleMapper;

    private final RuleConditionMapper ruleConditionMapper;

    private final SelectorMapper selectorMapper;

    private final PluginMapper pluginMapper;

    private final ApplicationEventPublisher eventPublisher;

    @Autowired(required = false)
    public RuleServiceImpl(final RuleMapper ruleMapper,
                           final RuleConditionMapper ruleConditionMapper,
                           final SelectorMapper selectorMapper,
                           final PluginMapper pluginMapper,
                           final ApplicationEventPublisher eventPublisher) {
        this.ruleMapper = ruleMapper;
        this.ruleConditionMapper = ruleConditionMapper;
        this.selectorMapper = selectorMapper;
        this.pluginMapper = pluginMapper;
        this.eventPublisher = eventPublisher;
    }

    @Override
    public String register(final RuleDTO ruleDTO) {
        RuleDO ruleDO = RuleDO.buildRuleDO(ruleDTO);
        List<RuleConditionDTO> ruleConditions = ruleDTO.getRuleConditions();
        if (StringUtils.isEmpty(ruleDTO.getId())) {
            ruleMapper.insertSelective(ruleDO);
            ruleConditions.forEach(ruleConditionDTO -> {
                ruleConditionDTO.setRuleId(ruleDO.getId());
                ruleConditionMapper.insertSelective(RuleConditionDO.buildRuleConditionDO(ruleConditionDTO));
            });
        }
        publishEvent(ruleDO, ruleConditions);
        return ruleDO.getId();
    }

    /**
     * create or update rule.
     *
     * @param ruleDTO {@linkplain RuleDTO}
     * @return rows
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public int createOrUpdate(final RuleDTO ruleDTO) {
        int ruleCount;
        RuleDO ruleDO = RuleDO.buildRuleDO(ruleDTO);
        List<RuleConditionDTO> ruleConditions = ruleDTO.getRuleConditions();
        if (StringUtils.isEmpty(ruleDTO.getId())) {
            ruleCount = ruleMapper.insertSelective(ruleDO);
            ruleConditions.forEach(ruleConditionDTO -> {
                ruleConditionDTO.setRuleId(ruleDO.getId());
                ruleConditionMapper.insertSelective(RuleConditionDO.buildRuleConditionDO(ruleConditionDTO));
            });
        } else {
            ruleCount = ruleMapper.updateSelective(ruleDO);
            //delete rule condition then add
            ruleConditionMapper.deleteByQuery(new RuleConditionQuery(ruleDO.getId()));
            ruleConditions.forEach(ruleConditionDTO -> {
                ruleConditionDTO.setRuleId(ruleDO.getId());
                RuleConditionDO ruleConditionDO = RuleConditionDO.buildRuleConditionDO(ruleConditionDTO);
                ruleConditionMapper.insertSelective(ruleConditionDO);
            });
        }
        publishEvent(ruleDO, ruleConditions);
        return ruleCount;
    }

    /**
     * delete rules.
     *
     * @param ids primary key.
     * @return rows
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public int delete(final List<String> ids) {
        for (String id : ids) {
            RuleDO ruleDO = ruleMapper.selectById(id);
            SelectorDO selectorDO = selectorMapper.selectById(ruleDO.getSelectorId());
            PluginDO pluginDO = pluginMapper.selectById(selectorDO.getPluginId());
            ruleMapper.delete(id);
            ruleConditionMapper.deleteByQuery(new RuleConditionQuery(id));

            // send deleted rule event
            eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.RULE, DataEventTypeEnum.DELETE,
                    Collections.singletonList(RuleDO.transFrom(ruleDO, pluginDO.getName(), null))));
        }
        return ids.size();
    }

    /**
     * find rule by id.
     *
     * @param id primary key..
     * @return {@linkplain RuleVO}
     */
    @Override
    public RuleVO findById(final String id) {
        return RuleVO.buildRuleVO(ruleMapper.selectById(id),
                ruleConditionMapper.selectByQuery(
                        new RuleConditionQuery(id)).stream()
                        .map(RuleConditionVO::buildRuleConditionVO)
                        .collect(Collectors.toList()));
    }

    /**
     * find page of rule by query.
     *
     * @param ruleQuery {@linkplain RuleQuery}
     * @return {@linkplain CommonPager}
     */
    @Override
    public CommonPager<RuleVO> listByPage(final RuleQuery ruleQuery) {
        return PageResultUtils.result(ruleQuery.getPageParameter(),
            () -> ruleMapper.countByQuery(ruleQuery),
            () -> ruleMapper.selectByQuery(ruleQuery).stream().map(RuleVO::buildRuleVO).collect(Collectors.toList()));
    }

    @Override
    public List<RuleData> listAll() {
        return ruleMapper.selectAll()
                .stream()
                .filter(Objects::nonNull)
                .map(this::buildRuleData)
                .collect(Collectors.toList());
    }

    @Override
    public List<RuleData> findBySelectorId(final String selectorId) {
        return ruleMapper.findBySelectorId(selectorId)
                .stream()
                .filter(Objects::nonNull)
                .map(this::buildRuleData)
                .collect(Collectors.toList());
    }

    private void publishEvent(final RuleDO ruleDO, final List<RuleConditionDTO> ruleConditions) {
        SelectorDO selectorDO = selectorMapper.selectById(ruleDO.getSelectorId());
        PluginDO pluginDO = pluginMapper.selectById(selectorDO.getPluginId());

        List<ConditionData> conditionDataList =
                ruleConditions.stream().map(ConditionTransfer.INSTANCE::mapToRuleDTO).collect(Collectors.toList());
        // publish change event.
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.RULE, DataEventTypeEnum.UPDATE,
                Collections.singletonList(RuleDO.transFrom(ruleDO, pluginDO.getName(), conditionDataList))));
    }

    private RuleData buildRuleData(final RuleDO ruleDO) {
        // query for conditions
        List<ConditionData> conditions = ruleConditionMapper.selectByQuery(
                new RuleConditionQuery(ruleDO.getId()))
                .stream()
                .filter(Objects::nonNull)
                .map(ConditionTransfer.INSTANCE::mapToRuleDO)
                .collect(Collectors.toList());
        SelectorDO selectorDO = selectorMapper.selectById(ruleDO.getSelectorId());
        if (Objects.isNull(selectorDO)) {
            return null;
        }
        PluginDO pluginDO = pluginMapper.selectById(selectorDO.getPluginId());
        if (Objects.isNull(pluginDO)) {
            return null;
        }
        return RuleDO.transFrom(ruleDO, pluginDO.getName(), conditions);
    }
}
