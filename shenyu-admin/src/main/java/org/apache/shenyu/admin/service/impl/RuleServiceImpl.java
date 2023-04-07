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

package org.apache.shenyu.admin.service.impl;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.aspect.annotation.DataPermission;
import org.apache.shenyu.admin.aspect.annotation.Pageable;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.RuleConditionMapper;
import org.apache.shenyu.admin.mapper.RuleMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.RuleConditionDTO;
import org.apache.shenyu.admin.model.dto.RuleDTO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.RuleConditionDO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.event.selector.BatchSelectorDeletedEvent;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.RuleConditionQuery;
import org.apache.shenyu.admin.model.query.RuleQuery;
import org.apache.shenyu.admin.model.query.RuleQueryCondition;
import org.apache.shenyu.admin.model.vo.RuleConditionVO;
import org.apache.shenyu.admin.model.vo.RuleVO;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.publish.RuleEventPublisher;
import org.apache.shenyu.admin.transfer.ConditionTransfer;
import org.apache.shenyu.admin.utils.Assert;
import org.apache.shenyu.admin.utils.ListUtil;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.apache.shenyu.admin.utils.ListUtil.map;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.RuleService}.
 */
@Service
public class RuleServiceImpl implements RuleService {
    
    private final RuleMapper ruleMapper;
    
    private final RuleConditionMapper ruleConditionMapper;
    
    private final SelectorMapper selectorMapper;
    
    private final PluginMapper pluginMapper;
    
    private final RuleEventPublisher ruleEventPublisher;
    
    public RuleServiceImpl(final RuleMapper ruleMapper,
                           final RuleConditionMapper ruleConditionMapper,
                           final SelectorMapper selectorMapper,
                           final PluginMapper pluginMapper,
                           final RuleEventPublisher ruleEventPublisher) {
        this.ruleMapper = ruleMapper;
        this.ruleConditionMapper = ruleConditionMapper;
        this.selectorMapper = selectorMapper;
        this.pluginMapper = pluginMapper;
        this.ruleEventPublisher = ruleEventPublisher;
    }
    
    @Override
    public void doConditionPreProcessing(final RuleQueryCondition condition) {
        if (SessionUtil.isAdmin()) {
            condition.setUserId(null);
        }
    }
    
    @Override
    public List<RuleVO> searchByCondition(final RuleQueryCondition condition) {
        condition.init();
        final List<RuleVO> rules = ruleMapper.selectByCondition(condition);
        for (RuleVO rule : rules) {
            rule.setMatchModeName(MatchModeEnum.getMatchModeByCode(rule.getMatchMode()));
        }
        return rules;
    }
    
    @Override
    public String registerDefault(final RuleDTO ruleDTO) {
        if (Objects.nonNull(ruleMapper.findBySelectorIdAndName(ruleDTO.getSelectorId(), ruleDTO.getName()))) {
            return "";
        }
        RuleDO ruleDO = RuleDO.buildRuleDO(ruleDTO);
        if (StringUtils.isEmpty(ruleDTO.getId())) {
            ruleMapper.insertSelective(ruleDO);
            addCondition(ruleDO, ruleDTO.getRuleConditions());
        }
        ruleEventPublisher.onRegister(ruleDO, ruleDTO.getRuleConditions());
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
        return RuleService.super.createOrUpdate(ruleDTO);
    }
    
    @Override
    public int create(final RuleDTO ruleDTO) {
        RuleDO ruleDO = RuleDO.buildRuleDO(ruleDTO);
        final int ruleCount = ruleMapper.insertSelective(ruleDO);
        addCondition(ruleDO, ruleDTO.getRuleConditions());
        if (ruleCount > 0) {
            ruleEventPublisher.onCreated(ruleDO, ruleDTO.getRuleConditions());
        }
        return ruleCount;
    }
    
    @Override
    public int update(final RuleDTO ruleDTO) {
        final RuleDO before = ruleMapper.selectById(ruleDTO.getId());
        Assert.notNull(before, "the updated rule is not found");
        RuleDO ruleDO = RuleDO.buildRuleDO(ruleDTO);
        final int ruleCount = ruleMapper.updateSelective(ruleDO);

        // need old data for cleaning
        List<RuleConditionDO> beforeRuleCondition = ruleConditionMapper.selectByQuery(new RuleConditionQuery(ruleDO.getId()));
        List<RuleConditionDTO> beforRuleCondition = beforeRuleCondition.stream().map(ruleConditionDO ->
                RuleConditionDTO.builder()
                        .ruleId(ruleConditionDO.getRuleId())
                        .operator(ruleConditionDO.getOperator())
                        .paramName(ruleConditionDO.getParamName())
                        .paramType(ruleConditionDO.getParamType())
                        .paramValue(ruleConditionDO.getParamValue())
                        .build()).collect(Collectors.toList());
        List<RuleConditionDTO> currentRuleCondition = ruleDTO.getRuleConditions().stream().map(ruleConditionDTO ->
                RuleConditionDTO.builder()
                        .ruleId(ruleConditionDTO.getRuleId())
                        .operator(ruleConditionDTO.getOperator())
                        .paramName(ruleConditionDTO.getParamName())
                        .paramType(ruleConditionDTO.getParamType())
                        .paramValue(ruleConditionDTO.getParamValue())
                        .build()).collect(Collectors.toList());
        if (CollectionUtils.isEqualCollection(beforRuleCondition, currentRuleCondition)) {
            beforeRuleCondition = Collections.emptyList();
        }
        //delete rule condition then add
        ruleConditionMapper.deleteByQuery(new RuleConditionQuery(ruleDO.getId()));

        // insert new condition
        addCondition(ruleDO, ruleDTO.getRuleConditions());
        if (ruleCount > 0) {
            ruleEventPublisher.onUpdated(ruleDO, before, ruleDTO.getRuleConditions(), beforeRuleCondition);
        }
        return ruleCount;
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
                map(ruleConditionMapper.selectByQuery(new RuleConditionQuery(id)), RuleConditionVO::buildRuleConditionVO));
    }
    
    /**
     * find page of rule by query.
     *
     * @param ruleQuery {@linkplain RuleQuery}
     * @return {@linkplain CommonPager}
     */
    @Override
    @DataPermission(dataType = AdminConstants.DATA_PERMISSION_RULE)
    @Pageable
    public CommonPager<RuleVO> listByPage(final RuleQuery ruleQuery) {
        return PageResultUtils.result(ruleQuery.getPageParameter(), () -> map(ruleMapper.selectByQuery(ruleQuery), RuleVO::buildRuleVO));
    }
    
    @Override
    public List<RuleData> listAll() {
        return this.buildRuleDataList(ruleMapper.selectAll());
    }
    
    @Override
    public List<RuleData> findBySelectorId(final String selectorId) {
        return this.buildRuleDataList(ruleMapper.findBySelectorId(selectorId));
    }
    
    @Override
    public List<RuleData> findBySelectorIdList(final List<String> selectorIdList) {
        return this.buildRuleDataList(ruleMapper.findBySelectorIds(selectorIdList));
    }
    
    @Override
    public RuleDO findByName(final String name) {
        return ruleMapper.findByName(name);
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
        List<RuleDO> rules = ruleMapper.selectByIds(ids);
        final int deleteCount = ruleMapper.deleteByIds(ids);
        if (deleteCount > 0) {
            ruleEventPublisher.onDeleted(rules);
            ruleConditionMapper.deleteByRuleIds(ids);
        }
        return deleteCount;
    }
    
    /**
     * listen {@link BatchSelectorDeletedEvent} delete rule.
     *
     * @param event event
     */
    @EventListener(BatchSelectorDeletedEvent.class)
    public void onSelectorDeleted(final BatchSelectorDeletedEvent event) {
        List<RuleDO> ruleDOList = ruleMapper.findBySelectorIds(event.getDeletedIds());
        final List<String> ruleIds = map(ruleDOList, RuleDO::getId);
        if (CollectionUtils.isNotEmpty(ruleDOList)) {
            final int deleteCount = ruleMapper.deleteByIds(ruleIds);
            ruleConditionMapper.deleteByRuleIds(ruleIds);
            if (deleteCount > 0) {
                ruleEventPublisher.onDeleted(ruleDOList, event);
            }
        }
    }
    
    private void addCondition(final RuleDO ruleDO, final List<RuleConditionDTO> ruleConditions) {
        for (RuleConditionDTO ruleCondition : ruleConditions) {
            ruleCondition.setRuleId(ruleDO.getId());
            ruleConditionMapper.insertSelective(RuleConditionDO.buildRuleConditionDO(ruleCondition));
        }
    }
    
    private List<RuleData> buildRuleDataList(final List<RuleDO> ruleDOList) {
        
        if (CollectionUtils.isEmpty(ruleDOList)) {
            return new ArrayList<>();
        }
        Map<String, String> ruleDOMap = ruleDOList.stream()
                .filter(Objects::nonNull)
                .collect(Collectors.toMap(RuleDO::getId, RuleDO::getSelectorId, (selectorId1, selectorId2) -> selectorId1));
        
        Map<String, String> pluginIdMap = Optional.ofNullable(selectorMapper.selectByIdSet(new HashSet<>(ruleDOMap.values()))).orElseGet(ArrayList::new)
                .stream()
                .filter(Objects::nonNull)
                .collect(Collectors.toMap(SelectorDO::getId, SelectorDO::getPluginId, (value1, value2) -> value1));
        
        Map<String, PluginDO> pluginDOMap = Optional.ofNullable(pluginMapper.selectByIds(new ArrayList<>(pluginIdMap.values())))
                .orElseGet(ArrayList::new)
                .stream()
                .filter(Objects::nonNull)
                .collect(Collectors.toMap(PluginDO::getId, Function.identity(), (value1, value2) -> value1));
        
        Map<String, List<ConditionData>> conditionMap = Optional.ofNullable(ruleConditionMapper.selectByRuleIdSet(ruleDOMap.keySet()))
                .orElseGet(ArrayList::new)
                .stream()
                .filter(Objects::nonNull)
                .collect(Collectors.toMap(RuleConditionDO::getRuleId, ruleConditionDO -> ListUtil.list(ConditionTransfer.INSTANCE.mapToRuleDO(ruleConditionDO)), ListUtil::merge));
        
        return ruleDOList.stream()
                .filter(Objects::nonNull)
                .map(ruleDO -> {
                    String ruleId = ruleDO.getId();
                    List<ConditionData> conditions = conditionMap.get(ruleId);
                    if (CollectionUtils.isNotEmpty(conditions)) {
                        String selectorId = ruleDO.getSelectorId();
                        String pluginId = pluginIdMap.get(selectorId);
                        if (StringUtils.isNotEmpty(pluginId)) {
                            PluginDO pluginDO = pluginDOMap.get(pluginId);
                            if (Objects.nonNull(pluginDO)) {
                                return RuleDO.transFrom(ruleDO, pluginDO.getName(), conditions);
                            }
                        }
                    }
                    return null;
                })
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
    }
}
