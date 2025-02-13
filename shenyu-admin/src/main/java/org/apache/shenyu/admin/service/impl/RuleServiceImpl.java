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

import com.google.common.collect.Lists;
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
import org.apache.shenyu.admin.model.event.rule.RuleCreatedEvent;
import org.apache.shenyu.admin.model.event.selector.BatchSelectorDeletedEvent;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.RuleConditionQuery;
import org.apache.shenyu.admin.model.query.RuleQuery;
import org.apache.shenyu.admin.model.query.RuleQueryCondition;
import org.apache.shenyu.admin.model.result.ConfigImportResult;
import org.apache.shenyu.admin.model.vo.RuleConditionVO;
import org.apache.shenyu.admin.model.vo.RuleVO;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.configs.ConfigsImportContext;
import org.apache.shenyu.admin.service.publish.RuleEventPublisher;
import org.apache.shenyu.admin.transfer.ConditionTransfer;
import org.apache.shenyu.admin.utils.Assert;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.common.utils.ListUtil;
import org.apache.shenyu.common.utils.UUIDUtils;
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
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.apache.shenyu.common.utils.ListUtil.map;

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
        ruleEventPublisher.publish(new RuleCreatedEvent(ruleDO, ruleDTO.getNamespaceId()));
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
        List<RuleConditionDTO> beforeCondition = beforeRuleCondition.stream().map(ruleConditionDO ->
                RuleConditionDTO.builder()
                        .ruleId(ruleConditionDO.getRuleId())
                        .operator(ruleConditionDO.getOperator())
                        .paramName(ruleConditionDO.getParamName())
                        .paramType(ruleConditionDO.getParamType())
                        .paramValue(ruleConditionDO.getParamValue())
                        .build()).collect(Collectors.toList());
        List<RuleConditionDTO> currentCondition = ruleDTO.getRuleConditions().stream().map(ruleConditionDTO ->
                RuleConditionDTO.builder()
                        .ruleId(ruleConditionDTO.getRuleId())
                        .operator(ruleConditionDTO.getOperator())
                        .paramName(ruleConditionDTO.getParamName())
                        .paramType(ruleConditionDTO.getParamType())
                        .paramValue(ruleConditionDTO.getParamValue())
                        .build()).collect(Collectors.toList());
        if (CollectionUtils.isEqualCollection(beforeCondition, currentCondition)) {
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
     * find rule by id and namespaceId.
     *
     * @param id primary key.
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
    public List<RuleData> listAllByNamespaceId(final String namespaceId) {
        return this.buildRuleDataList(ruleMapper.selectAllByNamespaceId(namespaceId));
    }

    @Override
    public List<RuleVO> listAllData() {
        return this.buildRuleVOList(ruleMapper.selectAll());
    }
    
    @Override
    public List<RuleVO> listAllDataByNamespaceId(final String namespaceId) {
        return this.buildRuleVOList(ruleMapper.selectAllByNamespaceId(namespaceId));
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

    @Override
    public RuleDO findBySelectorIdAndName(final String selectorId, final String name) {
        return ruleMapper.findBySelectorIdAndName(selectorId, name);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ConfigImportResult importData(final List<RuleDTO> ruleList) {
        if (CollectionUtils.isEmpty(ruleList)) {
            return ConfigImportResult.success();
        }

        Map<String, List<RuleDO>> selectorRuleMap = ruleMapper
                .selectAll()
                .stream()
                .collect(Collectors.groupingBy(RuleDO::getSelectorId));

        int successCount = 0;
        StringBuilder errorMsgBuilder = new StringBuilder();
        for (RuleDTO ruleDTO : ruleList) {
            String selectorId = ruleDTO.getSelectorId();
            String ruleName = ruleDTO.getName();
            Set<String> existRuleNameSet = selectorRuleMap
                    .getOrDefault(selectorId, Lists.newArrayList())
                    .stream()
                    .map(RuleDO::getName)
                    .collect(Collectors.toSet());

            if (existRuleNameSet.contains(ruleName)) {
                errorMsgBuilder
                        .append(ruleName)
                        .append(",");
                continue;
            }
            RuleDO ruleDO = RuleDO.buildRuleDO(ruleDTO);
            final int ruleCount = ruleMapper.insertSelective(ruleDO);
            addCondition(ruleDO, ruleDTO.getRuleConditions());
            if (ruleCount > 0) {
                successCount++;
            }
        }
        if (StringUtils.isNotEmpty(errorMsgBuilder)) {
            errorMsgBuilder.setLength(errorMsgBuilder.length() - 1);
            return ConfigImportResult
                    .fail(successCount, "import fail rule: " + errorMsgBuilder);
        }
        return ConfigImportResult.success(successCount);
    }
    
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ConfigImportResult importData(final String namespace, final List<RuleDTO> ruleList, final ConfigsImportContext context) {
        if (CollectionUtils.isEmpty(ruleList)) {
            return ConfigImportResult.success();
        }
        Map<String, String> selectorIdMapping = context.getSelectorIdMapping();
        Map<String, List<RuleDO>> selectorRuleMap = ruleMapper
                .selectAllByNamespaceId(namespace)
                .stream()
                .collect(Collectors.groupingBy(RuleDO::getSelectorId));
        
        int successCount = 0;
        StringBuilder errorMsgBuilder = new StringBuilder();
        for (RuleDTO ruleDTO : ruleList) {
            String selectorId = ruleDTO.getSelectorId();
            String ruleName = ruleDTO.getName();

            String newSelectorId = selectorIdMapping.get(selectorId);
            if (Objects.isNull(newSelectorId)) {
                errorMsgBuilder
                        .append(ruleName)
                        .append(",");
                continue;
            }
            Set<String> existRuleNameSet = selectorRuleMap
                    .getOrDefault(newSelectorId, Lists.newArrayList())
                    .stream()
                    .map(RuleDO::getName)
                    .collect(Collectors.toSet());
            
            if (existRuleNameSet.contains(ruleName)) {
                errorMsgBuilder
                        .append(ruleName)
                        .append(",");
                continue;
            }
            ruleDTO.setNamespaceId(namespace);
            ruleDTO.setSelectorId(newSelectorId);
            String ruleId = UUIDUtils.getInstance().generateShortUuid();
            ruleDTO.setId(ruleId);
            RuleDO ruleDO = RuleDO.buildRuleDO(ruleDTO);
            final int ruleCount = ruleMapper.insertSelective(ruleDO);
            Optional.ofNullable(ruleDTO.getRuleConditions())
                            .orElse(Collections.emptyList()).forEach(c -> {
                                c.setRuleId(ruleId);
                                c.setId(null);
                            });
            addCondition(ruleDO, ruleDTO.getRuleConditions());
            if (ruleCount > 0) {
                successCount++;
            }
        }
        if (StringUtils.isNotEmpty(errorMsgBuilder)) {
            errorMsgBuilder.setLength(errorMsgBuilder.length() - 1);
            return ConfigImportResult
                    .fail(successCount, "import fail rule: " + errorMsgBuilder);
        }
        return ConfigImportResult.success(successCount);
    }
    
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean enabledByIdsAndNamespaceId(final List<String> ids, final Boolean enabled, final String namespaceId) {
        ids.forEach(id -> {
            RuleDO ruleDO = ruleMapper.selectById(id);
            RuleDO before = JsonUtils.jsonToObject(JsonUtils.toJson(ruleDO), RuleDO.class);
            ruleDO.setEnabled(enabled);
            if (ruleMapper.updateEnable(id, enabled) > 0) {
                List<RuleConditionDO> conditionList = ruleConditionMapper.selectByQuery(new RuleConditionQuery(ruleDO.getId()));
                List<RuleConditionDTO> conditions = conditionList.stream().map(item ->
                        RuleConditionDTO.builder()
                                .ruleId(item.getRuleId())
                                .id(item.getId())
                                .operator(item.getOperator())
                                .paramName(item.getParamName())
                                .paramValue(item.getParamValue())
                                .paramType(item.getParamType())
                                .build()).toList();
                ruleEventPublisher.onUpdated(ruleDO, before, conditions, Collections.emptyList());
            }
        });
        return Boolean.TRUE;
    }

    /**
     * delete rules by ids and namespaceId.
     *
     * @param ids primary key.
     * @return rows
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public int deleteByIdsAndNamespaceId(final List<String> ids, final String namespaceId) {
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

    private List<RuleVO> buildRuleVOList(final List<RuleDO> ruleDOList) {

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

        Map<String, List<RuleConditionVO>> conditionMap = Optional.ofNullable(ruleConditionMapper.selectByRuleIdSet(ruleDOMap.keySet()))
                .orElseGet(ArrayList::new)
                .stream()
                .filter(Objects::nonNull)
                .collect(Collectors.toMap(RuleConditionDO::getRuleId, ruleConditionDO -> ListUtil.list(RuleConditionVO.buildRuleConditionVO(ruleConditionDO)), ListUtil::merge));

        return ruleDOList.stream()
                .filter(Objects::nonNull)
                .map(ruleDO -> {
                    String ruleId = ruleDO.getId();
                    List<RuleConditionVO> conditions = conditionMap.get(ruleId);
                    if (CollectionUtils.isNotEmpty(conditions)) {
                        String selectorId = ruleDO.getSelectorId();
                        String pluginId = pluginIdMap.get(selectorId);
                        if (StringUtils.isNotEmpty(pluginId)) {
                            PluginDO pluginDO = pluginDOMap.get(pluginId);
                            if (Objects.nonNull(pluginDO)) {
                                return RuleVO.buildRuleVO(ruleDO, conditions);
                            }
                        }
                    }
                    return null;
                })
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
    }
}
