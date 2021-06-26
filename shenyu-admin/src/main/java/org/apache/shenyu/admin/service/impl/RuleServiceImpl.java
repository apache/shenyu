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

import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.aspect.annotation.DataPermission;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.DataPermissionMapper;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.RuleConditionMapper;
import org.apache.shenyu.admin.mapper.RuleMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.DataPermissionDTO;
import org.apache.shenyu.admin.model.dto.RuleConditionDTO;
import org.apache.shenyu.admin.model.dto.RuleDTO;
import org.apache.shenyu.admin.model.entity.DataPermissionDO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.RuleConditionDO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.RuleConditionQuery;
import org.apache.shenyu.admin.model.query.RuleQuery;
import org.apache.shenyu.admin.model.vo.RuleConditionVO;
import org.apache.shenyu.admin.model.vo.RuleVO;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.transfer.ConditionTransfer;
import org.apache.shenyu.admin.utils.JwtUtils;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.RuleService}.
 */
@RequiredArgsConstructor
@Service
public class RuleServiceImpl implements RuleService {

    private final RuleMapper ruleMapper;

    private final RuleConditionMapper ruleConditionMapper;

    private final SelectorMapper selectorMapper;

    private final PluginMapper pluginMapper;

    private final DataPermissionMapper dataPermissionMapper;

    private final ApplicationEventPublisher eventPublisher;

    @Override
    public String register(final RuleDTO ruleDTO, final String name, final boolean metaDataIsNull) {
        if (Objects.nonNull(ruleMapper.findByName(name)) || metaDataIsNull) {
            return "";
        }
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
            if (dataPermissionMapper.listByUserId(JwtUtils.getUserInfo().getUserId()).size() > 0) {
                DataPermissionDTO dataPermissionDTO = new DataPermissionDTO();
                dataPermissionDTO.setUserId(JwtUtils.getUserInfo().getUserId());
                dataPermissionDTO.setDataId(ruleDO.getId());
                dataPermissionDTO.setDataType(AdminConstants.RULE_DATA_TYPE);
                dataPermissionMapper.insertSelective(DataPermissionDO.buildPermissionDO(dataPermissionDTO));
            }
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
            final RuleDO ruleDO = ruleMapper.selectById(id);
            final SelectorDO selectorDO = selectorMapper.selectById(ruleDO.getSelectorId());
            final PluginDO pluginDO = pluginMapper.selectById(selectorDO.getPluginId());

            ruleMapper.delete(id);
            ruleConditionMapper.deleteByQuery(new RuleConditionQuery(id));
            dataPermissionMapper.deleteByDataId(id);

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
    @DataPermission(dataType = AdminConstants.DATA_PERMISSION_RULE)
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

    @Override
    public RuleDO findByName(final String name) {
        return ruleMapper.findByName(name);
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
