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
import org.apache.shenyu.admin.mapper.DataPermissionMapper;
import org.apache.shenyu.admin.mapper.NamespaceUserRelMapper;
import org.apache.shenyu.admin.mapper.RuleMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.DataPermissionDTO;
import org.apache.shenyu.admin.model.entity.DataPermissionDO;
import org.apache.shenyu.admin.model.entity.NamespaceUserRelDO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.event.rule.RuleCreatedEvent;
import org.apache.shenyu.admin.model.event.selector.BatchSelectorDeletedEvent;
import org.apache.shenyu.admin.model.event.selector.SelectorCreatedEvent;
import org.apache.shenyu.admin.model.event.user.BatchUserDeletedEvent;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.RuleQuery;
import org.apache.shenyu.admin.model.query.SelectorQuery;
import org.apache.shenyu.admin.model.vo.DataPermissionPageVO;
import org.apache.shenyu.admin.service.DataPermissionService;
import org.apache.shenyu.admin.utils.JwtUtils;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.enums.AdminDataPermissionTypeEnum;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.DataPermissionService}.
 */
@Service
public class DataPermissionServiceImpl implements DataPermissionService {
    
    private final DataPermissionMapper dataPermissionMapper;
    
    private final RuleMapper ruleMapper;
    
    private final SelectorMapper selectorMapper;
    
    private final NamespaceUserRelMapper namespaceUserRelMapper;
    
    public DataPermissionServiceImpl(final DataPermissionMapper dataPermissionMapper,
                                     final RuleMapper ruleMapper,
                                     final SelectorMapper selectorMapper,
                                     final NamespaceUserRelMapper namespaceUserRelMapper) {
        this.dataPermissionMapper = dataPermissionMapper;
        this.ruleMapper = ruleMapper;
        this.selectorMapper = selectorMapper;
        this.namespaceUserRelMapper = namespaceUserRelMapper;
    }
    
    /**
     * Get all data permissions by user id.
     *
     * @param userId user id
     * @return list of {@linkplain DataPermissionDO}
     */
    @Override
    public List<DataPermissionDO> getUserDataPermissionList(final String userId) {
        return dataPermissionMapper.listByUserId(userId);
    }
    
    /**
     * get data permission by user id.
     *
     * @param userId user id
     * @return List
     */
    @Override
    public List<String> getDataPermission(final String userId) {
        return getUserDataPermissionList(userId).stream().map(DataPermissionDO::getDataId).collect(Collectors.toList());
    }
    
    /**
     * Create data permissions.
     *
     * @param dataPermissionDTO {@linkplain DataPermissionDTO}
     * @return int
     */
    @Override
    public int createSelector(final DataPermissionDTO dataPermissionDTO) {
        
        List<DataPermissionDO> allDOList = new LinkedList<>();
        dataPermissionDTO.setDataType(AdminDataPermissionTypeEnum.SELECTOR.ordinal());
        allDOList.add(DataPermissionDO.buildPermissionDO(dataPermissionDTO));
        
        List<DataPermissionDO> allRuleList = ruleMapper.findBySelectorId(dataPermissionDTO.getDataId())
                .stream()
                .filter(Objects::nonNull)
                .map(ruleDO -> DataPermissionDO.buildCreatePermissionDO(ruleDO.getId(),
                        dataPermissionDTO.getUserId(), AdminDataPermissionTypeEnum.RULE.ordinal()))
                .collect(Collectors.toList());
        
        if (CollectionUtils.isNotEmpty(allRuleList)) {
            allDOList.addAll(allRuleList);
        }
        
        return dataPermissionMapper.insertBatch(allDOList);
    }
    
    
    /**
     * deleteSelector data permission.
     *
     * @param dataPermissionDTO {@linkplain DataPermissionDTO}
     * @return int  effect rows
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public int deleteSelector(final DataPermissionDTO dataPermissionDTO) {
        List<String> allRuleIds = ruleMapper.findBySelectorId(dataPermissionDTO.getDataId())
                .stream()
                .filter(Objects::nonNull)
                .map(RuleDO::getId)
                .collect(Collectors.toList());
        
        int count = 0;
        if (CollectionUtils.isNotEmpty(allRuleIds)) {
            count = dataPermissionMapper.deleteByDataIdsAndUserId(allRuleIds, dataPermissionDTO.getUserId(),
                    AdminDataPermissionTypeEnum.RULE.ordinal());
        }
        
        count += dataPermissionMapper.deleteByUniqueKey(dataPermissionDTO.getDataId(), dataPermissionDTO.getUserId(),
                AdminDataPermissionTypeEnum.SELECTOR.ordinal());
        
        return count;
    }
    
    /**
     * list of selectors.
     *
     * @param selectorQuery {@linkplain SelectorQuery}
     * @param userId        user id
     * @return {@linkplain CommonPager}
     */
    @Override
    public CommonPager<DataPermissionPageVO> listSelectorsByPage(final SelectorQuery selectorQuery, final String userId) {
        int totalCount = selectorMapper.countByQuery(selectorQuery);
        
        List<DataPermissionPageVO> selectorList = Collections.emptyList();
        if (totalCount > 0) {
            Supplier<Stream<SelectorDO>> selectorDOStreamSupplier = () -> selectorMapper.selectByQuery(selectorQuery).stream();
            List<String> selectorIds = selectorDOStreamSupplier.get().map(SelectorDO::getId).collect(Collectors.toList());

            Set<String> hasDataPermissionSelectorIds = new HashSet<>();
            if (!selectorIds.isEmpty()) {
                hasDataPermissionSelectorIds.addAll(dataPermissionMapper.selectDataIds(selectorIds,
                        userId, AdminDataPermissionTypeEnum.SELECTOR.ordinal()));
            }

            selectorList = selectorDOStreamSupplier.get().map(selectorDO -> {
                boolean isChecked = hasDataPermissionSelectorIds.contains(selectorDO.getId());
                return DataPermissionPageVO.buildPageVOBySelector(selectorDO, isChecked);
            }).collect(Collectors.toList());
        }
        
        List<DataPermissionPageVO> finalSelectorList = selectorList;
        return PageResultUtils.result(selectorQuery.getPageParameter(), () -> totalCount, () -> finalSelectorList);
    }
    
    /**
     * list of rules.
     *
     * @param ruleQuery {@linkplain RuleQuery}
     * @param userId    user id
     * @return {@linkplain CommonPager}
     */
    @Override
    public CommonPager<DataPermissionPageVO> listRulesByPage(final RuleQuery ruleQuery, final String userId) {
        int totalCount = ruleMapper.countByQuery(ruleQuery);
        
        List<DataPermissionPageVO> selectorList = Collections.emptyList();
        if (totalCount > 0) {
            Supplier<Stream<RuleDO>> ruleDOStreamSupplier = () -> ruleMapper.selectByQuery(ruleQuery).stream();
            List<String> ruleIds = ruleDOStreamSupplier.get().map(RuleDO::getId).collect(Collectors.toList());
            
            Set<String> hasDataPermissionRuleIds = new HashSet<>(dataPermissionMapper.selectDataIds(ruleIds, userId,
                    AdminDataPermissionTypeEnum.RULE.ordinal()));
            
            selectorList = ruleDOStreamSupplier.get().map(ruleDO -> {
                boolean isChecked = hasDataPermissionRuleIds.contains(ruleDO.getId());
                return DataPermissionPageVO.buildPageVOByRule(ruleDO, isChecked);
            }).collect(Collectors.toList());
        }
        
        List<DataPermissionPageVO> finalSelectorList = selectorList;
        return PageResultUtils.result(ruleQuery.getPageParameter(), () -> totalCount, () -> finalSelectorList);
    }
    
    /**
     * create rule data permission.
     *
     * @param dataPermissionDTO {@linkplain DataPermissionDTO}
     * @return int, effect rows count
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public int createRule(final DataPermissionDTO dataPermissionDTO) {
        RuleDO ruleDO = ruleMapper.selectById(dataPermissionDTO.getDataId());
        if (Objects.isNull(ruleDO)) {
            return 0;
        }
        
        int count = 0;
        DataPermissionDO selectorDataPermissionDo = dataPermissionMapper.findOneByUniqueKey(ruleDO.getSelectorId(),
                dataPermissionDTO.getUserId(), AdminDataPermissionTypeEnum.SELECTOR.ordinal());
        if (Objects.isNull(selectorDataPermissionDo)) {
            DataPermissionDO selectorDataPermissionDO = DataPermissionDO.buildCreatePermissionDO(ruleDO.getSelectorId(),
                    dataPermissionDTO.getUserId(), AdminDataPermissionTypeEnum.SELECTOR.ordinal());
            count = dataPermissionMapper.insertSelective(selectorDataPermissionDO);
        }
        
        dataPermissionDTO.setDataType(AdminDataPermissionTypeEnum.RULE.ordinal());
        count += dataPermissionMapper.insertSelective(DataPermissionDO.buildPermissionDO(dataPermissionDTO));
        
        return count;
    }
    
    /**
     * delete rule data permission.
     *
     * @param dataPermissionDTO {@linkplain DataPermissionDTO}
     * @return effect rows count
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public int deleteRule(final DataPermissionDTO dataPermissionDTO) {
        return dataPermissionMapper.deleteByUniqueKey(dataPermissionDTO.getDataId(), dataPermissionDTO.getUserId(),
                AdminDataPermissionTypeEnum.RULE.ordinal());
    }
    
    /**
     * listen {@link SelectorCreatedEvent} add data permission.
     *
     * @param event event
     */
    @EventListener(SelectorCreatedEvent.class)
    public void onSelectorCreated(final SelectorCreatedEvent event) {
        // check selector add
        Boolean existed;
        try {
            existed = dataPermissionMapper.existed(JwtUtils.getUserInfo().getUserId());
        } catch (Exception e) {
            existed = false;
        }
        if (Boolean.TRUE.equals(existed)) {
            DataPermissionDTO dataPermissionDTO = new DataPermissionDTO();
            dataPermissionDTO.setUserId(JwtUtils.getUserInfo().getUserId());
            dataPermissionDTO.setDataId(event.getSelector().getId());
            dataPermissionDTO.setDataType(AdminConstants.SELECTOR_DATA_TYPE);
            dataPermissionMapper.insertSelective(DataPermissionDO.buildPermissionDO(dataPermissionDTO));
        } else {
            String namespaceId = event.getSelector().getNamespaceId();
            if (StringUtils.isNoneBlank(namespaceId)) {
                // support namespace
                List<NamespaceUserRelDO> namespaceUserRelDOList = namespaceUserRelMapper.selectListByNamespaceId(namespaceId);
                if (CollectionUtils.isNotEmpty(namespaceUserRelDOList)) {
                    namespaceUserRelDOList.forEach(namespaceUserRelDO -> {
                        DataPermissionDTO dataPermissionDTO = new DataPermissionDTO();
                        dataPermissionDTO.setUserId(namespaceUserRelDO.getUserId());
                        dataPermissionDTO.setDataId(event.getSelector().getId());
                        dataPermissionDTO.setDataType(AdminConstants.SELECTOR_DATA_TYPE);
                        dataPermissionMapper.insertSelective(DataPermissionDO.buildPermissionDO(dataPermissionDTO));
                    });
                }
            }
        }
    }
    
    /**
     * listen {@link RuleCreatedEvent} add data permission.
     *
     * @param event event
     */
    @EventListener(RuleCreatedEvent.class)
    public void onRuleCreated(final RuleCreatedEvent event) {
        // check rule add
        Boolean existed;
        try {
            existed = dataPermissionMapper.existed(JwtUtils.getUserInfo().getUserId());
        } catch (Exception e) {
            existed = false;
        }
        if (Boolean.TRUE.equals(existed)) {
            DataPermissionDTO dataPermissionDTO = new DataPermissionDTO();
            dataPermissionDTO.setUserId(JwtUtils.getUserInfo().getUserId());
            dataPermissionDTO.setDataId(event.getRule().getId());
            dataPermissionDTO.setDataType(AdminConstants.RULE_DATA_TYPE);
            dataPermissionMapper.insertSelective(DataPermissionDO.buildPermissionDO(dataPermissionDTO));
        } else {
            String namespaceId = event.getRule().getNamespaceId();
            if (StringUtils.isNoneBlank(namespaceId)) {
                // support namespace
                List<NamespaceUserRelDO> namespaceUserRelDOList = namespaceUserRelMapper.selectListByNamespaceId(namespaceId);
                if (CollectionUtils.isNotEmpty(namespaceUserRelDOList)) {
                    namespaceUserRelDOList.forEach(namespaceUserRelDO -> {
                        DataPermissionDTO dataPermissionDTO = new DataPermissionDTO();
                        dataPermissionDTO.setUserId(namespaceUserRelDO.getUserId());
                        dataPermissionDTO.setDataId(event.getRule().getId());
                        dataPermissionDTO.setDataType(AdminConstants.RULE_DATA_TYPE);
                        dataPermissionMapper.insertSelective(DataPermissionDO.buildPermissionDO(dataPermissionDTO));
                    });
                }
            }
        }
    }
    
    
    /**
     * listen {@link BatchSelectorDeletedEvent} delete data permission.
     *
     * @param event event
     */
    @EventListener(BatchSelectorDeletedEvent.class)
    public void onSelectorDeleted(final BatchSelectorDeletedEvent event) {
        // check selector delete
        dataPermissionMapper.deleteByDataIdList(event.getDeletedIds());
    }
    
    /**
     * listen {@link BatchUserDeletedEvent} delete data permission.
     *
     * @param event event
     */
    @EventListener(BatchUserDeletedEvent.class)
    public void onUserDeleted(final BatchUserDeletedEvent event) {
        dataPermissionMapper.deleteByUserIdList(event.getDeletedIds());
    }
}
