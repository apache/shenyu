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
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.aspect.annotation.DataPermission;
import org.apache.shenyu.admin.aspect.annotation.Pageable;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.DataPermissionMapper;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.RuleConditionMapper;
import org.apache.shenyu.admin.mapper.RuleMapper;
import org.apache.shenyu.admin.mapper.SelectorConditionMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.DataPermissionDTO;
import org.apache.shenyu.admin.model.dto.SelectorConditionDTO;
import org.apache.shenyu.admin.model.dto.SelectorDTO;
import org.apache.shenyu.admin.model.entity.BaseDO;
import org.apache.shenyu.admin.model.entity.DataPermissionDO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.entity.SelectorConditionDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.event.plugin.BatchPluginDeletedEvent;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.SelectorConditionQuery;
import org.apache.shenyu.admin.model.query.SelectorQuery;
import org.apache.shenyu.admin.model.query.SelectorQueryCondition;
import org.apache.shenyu.admin.model.vo.SelectorConditionVO;
import org.apache.shenyu.admin.model.vo.SelectorVO;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.service.publish.SelectorEventPublisher;
import org.apache.shenyu.admin.transfer.ConditionTransfer;
import org.apache.shenyu.admin.utils.CommonUpstreamUtils;
import org.apache.shenyu.admin.utils.JwtUtils;
import org.apache.shenyu.admin.utils.ListUtil;
import org.apache.shenyu.admin.utils.SelectorUtil;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.common.utils.ContextPathUtils;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.springframework.context.ApplicationEventPublisher;
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
import java.util.stream.Collectors;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.SelectorService}.
 */
@Service
public class SelectorServiceImpl implements SelectorService {
    
    private final SelectorMapper selectorMapper;
    
    private final SelectorConditionMapper selectorConditionMapper;
    
    private final PluginMapper pluginMapper;
    
    private final RuleMapper ruleMapper;
    
    private final RuleConditionMapper ruleConditionMapper;
    
    private final ApplicationEventPublisher eventPublisher;
    
    private final DataPermissionMapper dataPermissionMapper;
    
    private final UpstreamCheckService upstreamCheckService;
    
    private final SelectorEventPublisher selectorEventPublisher;
    
    public SelectorServiceImpl(final SelectorMapper selectorMapper,
                               final SelectorConditionMapper selectorConditionMapper,
                               final PluginMapper pluginMapper,
                               final RuleMapper ruleMapper,
                               final RuleConditionMapper ruleConditionMapper,
                               final ApplicationEventPublisher eventPublisher,
                               final DataPermissionMapper dataPermissionMapper,
                               final UpstreamCheckService upstreamCheckService,
                               final SelectorEventPublisher selectorEventPublisher) {
        this.selectorMapper = selectorMapper;
        this.selectorConditionMapper = selectorConditionMapper;
        this.pluginMapper = pluginMapper;
        this.ruleMapper = ruleMapper;
        this.ruleConditionMapper = ruleConditionMapper;
        this.eventPublisher = eventPublisher;
        this.dataPermissionMapper = dataPermissionMapper;
        this.upstreamCheckService = upstreamCheckService;
        this.selectorEventPublisher = selectorEventPublisher;
    }
    
    @Override
    public List<SelectorVO> searchByCondition(final SelectorQueryCondition condition) {
        condition.init();
        final List<SelectorVO> list = selectorMapper.selectByCondition(condition);
        for (SelectorVO selector : list) {
            selector.setMatchModeName(MatchModeEnum.getMatchModeByCode(selector.getMatchMode()));
            selector.setTypeName(SelectorTypeEnum.getSelectorTypeByCode(selector.getType()));
        }
        return list;
    }
    
    @Override
    public String registerDefault(final SelectorDTO selectorDTO) {
        SelectorDO selectorDO = SelectorDO.buildSelectorDO(selectorDTO);
        if (StringUtils.isEmpty(selectorDTO.getId())) {
            selectorMapper.insertSelective(selectorDO);
            createCondition(selectorDO.getId(), selectorDTO.getSelectorConditions());
        }
        publishEvent(selectorDO, selectorDTO.getSelectorConditions());
        return selectorDO.getId();
    }
    
    @Override
    public String registerDefault(final MetaDataRegisterDTO dto, final String pluginName, final String selectorHandler) {
        String contextPath = ContextPathUtils.buildContextPath(dto.getContextPath(), dto.getAppName());
        SelectorDO selectorDO = findByNameAndPluginName(contextPath, pluginName);
        if (Objects.isNull(selectorDO)) {
            SelectorDTO selectorDTO = SelectorUtil.buildSelectorDTO(contextPath, pluginMapper.selectByName(pluginName).getId());
            selectorDTO.setHandle(selectorHandler);
            return registerDefault(selectorDTO);
        }
        return selectorDO.getId();
    }
    
    /**
     * create or update selector.
     *
     * @param selectorDTO {@linkplain SelectorDTO}
     * @return rows
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public int createOrUpdate(final SelectorDTO selectorDTO) {
        return SelectorService.super.createOrUpdate(selectorDTO);
    }
    
    @Override
    public int create(final SelectorDTO selectorDTO) {
        SelectorDO selectorDO = SelectorDO.buildSelectorDO(selectorDTO);
        int selectorCount = selectorMapper.insertSelective(selectorDO);
        createCondition(selectorDO.getId(), selectorDTO.getSelectorConditions());
        initSelectorPermission(selectorDO);
        publishEvent(selectorDO, selectorDTO.getSelectorConditions());
        if (selectorCount > 0) {
            selectorEventPublisher.onCreated(selectorDO);
        }
        updateDivideUpstream(selectorDO);
        return selectorCount;
        
    }
    
    @Override
    public int update(final SelectorDTO selectorDTO) {
        final SelectorDO before = selectorMapper.selectById(selectorDTO.getId());
        SelectorDO selectorDO = SelectorDO.buildSelectorDO(selectorDTO);
        int selectorCount = selectorMapper.updateSelective(selectorDO);
        //delete rule condition then add
        selectorConditionMapper.deleteByQuery(new SelectorConditionQuery(selectorDO.getId()));
        createCondition(selectorDO.getId(), selectorDTO.getSelectorConditions());
        publishEvent(selectorDO, selectorDTO.getSelectorConditions());
        if (selectorCount > 0) {
            selectorEventPublisher.onUpdated(selectorDO, before);
        }
        updateDivideUpstream(selectorDO);
        return selectorCount;
    }
    
    @Override
    public int updateSelective(final SelectorDO selectorDO) {
        return selectorMapper.updateSelective(selectorDO);
    }
    
    /**
     * delete selectors.
     *
     * @param ids primary key.
     * @return rows
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public int delete(final List<String> ids) {
        
        if (CollectionUtils.isEmpty(ids)) {
            return 0;
        }
        Set<String> idSet = new HashSet<>(ids);
        List<SelectorDO> selectorDOList = selectorMapper.selectByIdSet(idSet);
        if (CollectionUtils.isNotEmpty(selectorDOList)) {
            Map<String, SelectorDO> selectorMap = ListUtil.toMap(selectorDOList, SelectorDO::getId);
            List<String> pluginIdList = ListUtil.map(selectorMap.values(), SelectorDO::getPluginId);
            List<PluginDO> pluginDOList = pluginMapper.selectByIds(pluginIdList);
            
            if (CollectionUtils.isNotEmpty(pluginDOList)) {
                Map<String, String> pluginMap = ListUtil.toMap(pluginDOList, PluginDO::getId, PluginDO::getName);
                if (pluginMap.size() == pluginIdList.size()) {
                    selectorMapper.deleteByIds(ids);
                    selectorConditionMapper.deleteBySelectorIds(ids);
                    dataPermissionMapper.deleteByDataIdList(ids);
                    
                    List<SelectorData> selectorDataList = selectorMap.values().stream()
                            .map(selectorDO -> {
                                String pluginName = pluginMap.get(selectorDO.getPluginId());
                                if (pluginName.equals(PluginEnum.DIVIDE.getName())) {
                                    UpstreamCheckService.removeByKey(selectorDO.getId());
                                }
                                return SelectorDO.transFrom(selectorDO, pluginName, null);
                            }).collect(Collectors.toList());
                    selectorEventPublisher.onDeleted(selectorDOList, selectorDataList);
                    deleteRule(ids, selectorMap, pluginMap);
                }
            }
        }
        return idSet.size();
    }
    
    /**
     * find selector by id.
     *
     * @param id primary key.
     * @return {@link SelectorVO}
     */
    @Override
    public SelectorVO findById(final String id) {
        return SelectorVO.buildSelectorVO(selectorMapper.selectById(id), ListUtil.map(selectorConditionMapper.selectByQuery(new SelectorConditionQuery(id)), SelectorConditionVO::buildSelectorConditionVO));
    }
    
    @Override
    public SelectorDO findByName(final String name) {
        return selectorMapper.selectByName(name);
    }
    
    /**
     * Find by name and plugin id selector do.
     *
     * @param name       the name
     * @param pluginName the plugin name
     * @return the selector do
     */
    @Override
    public SelectorDO findByNameAndPluginName(final String name, final String pluginName) {
        PluginDO pluginDO = pluginMapper.selectByName(pluginName);
        return selectorMapper.findByNameAndPluginId(name, pluginDO.getId());
    }
    
    @Override
    public SelectorData buildByName(final String name) {
        return buildSelectorData(selectorMapper.selectByName(name));
    }
    
    /**
     * Build by name selector data.
     *
     * @param name       the name
     * @param pluginName the plugin name
     * @return the selector data
     */
    @Override
    public SelectorData buildByName(final String name, final String pluginName) {
        return buildSelectorData(findByNameAndPluginName(name, pluginName));
    }
    
    /**
     * find page of selector by query.
     *
     * @param selectorQuery {@linkplain SelectorQuery}
     * @return {@linkplain CommonPager}
     */
    @Override
    @DataPermission(dataType = AdminConstants.DATA_PERMISSION_SELECTOR)
    @Pageable
    public CommonPager<SelectorVO> listByPage(final SelectorQuery selectorQuery) {
        return PageResultUtils.result(selectorQuery.getPageParameter(), () -> selectorMapper.selectByQuery(selectorQuery)
                .stream()
                .map(SelectorVO::buildSelectorVO)
                .collect(Collectors.toList()));
    }
    
    @Override
    public List<SelectorData> findByPluginId(final String pluginId) {
        return this.buildSelectorDataList(selectorMapper.findByPluginId(pluginId));
    }
    
    @Override
    public List<SelectorData> listAll() {
        return this.buildSelectorDataList(selectorMapper.selectAll());
    }
    
    /**
     * the plugin delete, synchronously delete selectors.
     *
     * @param event event
     */
    @EventListener(value = BatchPluginDeletedEvent.class)
    public void onPluginDeleted(final BatchPluginDeletedEvent event) {
        final List<SelectorDO> selectors = this.selectorMapper.findByPluginIds(event.getDeletedPluginIds());
        if (CollectionUtils.isNotEmpty(selectors)) {
            final List<String> selectorIds = ListUtil.map(selectors, BaseDO::getId);
            final int count = selectorMapper.deleteByIds(selectorIds);
            // delete all selector conditions
            this.selectorConditionMapper.deleteBySelectorIds(selectorIds);
            deleteRefRule(selectorIds);
            if (count > 0) {
                selectorEventPublisher.onDeleted(selectors);
            }
        }
    }
    
    private void deleteRefRule(final List<String> selectorIds) {
        // todo: delete rule will move rule service
        // delete all rules
        final List<String> ruleIds = ListUtil.map(ruleMapper.findBySelectorIds(selectorIds), RuleDO::getId);
        if (CollectionUtils.isNotEmpty(ruleIds)) {
            this.ruleMapper.deleteByIds(ruleIds);
            // delete all rule conditions
            this.ruleConditionMapper.deleteByRuleIds(ruleIds);
        }
    }
    
    private void deleteRule(final List<String> ids, final Map<String, SelectorDO> selectorDOMap, final Map<String, String> pluginMap) {
        // todo: delete rule will move rule service
        List<RuleDO> ruleDOList = ruleMapper.findBySelectorIds(ids);
        if (CollectionUtils.isNotEmpty(ruleDOList)) {
            List<String> ruleIdList = new ArrayList<>();
            List<RuleData> ruleDataList = ruleDOList.stream()
                    .filter(Objects::nonNull)
                    .map(ruleDO -> {
                        ruleIdList.add(ruleDO.getId());
                        SelectorDO selectorDO = selectorDOMap.get(ruleDO.getSelectorId());
                        String pluginName = pluginMap.get(selectorDO.getPluginId());
                        return RuleDO.transFrom(ruleDO, pluginName, null);
                    })
                    .collect(Collectors.toList());
            ruleMapper.deleteByIds(ruleIdList);
            ruleConditionMapper.deleteByRuleIds(ruleIdList);
            eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.RULE, DataEventTypeEnum.DELETE, ruleDataList));
        }
    }
    
    private void createCondition(final String selectorId, final List<SelectorConditionDTO> selectorConditions) {
        for (SelectorConditionDTO condition : selectorConditions) {
            condition.setSelectorId(selectorId);
            selectorConditionMapper.insertSelective(SelectorConditionDO.buildSelectorConditionDO(condition));
        }
    }
    
    private void initSelectorPermission(final SelectorDO selectorDO) {
        // todo add permission will move permission service
        // check selector add
        if (Boolean.TRUE.equals(dataPermissionMapper.existed(JwtUtils.getUserInfo().getUserId()))) {
            DataPermissionDTO dataPermissionDTO = new DataPermissionDTO();
            dataPermissionDTO.setUserId(JwtUtils.getUserInfo().getUserId());
            dataPermissionDTO.setDataId(selectorDO.getId());
            dataPermissionDTO.setDataType(AdminConstants.SELECTOR_DATA_TYPE);
            dataPermissionMapper.insertSelective(DataPermissionDO.buildPermissionDO(dataPermissionDTO));
        }
    }
    
    private void publishEvent(final SelectorDO selectorDO, final List<SelectorConditionDTO> selectorConditions) {
        PluginDO pluginDO = pluginMapper.selectById(selectorDO.getPluginId());
        List<ConditionData> conditionDataList = ListUtil.map(selectorConditions, ConditionTransfer.INSTANCE::mapToSelectorDTO);
        // publish change event.
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.SELECTOR, DataEventTypeEnum.UPDATE,
                Collections.singletonList(SelectorDO.transFrom(selectorDO, pluginDO.getName(), conditionDataList))));
    }
    
    private SelectorData buildSelectorData(final SelectorDO selectorDO) {
        // find conditions
        List<ConditionData> conditionDataList = ConditionTransfer.INSTANCE.mapToSelectorDOS(
                selectorConditionMapper.selectByQuery(new SelectorConditionQuery(selectorDO.getId())));
        PluginDO pluginDO = pluginMapper.selectById(selectorDO.getPluginId());
        if (Objects.isNull(pluginDO)) {
            return null;
        }
        return SelectorDO.transFrom(selectorDO, pluginDO.getName(), conditionDataList);
    }
    
    private List<SelectorData> buildSelectorDataList(final List<SelectorDO> selectorDOList) {
        
        Map<String, String> idMap = ListUtil.toMap(selectorDOList, SelectorDO::getId, SelectorDO::getPluginId);
        if (MapUtils.isEmpty(idMap)) {
            return new ArrayList<>();
        }
        Map<String, List<SelectorConditionDO>> selectorConditionMap = ListUtil.groupBy(selectorConditionMapper.selectBySelectorIds(idMap.keySet()), SelectorConditionDO::getSelectorId);
        
        Map<String, PluginDO> pluginDOMap = ListUtil.toMap(pluginMapper.selectByIds(Lists.newArrayList(idMap.values())), PluginDO::getId);
        
        return Optional.ofNullable(selectorDOList).orElseGet(ArrayList::new)
                .stream()
                .filter(Objects::nonNull)
                .map(selectorDO -> {
                    String id = selectorDO.getId();
                    String pluginId = selectorDO.getPluginId();
                    PluginDO pluginDO = pluginDOMap.get(pluginId);
                    if (Objects.isNull(pluginDO)) {
                        return null;
                    }
                    List<ConditionData> conditionDataList = ConditionTransfer.INSTANCE.mapToSelectorDOS(selectorConditionMap.get(id));
                    return SelectorDO.transFrom(selectorDO, pluginDO.getName(), conditionDataList);
                })
                .collect(Collectors.toList());
    }
    
    private void updateDivideUpstream(final SelectorDO selectorDO) {
        // todo update divide upstream will move upstream check service
        final PluginDO plugin = pluginMapper.selectById(selectorDO.getPluginId());
        List<DivideUpstream> existDivideUpstreams = SelectorUtil.buildDivideUpstream(selectorDO, plugin.getName());
        if (CollectionUtils.isNotEmpty(existDivideUpstreams)) {
            upstreamCheckService.replace(selectorDO.getId(), CommonUpstreamUtils.convertCommonUpstreamList(existDivideUpstreams));
        }
    }
    
}
