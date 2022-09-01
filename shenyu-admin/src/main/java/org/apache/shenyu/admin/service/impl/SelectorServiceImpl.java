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
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.SelectorConditionMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.SelectorConditionDTO;
import org.apache.shenyu.admin.model.dto.SelectorDTO;
import org.apache.shenyu.admin.model.entity.BaseDO;
import org.apache.shenyu.admin.model.entity.PluginDO;
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
import org.apache.shenyu.admin.utils.ListUtil;
import org.apache.shenyu.admin.utils.SelectorUtil;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.common.utils.ContextPathUtils;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.TreeSet;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.SelectorService}.
 * Maintain {@link SelectorDO} and {@link SelectorConditionDO} related data.
 */
@Service
public class SelectorServiceImpl implements SelectorService {
    
    private final SelectorMapper selectorMapper;
    
    private final SelectorConditionMapper selectorConditionMapper;
    
    private final PluginMapper pluginMapper;
    
    private final ApplicationEventPublisher eventPublisher;
    
    private final SelectorEventPublisher selectorEventPublisher;
    
    public SelectorServiceImpl(final SelectorMapper selectorMapper,
                               final SelectorConditionMapper selectorConditionMapper,
                               final PluginMapper pluginMapper,
                               final ApplicationEventPublisher eventPublisher,
                               final SelectorEventPublisher selectorEventPublisher) {
        this.selectorMapper = selectorMapper;
        this.selectorConditionMapper = selectorConditionMapper;
        this.pluginMapper = pluginMapper;
        this.eventPublisher = eventPublisher;
        this.selectorEventPublisher = selectorEventPublisher;
    }
    
    @Override
    public void doConditionPreProcessing(final SelectorQueryCondition condition) {
        if (SessionUtil.isAdmin()) {
            condition.setUserId(null);
        }
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
        final int selectorCount = selectorMapper.insertSelective(selectorDO);
        createCondition(selectorDO.getId(), selectorDTO.getSelectorConditions());
        publishEvent(selectorDO, selectorDTO.getSelectorConditions());
        if (selectorCount > 0) {
            selectorEventPublisher.onCreated(selectorDO);
        }
        return selectorCount;
        
    }
    
    @Override
    public int update(final SelectorDTO selectorDTO) {
        final SelectorDO before = selectorMapper.selectById(selectorDTO.getId());
        SelectorDO selectorDO = SelectorDO.buildSelectorDO(selectorDTO);
        final int selectorCount = selectorMapper.updateSelective(selectorDO);
        //delete rule condition then add
        selectorConditionMapper.deleteByQuery(new SelectorConditionQuery(selectorDO.getId()));
        createCondition(selectorDO.getId(), selectorDTO.getSelectorConditions());
        publishEvent(selectorDO, selectorDTO.getSelectorConditions());
        if (selectorCount > 0) {
            selectorEventPublisher.onUpdated(selectorDO, before);
        }
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
        final List<SelectorDO> selectors = selectorMapper.selectByIdSet(new TreeSet<>(ids));
        return deleteSelector(selectors, pluginMapper.selectByIds(ListUtil.map(selectors, SelectorDO::getPluginId)));
    }
    
    /**
     * find selector by id.
     *
     * @param id primary key.
     * @return {@link SelectorVO}
     */
    @Override
    public SelectorVO findById(final String id) {
        final List<SelectorConditionVO> conditions = ListUtil.map(selectorConditionMapper.selectByQuery(new SelectorConditionQuery(id)), SelectorConditionVO::buildSelectorConditionVO);
        return SelectorVO.buildSelectorVO(selectorMapper.selectById(id), conditions);
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
    public CommonPager<SelectorVO> listByPageWithPermission(final SelectorQuery selectorQuery) {
        return listByPage(selectorQuery);
    }
    
    @Override
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
        deleteSelector(selectorMapper.findByPluginIds(event.getDeletedPluginIds()), event.getPlugins());
    }
    
    private void createCondition(final String selectorId, final List<SelectorConditionDTO> selectorConditions) {
        for (SelectorConditionDTO condition : selectorConditions) {
            condition.setSelectorId(selectorId);
            selectorConditionMapper.insertSelective(SelectorConditionDO.buildSelectorConditionDO(condition));
        }
    }
    
    private int deleteSelector(final List<SelectorDO> selectors, final List<PluginDO> plugins) {
        if (CollectionUtils.isNotEmpty(selectors)) {
            final List<String> selectorIds = ListUtil.map(selectors, BaseDO::getId);
            final int count = selectorMapper.deleteByIds(selectorIds);
            // delete all selector conditions
            this.selectorConditionMapper.deleteBySelectorIds(selectorIds);
            if (count > 0) {
                selectorEventPublisher.onDeleted(selectors, plugins);
            }
            return count;
        }
        return selectors.size();
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
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
    }
    
}
