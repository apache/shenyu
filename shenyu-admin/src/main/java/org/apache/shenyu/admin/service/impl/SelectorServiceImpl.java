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
import org.apache.shenyu.admin.model.entity.DataPermissionDO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.entity.SelectorConditionDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.SelectorConditionQuery;
import org.apache.shenyu.admin.model.query.SelectorQuery;
import org.apache.shenyu.admin.model.vo.SelectorConditionVO;
import org.apache.shenyu.admin.model.vo.SelectorVO;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.transfer.ConditionTransfer;
import org.apache.shenyu.admin.utils.Assert;
import org.apache.shenyu.admin.utils.CommonUpstreamUtils;
import org.apache.shenyu.admin.utils.JwtUtils;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.dto.convert.selector.SpringCloudSelectorHandle;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.common.utils.ContextPathUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.springframework.context.ApplicationEventPublisher;
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
    
    public SelectorServiceImpl(final SelectorMapper selectorMapper,
                               final SelectorConditionMapper selectorConditionMapper,
                               final PluginMapper pluginMapper,
                               final RuleMapper ruleMapper,
                               final RuleConditionMapper ruleConditionMapper,
                               final ApplicationEventPublisher eventPublisher,
                               final DataPermissionMapper dataPermissionMapper,
                               final UpstreamCheckService upstreamCheckService) {
        this.selectorMapper = selectorMapper;
        this.selectorConditionMapper = selectorConditionMapper;
        this.pluginMapper = pluginMapper;
        this.ruleMapper = ruleMapper;
        this.ruleConditionMapper = ruleConditionMapper;
        this.eventPublisher = eventPublisher;
        this.dataPermissionMapper = dataPermissionMapper;
        this.upstreamCheckService = upstreamCheckService;
    }
    
    @Override
    public String registerDefault(final SelectorDTO selectorDTO) {
        SelectorDO selectorDO = SelectorDO.buildSelectorDO(selectorDTO);
        List<SelectorConditionDTO> selectorConditionDTOs = selectorDTO.getSelectorConditions();
        if (StringUtils.isEmpty(selectorDTO.getId())) {
            selectorMapper.insertSelective(selectorDO);
            selectorConditionDTOs.forEach(selectorConditionDTO -> {
                selectorConditionDTO.setSelectorId(selectorDO.getId());
                selectorConditionMapper.insertSelective(SelectorConditionDO.buildSelectorConditionDO(selectorConditionDTO));
            });
        }
        publishEvent(selectorDO, selectorConditionDTOs);
        return selectorDO.getId();
    }
    
    @Override
    public String registerDefault(final MetaDataRegisterDTO dto, final String pluginName, final String selectorHandler) {
        String contextPath = ContextPathUtils.buildContextPath(dto.getContextPath(), dto.getAppName());
        SelectorDO selectorDO = findByNameAndPluginName(contextPath, pluginName);
        if (Objects.isNull(selectorDO)) {
            return registerSelector(contextPath, pluginName, selectorHandler);
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
        if (Objects.equals(SelectorTypeEnum.CUSTOM_FLOW.getCode(), selectorDTO.getType())) {
            Assert.notNull(selectorDTO.getMatchMode(), "if type is custom, matchMode is not null");
            Assert.notEmpty(selectorDTO.getSelectorConditions(), "if type is custom, selectorConditions is not empty");
        }
        int selectorCount;
        SelectorDO selectorDO = SelectorDO.buildSelectorDO(selectorDTO);
        List<SelectorConditionDTO> selectorConditionDTOs = selectorDTO.getSelectorConditions();
        if (StringUtils.isEmpty(selectorDTO.getId())) {
            selectorCount = selectorMapper.insertSelective(selectorDO);
            selectorConditionDTOs.forEach(selectorConditionDTO -> {
                selectorConditionDTO.setSelectorId(selectorDO.getId());
                selectorConditionMapper.insertSelective(SelectorConditionDO.buildSelectorConditionDO(selectorConditionDTO));
            });
            // check selector add
            if (Boolean.TRUE.equals(dataPermissionMapper.existed(JwtUtils.getUserInfo().getUserId()))) {
                DataPermissionDTO dataPermissionDTO = new DataPermissionDTO();
                dataPermissionDTO.setUserId(JwtUtils.getUserInfo().getUserId());
                dataPermissionDTO.setDataId(selectorDO.getId());
                dataPermissionDTO.setDataType(AdminConstants.SELECTOR_DATA_TYPE);
                dataPermissionMapper.insertSelective(DataPermissionDO.buildPermissionDO(dataPermissionDTO));
            }
            
        } else {
            selectorCount = selectorMapper.updateSelective(selectorDO);
            //delete rule condition then add
            selectorConditionMapper.deleteByQuery(new SelectorConditionQuery(selectorDO.getId()));
            selectorConditionDTOs.forEach(selectorConditionDTO -> {
                selectorConditionDTO.setSelectorId(selectorDO.getId());
                SelectorConditionDO selectorConditionDO = SelectorConditionDO.buildSelectorConditionDO(selectorConditionDTO);
                selectorConditionMapper.insertSelective(selectorConditionDO);
            });
        }
        publishEvent(selectorDO, selectorConditionDTOs);
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
            Map<String, SelectorDO> selectorDOMap = selectorDOList.stream().filter(Objects::nonNull)
                    .collect(Collectors.toMap(SelectorDO::getId, Function.identity(), (value1, value2) -> value1));
            List<String> pluginIdList = selectorDOMap.values().stream().map(SelectorDO::getPluginId).collect(Collectors.toList());
            List<PluginDO> pluginDOList = pluginMapper.selectByIds(new ArrayList<>(pluginIdList));
            
            if (CollectionUtils.isNotEmpty(pluginDOList)) {
                Map<String, String> pluginMap = pluginDOList.stream().filter(Objects::nonNull)
                        .collect(Collectors.toMap(PluginDO::getId, PluginDO::getName, (value1, value2) -> value1));
                if (pluginMap.size() == pluginIdList.size()) {
                    selectorMapper.deleteByIds(ids);
                    selectorConditionMapper.deleteBySelectorIds(ids);
                    dataPermissionMapper.deleteByDataIdList(ids);
                    
                    List<SelectorData> selectorDataList = selectorDOMap.values().stream().map(selectorDO -> {
                        String pluginName = pluginMap.get(selectorDO.getPluginId());
                        if (pluginName.equals(PluginEnum.DIVIDE.getName())) {
                            UpstreamCheckService.removeByKey(selectorDO.getId());
                        }
                        return SelectorDO.transFrom(selectorDO, pluginName, null);
                    }).collect(Collectors.toList());
                    eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.SELECTOR, DataEventTypeEnum.DELETE, selectorDataList));
                    
                    List<RuleDO> ruleDOList = ruleMapper.findBySelectorIds(ids);
                    if (CollectionUtils.isNotEmpty(ruleDOList)) {
                        List<String> ruleIdList = new ArrayList<>();
                        List<RuleData> ruleDataList = ruleDOList.stream().filter(Objects::nonNull).map(ruleDO -> {
                            ruleIdList.add(ruleDO.getId());
                            SelectorDO selectorDO = selectorDOMap.get(ruleDO.getSelectorId());
                            String pluginName = pluginMap.get(selectorDO.getPluginId());
                            return RuleDO.transFrom(ruleDO, pluginName, null);
                        }).collect(Collectors.toList());
                        ruleMapper.deleteByIds(ruleIdList);
                        ruleConditionMapper.deleteByRuleIds(ruleIdList);
                        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.RULE, DataEventTypeEnum.DELETE, ruleDataList));
                    }
                }
            }
        }
        return idSet.size();
    }
    
    /**
     * find selector by id.
     *
     * @param id primary key.
     * @return {@linkplain SelectorVO}
     */
    @Override
    public SelectorVO findById(final String id) {
        return SelectorVO.buildSelectorVO(selectorMapper.selectById(id),
                selectorConditionMapper.selectByQuery(
                        new SelectorConditionQuery(id))
                        .stream()
                        .map(SelectorConditionVO::buildSelectorConditionVO)
                        .collect(Collectors.toList()));
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
        
        List<SelectorDO> selectorDOList = selectorMapper.findByPluginId(pluginId);
        
        return this.buildSelectorDataList(selectorDOList);
    }
    
    @Override
    public List<SelectorData> listAll() {
        
        List<SelectorDO> selectorDOList = selectorMapper.selectAll();
        
        return this.buildSelectorDataList(selectorDOList);
    }
    
    private void publishEvent(final SelectorDO selectorDO, final List<SelectorConditionDTO> selectorConditionDTOs) {
        PluginDO pluginDO = pluginMapper.selectById(selectorDO.getPluginId());
        List<ConditionData> conditionDataList =
                selectorConditionDTOs.stream().map(ConditionTransfer.INSTANCE::mapToSelectorDTO).collect(Collectors.toList());
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
        
        Map<String, String> idMap = Optional.ofNullable(selectorDOList).orElseGet(ArrayList::new)
                .stream().filter(Objects::nonNull)
                .collect(Collectors.toMap(SelectorDO::getId, SelectorDO::getPluginId, (value1, value2) -> value1));
        if (MapUtils.isEmpty(idMap)) {
            return new ArrayList<>();
        }
        Map<String, List<SelectorConditionDO>> selectorConditionMap = Optional.ofNullable(selectorConditionMapper.selectBySelectorIds(idMap.keySet()))
                .orElseGet(ArrayList::new).stream().collect(Collectors.toMap(SelectorConditionDO::getSelectorId, selectorConditionDO -> {
                    List<SelectorConditionDO> dataList = new ArrayList<>();
                    dataList.add(selectorConditionDO);
                    return dataList;
                }, (list1, list2) -> {
                        list1.addAll(list2);
                        return list1;
                    }));
        
        Map<String, PluginDO> pluginDOMap = Optional.ofNullable(pluginMapper.selectByIds(Lists.newArrayList(idMap.values()))).orElseGet(ArrayList::new)
                .stream().filter(Objects::nonNull).collect(Collectors.toMap(PluginDO::getId, Function.identity(), (value1, value2) -> value1));
        
        return Optional.ofNullable(selectorDOList).orElseGet(ArrayList::new)
                .stream().filter(Objects::nonNull).map(selectorDO -> {
                    String id = selectorDO.getId();
                    String pluginId = selectorDO.getPluginId();
                    PluginDO pluginDO = pluginDOMap.get(pluginId);
                    if (Objects.isNull(pluginDO)) {
                        return null;
                    }
                    List<ConditionData> conditionDataList = ConditionTransfer.INSTANCE.mapToSelectorDOS(selectorConditionMap.get(id));
                    return SelectorDO.transFrom(selectorDO, pluginDO.getName(), conditionDataList);
                }).collect(Collectors.toList());
    }
    
    private void updateDivideUpstream(final SelectorDO selectorDO) {
        String selectorId = selectorDO.getId();
        PluginDO pluginDO = pluginMapper.selectById(selectorDO.getPluginId());
        List<DivideUpstream> existDivideUpstreams = null;
        if (PluginEnum.SPRING_CLOUD.getName().equals(pluginDO.getName())) {
            if (Objects.nonNull(selectorDO.getHandle())) {
                SpringCloudSelectorHandle springCloudSelectorHandle = GsonUtils.getInstance()
                        .fromJson(selectorDO.getHandle(), SpringCloudSelectorHandle.class);
                existDivideUpstreams = springCloudSelectorHandle.getDivideUpstreams();
            }
        } else if (PluginEnum.DIVIDE.getName().equals(pluginDO.getName())) {
            String handle = selectorDO.getHandle();
            if (StringUtils.isNotBlank(handle)) {
                existDivideUpstreams = GsonUtils.getInstance().fromList(handle, DivideUpstream.class);
            }
        }
        if (CollectionUtils.isNotEmpty(existDivideUpstreams)) {
            upstreamCheckService.replace(selectorId, CommonUpstreamUtils.convertCommonUpstreamList(existDivideUpstreams));
        }
    }
    
    private String registerSelector(final String contextPath, final String pluginName, final String selectorHandler) {
        SelectorDTO selectorDTO = buildSelectorDTO(contextPath, pluginMapper.selectByName(pluginName).getId());
        selectorDTO.setHandle(selectorHandler);
        return registerDefault(selectorDTO);
    }
    
    private SelectorDTO buildSelectorDTO(final String contextPath, final String pluginId) {
        SelectorDTO selectorDTO = buildDefaultSelectorDTO(contextPath);
        selectorDTO.setPluginId(pluginId);
        selectorDTO.setSelectorConditions(buildDefaultSelectorConditionDTO(contextPath));
        return selectorDTO;
    }
    
    private SelectorDTO buildDefaultSelectorDTO(final String name) {
        return SelectorDTO.builder()
                .name(name)
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                .matchMode(MatchModeEnum.AND.getCode())
                .enabled(Boolean.TRUE)
                .loged(Boolean.TRUE)
                .continued(Boolean.TRUE)
                .sort(1)
                .build();
    }
    
    private List<SelectorConditionDTO> buildDefaultSelectorConditionDTO(final String contextPath) {
        SelectorConditionDTO selectorConditionDTO = new SelectorConditionDTO();
        selectorConditionDTO.setParamType(ParamTypeEnum.URI.getName());
        selectorConditionDTO.setParamName("/");
        selectorConditionDTO.setOperator(OperatorEnum.MATCH.getAlias());
        selectorConditionDTO.setParamValue(contextPath + AdminConstants.URI_SUFFIX);
        return Collections.singletonList(selectorConditionDTO);
    }
}
