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
import org.apache.shenyu.admin.discovery.DiscoveryLevel;
import org.apache.shenyu.admin.discovery.DiscoveryProcessor;
import org.apache.shenyu.admin.discovery.DiscoveryProcessorHolder;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.DiscoveryHandlerMapper;
import org.apache.shenyu.admin.mapper.DiscoveryMapper;
import org.apache.shenyu.admin.mapper.DiscoveryRelMapper;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.SelectorConditionMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.dto.RuleConditionDTO;
import org.apache.shenyu.admin.model.dto.SelectorConditionDTO;
import org.apache.shenyu.admin.model.dto.SelectorDTO;
import org.apache.shenyu.admin.model.entity.BaseDO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.model.entity.DiscoveryHandlerDO;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.SelectorConditionDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.event.plugin.BatchNamespacePluginDeletedEvent;
import org.apache.shenyu.admin.model.event.selector.SelectorCreatedEvent;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.SelectorConditionQuery;
import org.apache.shenyu.admin.model.query.SelectorQuery;
import org.apache.shenyu.admin.model.query.SelectorQueryCondition;
import org.apache.shenyu.admin.model.result.ConfigImportResult;
import org.apache.shenyu.admin.model.vo.DiscoveryUpstreamVO;
import org.apache.shenyu.admin.model.vo.DiscoveryVO;
import org.apache.shenyu.admin.model.vo.SelectorConditionVO;
import org.apache.shenyu.admin.model.vo.SelectorVO;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.service.configs.ConfigsImportContext;
import org.apache.shenyu.admin.service.publish.SelectorEventPublisher;
import org.apache.shenyu.admin.transfer.ConditionTransfer;
import org.apache.shenyu.admin.transfer.DiscoveryTransfer;
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
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.common.utils.ListUtil;
import org.apache.shenyu.common.utils.UUIDUtils;
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
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;

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

    private final DiscoveryHandlerMapper discoveryHandlerMapper;

    private final DiscoveryUpstreamMapper discoveryUpstreamMapper;

    private final DiscoveryMapper discoveryMapper;

    private final DiscoveryRelMapper discoveryRelMapper;

    private final DiscoveryProcessorHolder discoveryProcessorHolder;

    public SelectorServiceImpl(final SelectorMapper selectorMapper,
                               final SelectorConditionMapper selectorConditionMapper,
                               final PluginMapper pluginMapper,
                               final ApplicationEventPublisher eventPublisher,
                               final DiscoveryMapper discoveryMapper,
                               final DiscoveryHandlerMapper discoveryHandlerMapper,
                               final DiscoveryRelMapper discoveryRelMapper,
                               final DiscoveryUpstreamMapper discoveryUpstreamMapper,
                               final DiscoveryProcessorHolder discoveryProcessorHolder,
                               final SelectorEventPublisher selectorEventPublisher) {
        this.selectorMapper = selectorMapper;
        this.selectorConditionMapper = selectorConditionMapper;
        this.pluginMapper = pluginMapper;
        this.discoveryMapper = discoveryMapper;
        this.discoveryHandlerMapper = discoveryHandlerMapper;
        this.discoveryRelMapper = discoveryRelMapper;
        this.discoveryUpstreamMapper = discoveryUpstreamMapper;
        this.discoveryProcessorHolder = discoveryProcessorHolder;
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
        // adapt to namespace
        selectorEventPublisher.publish(new SelectorCreatedEvent(selectorDO, selectorDO.getNamespaceId()));
        publishEvent(selectorDO, selectorDTO.getSelectorConditions(), Collections.emptyList());
        return selectorDO.getId();
    }

    @Override
    public String registerDefault(final MetaDataRegisterDTO dto, final String pluginName, final String selectorHandler) {
        String contextPath = ContextPathUtils.buildContextPath(dto.getContextPath(), dto.getAppName());
        String namespaceId = StringUtils.defaultIfEmpty(dto.getNamespaceId(), SYS_DEFAULT_NAMESPACE_ID);
        SelectorDO selectorDO = findByNameAndPluginNameAndNamespaceId(contextPath, pluginName, namespaceId);
        if (Objects.isNull(selectorDO)) {
            SelectorDTO selectorDTO = SelectorUtil.buildSelectorDTO(contextPath, pluginMapper.selectByName(pluginName).getId());
            selectorDTO.setHandle(selectorHandler);
            selectorDTO.setNamespaceId(namespaceId);
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
        selectorDTO.setId(selectorDO.getId());
        createCondition(selectorDO.getId(), selectorDTO.getSelectorConditions());
        publishEvent(selectorDO, selectorDTO.getSelectorConditions(), Collections.emptyList());
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

        // need old data for cleaning
        List<SelectorConditionDO> beforeSelectorConditionList = selectorConditionMapper.selectByQuery(new SelectorConditionQuery(selectorDO.getId()));
        List<RuleConditionDTO> beforeCondition = beforeSelectorConditionList.stream().map(selectorConditionDO ->
                SelectorConditionDTO.builder()
                        .selectorId(selectorConditionDO.getSelectorId())
                        .operator(selectorConditionDO.getOperator())
                        .paramName(selectorConditionDO.getParamName())
                        .paramType(selectorConditionDO.getParamType())
                        .paramValue(selectorConditionDO.getParamValue())
                        .build()).collect(Collectors.toList());
        List<RuleConditionDTO> currentCondition = selectorDTO.getSelectorConditions().stream().map(selectorConditionDTO ->
                SelectorConditionDTO.builder()
                        .selectorId(selectorConditionDTO.getSelectorId())
                        .operator(selectorConditionDTO.getOperator())
                        .paramName(selectorConditionDTO.getParamName())
                        .paramType(selectorConditionDTO.getParamType())
                        .paramValue(selectorConditionDTO.getParamValue())
                        .build()).collect(Collectors.toList());
        if (CollectionUtils.isEqualCollection(beforeCondition, currentCondition)) {
            beforeSelectorConditionList = Collections.emptyList();
        }

        //delete rule condition then add
        selectorConditionMapper.deleteByQuery(new SelectorConditionQuery(selectorDO.getId()));
        createCondition(selectorDO.getId(), selectorDTO.getSelectorConditions());
        publishEvent(selectorDO, selectorDTO.getSelectorConditions(), beforeSelectorConditionList);
        if (selectorCount > 0) {
            selectorEventPublisher.onUpdated(selectorDO, before);
        }
        return selectorCount;
    }

    @Override
    public int updateSelective(final SelectorDO selectorDO) {
        final SelectorDO before = selectorMapper.selectById(selectorDO.getId());
        final int updateCount = selectorMapper.updateSelective(selectorDO);
        if (updateCount > 0) {
            selectorEventPublisher.onUpdated(selectorDO, before);
        }
        return updateCount;
    }

    /**
     * delete selectors by ids and namespaceId.
     *
     * @param ids primary key.
     * @param namespaceId namespaceId.
     * @return rows
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public int deleteByNamespaceId(final List<String> ids, final String namespaceId) {
        final List<SelectorDO> selectors = selectorMapper.selectByIdSet(new TreeSet<>(ids));
        List<PluginDO> pluginDOS = pluginMapper.selectByIds(ListUtil.map(selectors, SelectorDO::getPluginId));
        unbindDiscovery(selectors, pluginDOS);
        return deleteSelector(selectors, pluginDOS);
    }

    /**
     * unbind discovery.
     *
     * @param selectors selectors
     */
    private void unbindDiscovery(final List<SelectorDO> selectors, final List<PluginDO> pluginDOS) {
        Map<String, String> pluginMap = ListUtil.toMap(pluginDOS, PluginDO::getId, PluginDO::getName);
        for (SelectorDO selector : selectors) {
            DiscoveryHandlerDO discoveryHandlerDO = discoveryHandlerMapper.selectBySelectorId(selector.getId());
            if (Objects.isNull(discoveryHandlerDO)) {
                continue;
            }
            discoveryHandlerMapper.delete(discoveryHandlerDO.getId());
            discoveryRelMapper.deleteByDiscoveryHandlerId(discoveryHandlerDO.getId());
            discoveryUpstreamMapper.deleteByDiscoveryHandlerId(discoveryHandlerDO.getId());
            DiscoveryDO discoveryDO = discoveryMapper.selectById(discoveryHandlerDO.getDiscoveryId());
            if (Objects.nonNull(discoveryDO)) {
                final DiscoveryProcessor discoveryProcessor = discoveryProcessorHolder.chooseProcessor(discoveryDO.getType());
                ProxySelectorDTO proxySelectorDTO = new ProxySelectorDTO();
                proxySelectorDTO.setId(selector.getId());
                proxySelectorDTO.setName(selector.getName());
                proxySelectorDTO.setPluginName(pluginMap.getOrDefault(selector.getPluginId(), ""));
                proxySelectorDTO.setNamespaceId(selector.getNamespaceId());
                discoveryProcessor.removeProxySelector(DiscoveryTransfer.INSTANCE.mapToDTO(discoveryHandlerDO), proxySelectorDTO);
                if (DiscoveryLevel.SELECTOR.getCode().equals(discoveryDO.getLevel())) {
                    discoveryProcessor.removeDiscovery(discoveryDO);
                    discoveryProcessor.removeSelectorUpstream(proxySelectorDTO);
                    discoveryMapper.delete(discoveryDO.getId());
                }
            }
        }
    }

    /**
     * find selector by id and namespaceId.
     *
     * @param id primary key.
     * @return {@link SelectorVO}
     */
    @Override
    public SelectorVO findById(final String id) {
        final List<SelectorConditionVO> conditions = ListUtil.map(selectorConditionMapper.selectByQuery(new SelectorConditionQuery(id)), SelectorConditionVO::buildSelectorConditionVO);
        SelectorVO selectorVO = SelectorVO.buildSelectorVO(selectorMapper.selectById(id), conditions);
        DiscoveryHandlerDO discoveryHandlerDO = discoveryHandlerMapper.selectBySelectorId(id);
        if (Objects.nonNull(discoveryHandlerDO)) {
            selectorVO.setDiscoveryHandler(DiscoveryTransfer.INSTANCE.mapToVo(discoveryHandlerDO));
            String discoveryId = discoveryHandlerDO.getDiscoveryId();
            DiscoveryDO discoveryDO = discoveryMapper.selectById(discoveryId);
            DiscoveryVO discoveryVO = DiscoveryTransfer.INSTANCE.mapToVo(discoveryDO);
            selectorVO.setDiscoveryVO(discoveryVO);
            List<DiscoveryUpstreamDO> discoveryUpstreamDOS = discoveryUpstreamMapper.selectByDiscoveryHandlerId(discoveryHandlerDO.getId());
            Optional.ofNullable(discoveryUpstreamDOS).ifPresent(list -> {
                List<DiscoveryUpstreamVO> upstreamVOS = list.stream().map(DiscoveryTransfer.INSTANCE::mapToVo).collect(Collectors.toList());
                selectorVO.setDiscoveryUpstreams(upstreamVOS);
            });
        }
        return selectorVO;
    }

    @Override
    public SelectorDO findByNameAndNamespaceId(final String name, final String namespaceId) {
        List<SelectorDO> doList = selectorMapper.selectByNameAndNamespaceId(name, namespaceId);
        return CollectionUtils.isNotEmpty(doList) ? doList.get(0) : null;
    }

    @Override
    public List<SelectorDO> findListByNameAndNamespaceId(final String name, final String namespaceId) {
        return selectorMapper.selectByNameAndNamespaceId(name, namespaceId);
    }

    /**
     * Find selectorDO by name and plugin name and namespaceId.
     *
     * @param name       the name
     * @param pluginName the plugin name
     * @param namespaceId namespaceId
     * @return the selector do
     */
    @Override
    public SelectorDO findByNameAndPluginNameAndNamespaceId(final String name, final String pluginName, final String namespaceId) {
        PluginDO pluginDO = pluginMapper.selectByName(pluginName);
        return selectorMapper.findByNameAndPluginIdAndNamespaceId(name, pluginDO.getId(), namespaceId);
    }

    @Override
    public SelectorDO findByNameAndPluginNameAndNamespaceIdForUpdate(final String name, final String pluginName, final String namespaceId) {
        PluginDO pluginDO = pluginMapper.selectByNameForUpdate(pluginName);
        return selectorMapper.findByNameAndPluginIdAndNamespaceId(name, pluginDO.getId(), namespaceId);
    }

    @Override
    public List<SelectorDO> findByNameAndPluginNamesAndNamespaceId(final String name, final List<String> pluginNames, final String namespaceID) {
        final List<PluginDO> pluginDOList = pluginMapper.selectByNames(pluginNames);
        if (CollectionUtils.isEmpty(pluginDOList)) {
            return Lists.newArrayList();
        }
        List<String> pluginIds = pluginDOList.stream().map(BaseDO::getId).collect(Collectors.toList());
        SelectorQuery selectorQuery = new SelectorQuery();
        selectorQuery.setName(name);
        selectorQuery.setPluginIds(pluginIds);
        selectorQuery.setNamespaceId(namespaceID);
        return selectorMapper.selectByQuery(selectorQuery);
    }

    @Override
    public SelectorData buildByNameAndNamespaceId(final String name, final String namespaceId) {
        return buildSelectorData(this.findByNameAndNamespaceId(name, namespaceId));
    }

    /**
     * Build by name and namespaceId selector data.
     *
     * @param name       the name
     * @param pluginName the plugin name
     * @param namespaceId the namespaceId.
     * @return the selector data
     */
    @Override
    public SelectorData buildByNameAndPluginNameAndNamespaceId(final String name, final String pluginName, final String namespaceId) {
        return buildSelectorData(findByNameAndPluginNameAndNamespaceId(name, pluginName, namespaceId));
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
    public List<SelectorData> findByPluginIdAndNamespaceId(final String pluginId, final String namespaceId) {
        return this.buildSelectorDataList(selectorMapper.findByPluginIdAndNamespaceId(pluginId, namespaceId));
    }

    @Override
    public List<SelectorData> listAll() {
        return this.buildSelectorDataList(selectorMapper.selectAll());
    }

    @Override
    public List<SelectorData> listAllByNamespaceId(final String namespaceId) {
        return this.buildSelectorDataList(selectorMapper.selectAllByNamespaceId(namespaceId));
    }

    @Override
    public List<SelectorVO> listAllData() {
        return this.buildSelectorExportVOList(selectorMapper.selectAll());
    }
    
    @Override
    public List<SelectorVO> listAllDataByNamespaceId(final String namespaceId) {
        return this.buildSelectorExportVOList(selectorMapper.selectAllByNamespaceId(namespaceId));
    }
    
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ConfigImportResult importData(final List<SelectorDTO> selectorList) {
        if (CollectionUtils.isEmpty(selectorList)) {
            return ConfigImportResult.success();
        }
        StringBuilder errorMsgBuilder = new StringBuilder();
        int successCount = 0;
        Map<String, List<SelectorDO>> pluginSelectorMap = selectorMapper.selectAll().stream()
                .filter(Objects::nonNull)
                .collect(Collectors.groupingBy(SelectorDO::getPluginId));

        Map<String, List<SelectorDTO>> importSelectorMap = selectorList.stream()
                .collect(Collectors.groupingBy(SelectorDTO::getPluginId));

        for (Map.Entry<String, List<SelectorDTO>> selectorEntry : importSelectorMap.entrySet()) {
            // the import selector's pluginId
            String pluginId = selectorEntry.getKey();
            List<SelectorDTO> selectorDTOList = selectorEntry.getValue();
            if (CollectionUtils.isNotEmpty(selectorDTOList)) {

                Set<String> existSelectorSet = Optional
                        .ofNullable(pluginSelectorMap.get(pluginId))
                        .orElseGet(Lists::newArrayList)
                        .stream()
                        .map(SelectorDO::getName)
                        .collect(Collectors.toSet());

                for (SelectorDTO selectorDTO : selectorDTOList) {
                    // filter by selectorName
                    String selectorName = selectorDTO.getName();
                    if (CollectionUtils.isNotEmpty(existSelectorSet)
                            && existSelectorSet.contains(selectorName)) {
                        errorMsgBuilder
                                .append(selectorName)
                                .append(",");
                    } else {
                        create(selectorDTO);
                        successCount++;
                    }
                }
            }
        }
        if (StringUtils.isNotEmpty(errorMsgBuilder)) {
            errorMsgBuilder.setLength(errorMsgBuilder.length() - 1);
            return ConfigImportResult
                    .fail(successCount, "import fail selector: " + errorMsgBuilder);
        }
        return ConfigImportResult.success(successCount);
    }
    
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ConfigImportResult importData(final String namespace, final List<SelectorDTO> selectorList, final ConfigsImportContext context) {
        if (CollectionUtils.isEmpty(selectorList)) {
            return ConfigImportResult.success();
        }
        Map<String, String> selectorIdMapping = context.getSelectorIdMapping();
        StringBuilder errorMsgBuilder = new StringBuilder();
        int successCount = 0;
        Map<String, List<SelectorDO>> pluginSelectorMap = selectorMapper.selectAllByNamespaceId(namespace).stream()
                .filter(Objects::nonNull)
                .collect(Collectors.groupingBy(SelectorDO::getPluginId));
        
        Map<String, List<SelectorDTO>> importSelectorMap = selectorList.stream()
                .collect(Collectors.groupingBy(SelectorDTO::getPluginId));
        
        for (Map.Entry<String, List<SelectorDTO>> selectorEntry : importSelectorMap.entrySet()) {
            // the import selector's pluginId
            String pluginId = context.getPluginTemplateIdMapping().get(selectorEntry.getKey());
            List<SelectorDTO> selectorDTOList = selectorEntry.getValue();
            if (CollectionUtils.isNotEmpty(selectorDTOList)) {
                
                Map<String, String> existSelectorSet = Optional
                        .ofNullable(pluginSelectorMap.get(pluginId))
                        .orElseGet(Lists::newArrayList)
                        .stream()
                        .collect(Collectors.toMap(SelectorDO::getName, SelectorDO::getId));
                
                for (SelectorDTO selectorDTO : selectorDTOList) {
                    // filter by selectorName
                    String selectorName = selectorDTO.getName();
                    if (MapUtils.isNotEmpty(existSelectorSet)
                            && existSelectorSet.containsKey(selectorName)) {
                        errorMsgBuilder
                                .append(selectorName)
                                .append(",");
                        selectorIdMapping.put(selectorDTO.getId(), existSelectorSet.get(selectorName));
                    } else {
                        // gen new id
                        String selectorId = UUIDUtils.getInstance().generateShortUuid();
                        selectorIdMapping.put(selectorDTO.getId(), selectorId);
                        selectorDTO.setId(selectorId);
                        selectorDTO.setNamespaceId(namespace);
                        selectorDTO.setPluginId(pluginId);
                        Optional.ofNullable(selectorDTO.getSelectorConditions())
                                        .orElse(Collections.emptyList()).forEach(c -> {
                                            c.setSelectorId(selectorId);
                                            c.setId(null);
                                        });
                        create(selectorDTO);
                        successCount++;
                    }
                }
            }
        }
        if (StringUtils.isNotEmpty(errorMsgBuilder)) {
            errorMsgBuilder.setLength(errorMsgBuilder.length() - 1);
            return ConfigImportResult
                    .fail(successCount, "import fail selector: " + errorMsgBuilder);
        }
        return ConfigImportResult.success(successCount);
    }
    
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean enabledByIdsAndNamespaceId(final List<String> ids, final Boolean enabled, final String namespaceId) {
        ids.forEach(id -> {
            SelectorDO selectorDO = selectorMapper.selectById(id);
            SelectorDO before = JsonUtils.jsonToObject(JsonUtils.toJson(selectorDO), SelectorDO.class);
            selectorDO.setEnabled(enabled);
            if (selectorMapper.updateEnable(id, enabled) > 0) {
                List<SelectorConditionDO> conditionList = selectorConditionMapper.selectByQuery(new SelectorConditionQuery(selectorDO.getId()));
                List<SelectorConditionDTO> selectorConditions = conditionList.stream().map(item -> {
                    SelectorConditionDTO selectorConditionDTO = new SelectorConditionDTO();
                    selectorConditionDTO.setId(item.getId());
                    selectorConditionDTO.setSelectorId(item.getSelectorId());
                    selectorConditionDTO.setParamType(item.getParamType());
                    selectorConditionDTO.setOperator(item.getOperator());
                    selectorConditionDTO.setParamName(item.getParamName());
                    selectorConditionDTO.setParamValue(item.getParamValue());
                    return selectorConditionDTO;
                }).toList();
                publishEvent(selectorDO, selectorConditions, Collections.emptyList());
                selectorEventPublisher.onUpdated(selectorDO, before);
            }
        });
        return Boolean.TRUE;
    }

    /**
     * the plugin delete, synchronously delete selectors.
     *
     * @param event event
     */
    @EventListener(value = BatchNamespacePluginDeletedEvent.class)
    public void onPluginDeleted(final BatchNamespacePluginDeletedEvent event) {
        deleteSelector(selectorMapper.findByPluginIdsAndNamespaceId(event.getDeletedPluginIds(), event.getNamespaceId()), event.getPlugins());
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

    private void publishEvent(final SelectorDO selectorDO, final List<SelectorConditionDTO> selectorConditions, final List<SelectorConditionDO> beforeSelectorCondition) {
        PluginDO pluginDO = pluginMapper.selectById(selectorDO.getPluginId());
        List<ConditionData> conditionDataList = ListUtil.map(selectorConditions, ConditionTransfer.INSTANCE::mapToSelectorDTO);
        List<ConditionData> beforeConditionDataList = ListUtil.map(beforeSelectorCondition, ConditionTransfer.INSTANCE::mapToSelectorDO);
        // build selector data.
        SelectorData selectorData = SelectorDO.transFrom(selectorDO, pluginDO.getName(), conditionDataList, beforeConditionDataList);
        // publish change event.
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.SELECTOR, DataEventTypeEnum.UPDATE,
                Collections.singletonList(selectorData)));
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

    private List<SelectorVO> buildSelectorExportVOList(final List<SelectorDO> selectorDOList) {
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
                    List<SelectorConditionVO> selectorConditionList = SelectorConditionVO.buildSelectorConditionVOList(selectorConditionMap.get(id));
                    SelectorVO selectorExportVO = SelectorVO.buildSelectorVO(selectorDO);
                    selectorExportVO.setSelectorConditions(selectorConditionList);
                    return selectorExportVO;
                })
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
    }

}
