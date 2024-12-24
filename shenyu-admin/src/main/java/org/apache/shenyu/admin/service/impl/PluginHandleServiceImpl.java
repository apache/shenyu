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
import org.apache.shenyu.admin.aspect.annotation.Pageable;
import org.apache.shenyu.admin.mapper.PluginHandleMapper;
import org.apache.shenyu.admin.mapper.ShenyuDictMapper;
import org.apache.shenyu.admin.model.dto.PluginHandleDTO;
import org.apache.shenyu.admin.model.entity.BaseDO;
import org.apache.shenyu.admin.model.entity.PluginHandleDO;
import org.apache.shenyu.admin.model.event.plugin.BatchPluginDeletedEvent;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.PluginHandleQuery;
import org.apache.shenyu.admin.model.result.ConfigImportResult;
import org.apache.shenyu.admin.model.vo.PluginHandleVO;
import org.apache.shenyu.admin.model.vo.ShenyuDictVO;
import org.apache.shenyu.admin.service.PluginHandleService;
import org.apache.shenyu.admin.service.configs.ConfigsImportContext;
import org.apache.shenyu.admin.service.publish.PluginHandleEventPublisher;
import org.apache.shenyu.common.utils.ListUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.PluginHandleService}.
 */
@Service
public class PluginHandleServiceImpl implements PluginHandleService {

    private static final Logger LOG = LoggerFactory.getLogger(PluginHandleServiceImpl.class);

    private static final int SELECT_BOX_DATA_TYPE = 3;

    private final PluginHandleMapper pluginHandleMapper;

    private final ShenyuDictMapper shenyuDictMapper;

    private final PluginHandleEventPublisher eventPublisher;

    public PluginHandleServiceImpl(final PluginHandleMapper pluginHandleMapper,
                                   final ShenyuDictMapper shenyuDictMapper,
                                   final PluginHandleEventPublisher eventPublisher) {
        this.pluginHandleMapper = pluginHandleMapper;
        this.shenyuDictMapper = shenyuDictMapper;
        this.eventPublisher = eventPublisher;
    }

    @Override
    @Pageable
    public CommonPager<PluginHandleVO> listByPage(final PluginHandleQuery pluginHandleQuery) {
        List<PluginHandleDO> pluginHandleDOList = pluginHandleMapper.selectByQuery(pluginHandleQuery);
        return PageResultUtils.result(pluginHandleQuery.getPageParameter(), () -> this.buildPluginHandleVO(pluginHandleDOList));
    }

    @Override
    public Integer create(final PluginHandleDTO pluginHandleDTO) {
        PluginHandleDO pluginHandleDO = PluginHandleDO.buildPluginHandleDO(pluginHandleDTO);
        int pluginHandleCount = pluginHandleMapper.insertSelective(pluginHandleDO);
        if (pluginHandleCount > 0) {
            eventPublisher.onCreated(pluginHandleDO);
        }
        return pluginHandleCount;
    }

    @Override
    public Integer update(final PluginHandleDTO pluginHandleDTO) {
        PluginHandleDO pluginHandleDO = PluginHandleDO.buildPluginHandleDO(pluginHandleDTO);
        final PluginHandleDO before = pluginHandleMapper.selectById(pluginHandleDTO.getId());
        int pluginHandleCount = pluginHandleMapper.updateByPrimaryKeySelective(pluginHandleDO);
        if (pluginHandleCount > 0) {
            eventPublisher.onUpdated(pluginHandleDO, before);
        }
        return pluginHandleCount;
    }

    @Override
    public Integer deletePluginHandles(final List<String> ids) {
        if (CollectionUtils.isEmpty(ids)) {
            return 0;
        }
        final List<PluginHandleDO> handles = pluginHandleMapper.selectByIdList(ids);
        final int count = pluginHandleMapper.deleteByIdList(ids);
        if (count > 0) {
            eventPublisher.onDeleted(handles);
        }
        return count;
    }

    @Override
    public PluginHandleVO findById(final String id) {
        return buildPluginHandleVO(pluginHandleMapper.selectById(id));
    }

    @Override
    public List<PluginHandleVO> list(final String pluginId, final Integer type) {
        List<PluginHandleDO> pluginHandleDOList = pluginHandleMapper.selectByQuery(PluginHandleQuery.builder()
                .pluginId(pluginId)
                .type(type)
                .build());
        return buildPluginHandleVO(pluginHandleDOList);
    }

    @Override
    public List<PluginHandleVO> listAllData() {
        List<PluginHandleDO> pluginHandleDOList = pluginHandleMapper.selectByQuery(PluginHandleQuery.builder()
                .build());
        return buildPluginHandleVO(pluginHandleDOList);
    }
    
    @Override
    public List<PluginHandleVO> listAllDataByPluginIds(final Collection<String> pluginIds) {
        List<PluginHandleDO> pluginHandleDOList = pluginHandleMapper.selectByPluginIds(pluginIds);
        if (CollectionUtils.isEmpty(pluginHandleDOList)) {
            return Lists.newArrayList();
        }
        return buildPluginHandleVO(pluginHandleDOList);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ConfigImportResult importData(final List<PluginHandleDTO> pluginHandleList, final ConfigsImportContext context) {
        Map<String, List<PluginHandleVO>> existHandleMap = listAllData()
                .stream()
                .filter(Objects::nonNull)
                .collect(Collectors.groupingBy(PluginHandleVO::getPluginId));
        Map<String, List<PluginHandleDTO>> importHandleMap = pluginHandleList
                .stream()
                .filter(Objects::nonNull)
                .collect(Collectors.groupingBy(PluginHandleDTO::getPluginId));

        int successCount = 0;
        for (Map.Entry<String, List<PluginHandleDTO>> pluginHandleEntry : importHandleMap.entrySet()) {
            // pluginId
            String pluginId = context.getPluginTemplateIdMapping().get(pluginHandleEntry.getKey());
            List<PluginHandleDTO> handles = pluginHandleEntry.getValue();
            if (CollectionUtils.isNotEmpty(handles)) {
                if (existHandleMap.containsKey(pluginId)) {
                    // check and just add difference handle
                    List<PluginHandleVO> existHandles = existHandleMap.getOrDefault(pluginId, Lists.newArrayList());
                    Set<String> handleFiledMap = existHandles.stream()
                            .map(PluginHandleVO::getField)
                            .collect(Collectors.toSet());
                    for (PluginHandleDTO handle : handles) {
                        if (!handleFiledMap.contains(handle.getField())) {
                            handle.setPluginId(pluginId);
                            create(handle);
                            successCount++;
                        }
                    }
                } else {
                    for (PluginHandleDTO handle : handles) {
                        handle.setPluginId(pluginId);
                        create(handle);
                        successCount++;
                    }
                }
            }
        }
        return ConfigImportResult.success(successCount);
    }

    /**
     * The associated Handle needs to be deleted synchronously.
     *
     * @param event event
     */
    @EventListener(value = BatchPluginDeletedEvent.class)
    public void onPluginDeleted(final BatchPluginDeletedEvent event) {
        deletePluginHandles(ListUtil.map(pluginHandleMapper.selectByPluginIdList(event.getDeletedPluginIds()), BaseDO::getId));
    }

    private PluginHandleVO buildPluginHandleVO(final PluginHandleDO pluginHandleDO) {
        List<ShenyuDictVO> dictOptions = null;
        if (pluginHandleDO.getDataType() == SELECT_BOX_DATA_TYPE) {
            dictOptions = shenyuDictMapper.findByType(pluginHandleDO.getField())
                    .stream()
                    .filter(item -> Objects.equals(item.getEnabled(), Boolean.TRUE))
                    .map(ShenyuDictVO::buildShenyuDictVO)
                    .toList();
        }
        return PluginHandleVO.buildPluginHandleVO(pluginHandleDO, dictOptions);
    }

    private List<PluginHandleVO> buildPluginHandleVO(final List<PluginHandleDO> pluginHandleDOList) {

        List<String> fieldList = pluginHandleDOList.stream()
                .filter(pluginHandleDO -> pluginHandleDO.getDataType() == SELECT_BOX_DATA_TYPE)
                .map(PluginHandleDO::getField)
                .distinct()
                .collect(Collectors.toList());

        Map<String, List<ShenyuDictVO>> shenyuDictMap = CollectionUtils.isNotEmpty(fieldList)
                ? Optional.ofNullable(shenyuDictMapper.findByTypeBatch(fieldList))
                .orElseGet(ArrayList::new)
                .stream()
                .filter(item -> Objects.equals(item.getEnabled(), Boolean.TRUE))
                .map(ShenyuDictVO::buildShenyuDictVO)
                .collect(Collectors.groupingBy(ShenyuDictVO::getType))
                : new HashMap<>(0);

        return pluginHandleDOList.stream()
                .map(pluginHandleDO -> {
                    List<ShenyuDictVO> dictOptions = shenyuDictMap.get(pluginHandleDO.getField());
                    return PluginHandleVO.buildPluginHandleVO(pluginHandleDO, dictOptions);
                })
                .collect(Collectors.toList());
    }
}
