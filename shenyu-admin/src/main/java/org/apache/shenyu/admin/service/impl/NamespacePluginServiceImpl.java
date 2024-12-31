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
import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.mapper.NamespacePluginRelMapper;
import org.apache.shenyu.admin.mapper.PluginHandleMapper;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.NamespacePluginDTO;
import org.apache.shenyu.admin.model.entity.NamespacePluginRelDO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.PluginHandleDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.NamespacePluginQuery;
import org.apache.shenyu.admin.model.result.ConfigImportResult;
import org.apache.shenyu.admin.model.vo.NamespacePluginVO;
import org.apache.shenyu.admin.model.vo.PluginSnapshotVO;
import org.apache.shenyu.admin.model.vo.PluginVO;
import org.apache.shenyu.admin.service.NamespacePluginService;
import org.apache.shenyu.admin.service.configs.ConfigsImportContext;
import org.apache.shenyu.admin.service.publish.NamespacePluginEventPublisher;
import org.apache.shenyu.admin.transfer.PluginTransfer;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.utils.ListUtil;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

@Service
public class NamespacePluginServiceImpl implements NamespacePluginService {
    
    private final NamespacePluginRelMapper namespacePluginRelMapper;
    
    private final NamespacePluginEventPublisher namespacePluginEventPublisher;
    
    private final PluginMapper pluginMapper;
    
    private final SelectorMapper selectorMapper;
    
    private final PluginHandleMapper pluginHandleMapper;
    
    public NamespacePluginServiceImpl(final NamespacePluginRelMapper namespacePluginRelMapper,
                                      final NamespacePluginEventPublisher namespacePluginEventPublisher,
                                      final PluginMapper pluginMapper,
                                      final SelectorMapper selectorMapper,
                                      final PluginHandleMapper pluginHandleMapper) {
        this.namespacePluginRelMapper = namespacePluginRelMapper;
        this.namespacePluginEventPublisher = namespacePluginEventPublisher;
        this.pluginMapper = pluginMapper;
        this.selectorMapper = selectorMapper;
        this.pluginHandleMapper = pluginHandleMapper;
    }
    
    @Override
    public NamespacePluginVO findById(final String id) {
        return namespacePluginRelMapper.selectById(id);
    }
    
    @Override
    public NamespacePluginVO findByNamespaceIdAndPluginId(final String namespaceId, final String pluginId) {
        return namespacePluginRelMapper.selectByPluginIdAndNamespaceId(pluginId, namespaceId);
    }
    
    @Override
    public NamespacePluginVO create(final String namespaceId, final String pluginId) {
        NamespacePluginVO existNamespacePluginVO = namespacePluginRelMapper.selectByPluginIdAndNamespaceId(pluginId, namespaceId);
        if (Objects.nonNull(existNamespacePluginVO)) {
            throw new ShenyuAdminException(AdminConstants.NAMESPACE_PLUGIN_EXIST);
        }
        PluginDO pluginDO = pluginMapper.selectById(pluginId);
        NamespacePluginRelDO namespacePluginRelDO = NamespacePluginRelDO.buildNamespacePluginRelDO(pluginDO, namespaceId);
        namespacePluginRelMapper.insertSelective(namespacePluginRelDO);
        return namespacePluginRelMapper.selectByPluginIdAndNamespaceId(pluginId, namespaceId);
    }
    
    @Override
    @Transactional(rollbackFor = Exception.class)
    public String update(final NamespacePluginDTO namespacePluginDTO) {
        final NamespacePluginVO before = namespacePluginRelMapper.selectById(namespacePluginDTO.getId());
        NamespacePluginRelDO namespacePluginRelDO = NamespacePluginRelDO.buildNamespacePluginRelDO(namespacePluginDTO);
        if (namespacePluginRelMapper.updateSelective(namespacePluginRelDO) > 0) {
            final NamespacePluginVO now = namespacePluginRelMapper.selectById(namespacePluginDTO.getId());
            // publish update event.
            namespacePluginEventPublisher.onUpdated(now, before);
        }
        return ShenyuResultMessage.UPDATE_SUCCESS;
    }
    
    @Override
    @Transactional(rollbackFor = Exception.class)
    public String delete(final List<String> ids) {
        // select namespace plugin by ns plugin rel Ids.
        List<NamespacePluginVO> namespacePluginVOS = this.namespacePluginRelMapper.selectByIds(ids);
        if (CollectionUtils.isEmpty(namespacePluginVOS)) {
            return AdminConstants.SYS_PLUGIN_ID_NOT_EXIST;
        }
        // delete plugins.
        if (this.namespacePluginRelMapper.deleteByIds(ListUtil.map(namespacePluginVOS, NamespacePluginVO::getId)) > 0) {
            // publish deleted event. synchronously delete and link data[selector,rule,condition,resource]
            namespacePluginEventPublisher.onDeleted(namespacePluginVOS);
        }
        return StringUtils.EMPTY;
    }
    
    @Override
    public CommonPager<NamespacePluginVO> listByPage(final NamespacePluginQuery namespacePluginQuery) {
        return PageResultUtils.result(namespacePluginQuery.getPageParameter(), () -> namespacePluginRelMapper.selectByQuery(namespacePluginQuery));
    }
    
    @Override
    public List<PluginData> listAll(final String namespaceId) {
        return ListUtil.map(namespacePluginRelMapper.selectAllByNamespaceId(namespaceId), PluginTransfer.INSTANCE::mapToData);
    }
    
    @Override
    public List<PluginData> listAll() {
        return ListUtil.map(namespacePluginRelMapper.selectAll(), PluginTransfer.INSTANCE::mapToData);
    }
    
    @Override
    public List<NamespacePluginVO> listByNamespaceId(final String namespaceId) {
        return namespacePluginRelMapper.selectAllByNamespaceId(namespaceId);
    }
    
    @Override
    public List<NamespacePluginVO> listAllData(final String namespaceId) {
        List<NamespacePluginVO> namespacePluginVOList = namespacePluginRelMapper.selectByNamespaceId(namespaceId);
        if (CollectionUtils.isEmpty(namespacePluginVOList)) {
            return Lists.newArrayList();
        }
        return namespacePluginVOList;
    }
    
    @Override
    public String enabled(final List<String> ids, final Boolean enabled) {
        List<NamespacePluginVO> namespacePluginVOList = namespacePluginRelMapper.selectByIds(ids);
        if (CollectionUtils.isEmpty(namespacePluginVOList)) {
            return AdminConstants.SYS_PLUGIN_ID_NOT_EXIST;
        }
        namespacePluginVOList.forEach(pluginNsRelDO -> pluginNsRelDO.setEnabled(enabled));
        namespacePluginRelMapper.updateEnableByIdList(ids, enabled);
        // publish change event.
        if (CollectionUtils.isNotEmpty(namespacePluginVOList)) {
            namespacePluginEventPublisher.onEnabled(namespacePluginVOList);
        }
        return StringUtils.EMPTY;
    }
    
    @Override
    public String enabled(final String namespaceId, final List<String> pluginIds, final Boolean enabled) {
        
        List<NamespacePluginVO> namespacePluginList = namespacePluginRelMapper.selectByNamespaceId(namespaceId);
        
        if (CollectionUtils.isEmpty(namespacePluginList)) {
            return StringUtils.EMPTY;
        }
        
        Map<String, NamespacePluginVO> namespacePluginMap = namespacePluginList.stream().collect(Collectors.toMap(NamespacePluginVO::getPluginId, Function.identity()));
        
        List<NamespacePluginVO> updateList = Lists.newArrayList();
        
        for (String pluginId : pluginIds) {
            NamespacePluginVO namespacePluginVO = namespacePluginMap.get(pluginId);
            if (Objects.isNull(namespacePluginVO)) {
                return AdminConstants.SYS_PLUGIN_ID_NOT_EXIST;
            }
            namespacePluginVO.setEnabled(enabled);
            updateList.add(namespacePluginVO);
        }
        namespacePluginRelMapper.updateEnableByNamespaceIdAndPluginIdList(namespaceId, pluginIds, enabled);
        // publish change event.
        if (CollectionUtils.isNotEmpty(updateList)) {
            namespacePluginEventPublisher.onEnabled(updateList);
        }
        return StringUtils.EMPTY;
    }
    
    @Override
    public List<PluginSnapshotVO> activePluginSnapshot(final String namespaceId) {
        List<NamespacePluginVO> namespacePluginVOList = namespacePluginRelMapper.selectByNamespaceId(namespaceId);
        
        if (CollectionUtils.isEmpty(namespacePluginVOList)) {
            return Lists.newArrayList();
        }
        namespacePluginVOList = namespacePluginVOList.stream().filter(PluginVO::getEnabled).toList();
        if (CollectionUtils.isEmpty(namespacePluginVOList)) {
            return Lists.newArrayList();
        }

        List<String> pluginIds = namespacePluginVOList.stream().map(NamespacePluginVO::getPluginId).toList();
        
        List<SelectorDO> selectorDOList = selectorMapper.selectAllByNamespaceId(namespaceId);
        
        Map<String, Integer> selectorCountMap = selectorDOList.stream().collect(Collectors.groupingBy(SelectorDO::getPluginId, Collectors.summingInt(x -> 1)));
        
        List<PluginHandleDO> pluginHandleDOList = pluginHandleMapper.selectByPluginIdList(pluginIds);
        
        Map<String, Integer> pluginHandleCountMap = pluginHandleDOList.stream().collect(Collectors.groupingBy(PluginHandleDO::getPluginId, Collectors.summingInt(x -> 1)));
        
        return namespacePluginVOList.stream().map(namespacePluginVO -> {
            PluginSnapshotVO pluginSnapshotVO = new PluginSnapshotVO();
            pluginSnapshotVO.setId(namespacePluginVO.getPluginId());
            pluginSnapshotVO.setName(namespacePluginVO.getName());
            pluginSnapshotVO.setConfig(namespacePluginVO.getConfig());
            pluginSnapshotVO.setRole(namespacePluginVO.getRole());
            pluginSnapshotVO.setSelectorCount(selectorCountMap.getOrDefault(namespacePluginVO.getPluginId(), 0));
            pluginSnapshotVO.setHandleCount(pluginHandleCountMap.getOrDefault(namespacePluginVO.getPluginId(), 0));
            return pluginSnapshotVO;
        }).collect(Collectors.toList());
        
    }
    
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ConfigImportResult importData(final String namespace, final List<NamespacePluginDTO> namespacePluginList, final ConfigsImportContext context) {
        if (CollectionUtils.isEmpty(namespacePluginList)) {
            return ConfigImportResult.success();
        }
        Map<String, NamespacePluginRelDO> existPluginMap = namespacePluginRelMapper.listByNamespaceId(namespace)
                .stream()
                .filter(Objects::nonNull)
                .collect(Collectors.toMap(NamespacePluginRelDO::getPluginId, x -> x));
        StringBuilder errorMsgBuilder = new StringBuilder();
        int successCount = 0;
        for (NamespacePluginDTO namespacePluginDTO : namespacePluginList) {
            String pluginId = context.getPluginTemplateIdMapping().get(namespacePluginDTO.getPluginId());
            // check plugin base info
            if (existPluginMap.containsKey(pluginId)) {
                errorMsgBuilder
                        .append(pluginId)
                        .append(",");
            } else {
                namespacePluginDTO.setId(UUIDUtils.getInstance().generateShortUuid());
                namespacePluginDTO.setNamespaceId(namespace);
                namespacePluginDTO.setPluginId(pluginId);
                NamespacePluginRelDO namespacePluginRelDO = NamespacePluginRelDO.buildNamespacePluginRelDO(namespacePluginDTO);
                if (namespacePluginRelMapper.insertSelective(namespacePluginRelDO) > 0) {
                    // publish create event. init plugin data
                    successCount++;
                }
            }
        }
        if (StringUtils.isNotEmpty(errorMsgBuilder)) {
            errorMsgBuilder.setLength(errorMsgBuilder.length() - 1);
            return ConfigImportResult
                    .fail(successCount, "import fail plugin: " + errorMsgBuilder);
        }
        return ConfigImportResult.success(successCount);
    }
    
    @Override
    public List<PluginData> listByNamespace(final String namespace) {
        List<NamespacePluginVO> namespacePluginList = namespacePluginRelMapper.selectAllByNamespaceId(namespace);
        if (CollectionUtils.isEmpty(namespacePluginList)) {
            return Lists.newArrayList();
        }
        List<String> pluginIds = namespacePluginList.stream().map(NamespacePluginVO::getPluginId).distinct().collect(Collectors.toList());
        return ListUtil.map(pluginMapper.selectByIds(pluginIds), PluginTransfer.INSTANCE::mapToData);
    }
}
