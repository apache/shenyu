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
import org.apache.shenyu.admin.mapper.PluginNsRelMapper;
import org.apache.shenyu.admin.model.dto.PluginDTO;
import org.apache.shenyu.admin.model.dto.PluginNamespaceDTO;
import org.apache.shenyu.admin.model.entity.PluginNsRelDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.PluginNamespaceQuery;
import org.apache.shenyu.admin.model.result.ConfigImportResult;
import org.apache.shenyu.admin.model.vo.PluginHandleVO;
import org.apache.shenyu.admin.model.vo.PluginNamespaceVO;
import org.apache.shenyu.admin.model.vo.PluginSnapshotVO;
import org.apache.shenyu.admin.service.PluginHandleService;
import org.apache.shenyu.admin.service.PluginNamespaceService;
import org.apache.shenyu.admin.service.publish.PluginNamespaceEventPublisher;
import org.apache.shenyu.admin.transfer.PluginTransfer;
import org.apache.shenyu.admin.utils.Assert;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.utils.ListUtil;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class PluginNamespaceServiceImpl implements PluginNamespaceService {

    private final PluginNsRelMapper pluginNsRelMapper;

    private final PluginHandleService pluginHandleService;

    private final PluginNamespaceEventPublisher pluginNamespaceEventPublisher;

    public PluginNamespaceServiceImpl(final PluginNsRelMapper pluginNsRelMapper,
                                      final PluginHandleService pluginHandleService,
                                      final PluginNamespaceEventPublisher pluginNamespaceEventPublisher) {
        this.pluginNsRelMapper = pluginNsRelMapper;
        this.pluginHandleService = pluginHandleService;
        this.pluginNamespaceEventPublisher = pluginNamespaceEventPublisher;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public String update(final PluginNamespaceDTO pluginNamespaceDTO) {
        Assert.isNull(pluginNsRelMapper.nameExistedExclude(pluginNamespaceDTO.getName(),
                Collections.singletonList(pluginNamespaceDTO.getPluginId()),
                pluginNamespaceDTO.getNamespaceId()), AdminConstants.PLUGIN_NAME_IS_EXIST);
        final PluginNamespaceVO before = pluginNsRelMapper.selectById(pluginNamespaceDTO.getPluginId(),
                pluginNamespaceDTO.getNamespaceId());
        PluginNsRelDO pluginNsRelDO = PluginNsRelDO.buildPluginNsRelDO(pluginNamespaceDTO);
        if (pluginNsRelMapper.updateSelective(pluginNsRelDO) > 0) {
            final PluginNamespaceVO now = pluginNsRelMapper.selectById(pluginNamespaceDTO.getPluginId(), pluginNamespaceDTO.getNamespaceId());
            // publish update event.
            pluginNamespaceEventPublisher.onUpdated(now, before);
        }
        return ShenyuResultMessage.UPDATE_SUCCESS;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public String delete(final List<String> pluginIds, final String namespaceId) {
        // select plugin id.
        List<PluginNamespaceVO> pluginNamespaceVOS = this.pluginNsRelMapper.selectByIds(pluginIds, namespaceId);
        if (CollectionUtils.isEmpty(pluginNamespaceVOS)) {
            return AdminConstants.SYS_PLUGIN_ID_NOT_EXIST;
        }
        // delete plugins.
        if (this.pluginNsRelMapper.deleteByIds(ListUtil.map(pluginNamespaceVOS, PluginNamespaceVO::getPluginId), namespaceId) > 0) {
            // publish deleted event. synchronously delete and link data[selector,rule,condition,resource]
            pluginNamespaceEventPublisher.onDeleted(pluginNamespaceVOS);
        }
        return StringUtils.EMPTY;
    }

    @Override
    public PluginNamespaceVO findById(final String pluginId, final String namespaceId) {
        return this.pluginNsRelMapper.selectById(pluginId, namespaceId);
    }

    @Override
    public CommonPager<PluginNamespaceVO> listByPage(final PluginNamespaceQuery pluginNamespaceQuery) {
        return PageResultUtils.result(pluginNamespaceQuery.getPageParameter(), () -> pluginNsRelMapper.selectByQuery(pluginNamespaceQuery));
    }

    @Override
    public List<PluginData> listAll(final String namespaceId) {
        return ListUtil.map(pluginNsRelMapper.selectAll(namespaceId), PluginTransfer.INSTANCE::mapToData);
    }

    @Override
    public List<PluginData> listAll() {
        return ListUtil.map(pluginNsRelMapper.selectAll(), PluginTransfer.INSTANCE::mapToData);
    }

    @Override
    public List<PluginNamespaceVO> listAllData(final String namespaceId) {
        Map<String, List<PluginHandleVO>> pluginHandleMap = pluginHandleService.listAllData()
                .stream()
                .collect(Collectors.groupingBy(PluginHandleVO::getPluginId));

        return pluginNsRelMapper.selectAll(namespaceId)
                .stream()
                .filter(Objects::nonNull)
                .peek(pluginNamespaceVO -> {
                    List<PluginHandleVO> pluginHandleList = Optional
                            .ofNullable(pluginHandleMap.get(pluginNamespaceVO.getPluginId()))
                            .orElse(Lists.newArrayList())
                            .stream()
                            // to make less volume of export data
                            .peek(x -> x.setDictOptions(null))
                            .collect(Collectors.toList());
                    pluginNamespaceVO.setPluginHandleList(pluginHandleList);
                }).collect(Collectors.toList());
    }

    @Override
    public String enabled(final List<String> ids, final Boolean enabled, final String namespaceId) {
        List<PluginNamespaceVO> pluginNamespaceVOList = pluginNsRelMapper.selectByIds(ids, namespaceId);
        if (CollectionUtils.isEmpty(pluginNamespaceVOList)) {
            return AdminConstants.SYS_PLUGIN_ID_NOT_EXIST;
        }
        pluginNamespaceVOList.forEach(pluginNsRelDO -> pluginNsRelDO.setEnabled(enabled));
        pluginNsRelMapper.updateEnableByIdList(ids, enabled);
        // publish change event.
        if (CollectionUtils.isNotEmpty(pluginNamespaceVOList)) {
            pluginNamespaceEventPublisher.onEnabled(pluginNamespaceVOList);
        }
        return StringUtils.EMPTY;
    }

    @Override
    public List<PluginSnapshotVO> activePluginSnapshot() {
        return null;
    }

    @Override
    public ConfigImportResult importData(final List<PluginDTO> pluginList) {
        return null;
    }
}
