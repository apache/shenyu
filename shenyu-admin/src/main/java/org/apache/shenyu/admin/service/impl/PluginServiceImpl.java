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
import org.apache.shenyu.admin.aspect.annotation.Pageable;
import org.apache.shenyu.admin.mapper.NamespacePluginRelMapper;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.model.dto.PluginDTO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.event.plugin.PluginCreatedEvent;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.PluginQuery;
import org.apache.shenyu.admin.model.query.PluginQueryCondition;
import org.apache.shenyu.admin.model.result.ConfigImportResult;
import org.apache.shenyu.admin.model.vo.PluginSnapshotVO;
import org.apache.shenyu.admin.model.vo.PluginVO;
import org.apache.shenyu.admin.service.PluginHandleService;
import org.apache.shenyu.admin.service.PluginService;
import org.apache.shenyu.admin.service.configs.ConfigsImportContext;
import org.apache.shenyu.admin.service.publish.PluginEventPublisher;
import org.apache.shenyu.admin.transfer.PluginTransfer;
import org.apache.shenyu.admin.utils.Assert;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.JarDependencyUtils;
import org.apache.shenyu.common.utils.ListUtil;
import org.apache.shenyu.common.utils.LogUtils;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.opengauss.util.Base64;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link PluginService}.
 */
@Service
public class PluginServiceImpl implements PluginService {

    /**
     * logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(PluginServiceImpl.class);

    private final PluginMapper pluginMapper;

    private final PluginEventPublisher pluginEventPublisher;

    private final PluginHandleService pluginHandleService;

    private final NamespacePluginRelMapper namespacePluginRelMapper;

    public PluginServiceImpl(final PluginMapper pluginMapper,
                             final PluginEventPublisher pluginEventPublisher,
                             final PluginHandleService pluginHandleService,
                             final NamespacePluginRelMapper namespacePluginRelMapper) {
        this.pluginMapper = pluginMapper;
        this.pluginEventPublisher = pluginEventPublisher;
        this.pluginHandleService = pluginHandleService;
        this.namespacePluginRelMapper = namespacePluginRelMapper;
    }

    @Override
    public List<PluginVO> searchByCondition(final PluginQueryCondition condition) {
        condition.init();
        return pluginMapper.searchByCondition(condition);
    }

    /**
     * create or update plugin.
     *
     * @param pluginDTO {@linkplain PluginDTO}
     * @return rows
     */
    @Override
    public String createOrUpdate(final PluginDTO pluginDTO) {
        return StringUtils.isBlank(pluginDTO.getId()) ? this.create(pluginDTO) : this.update(pluginDTO);
    }

    /**
     * create plugin resource.
     *
     * @param pluginDTO the plugin dto
     * @return result message
     */
    @Override
    public String createPluginResource(final PluginDTO pluginDTO) {
        pluginEventPublisher.onCreated(PluginDO.buildPluginDO(pluginDTO));
        return ShenyuResultMessage.CREATE_SUCCESS;
    }

    /**
     * delete plugins.
     *
     * @param ids primary key.
     * @return rows
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public String delete(final List<String> ids) {
        // select plugin id.
        List<PluginDO> plugins = this.pluginMapper.selectByIds(ids);
        if (CollectionUtils.isEmpty(plugins)) {
            return AdminConstants.SYS_PLUGIN_ID_NOT_EXIST;
        }
        Optional<PluginDO> exist = plugins.stream()
                .filter(value -> Objects.nonNull(this.namespacePluginRelMapper.selectByPluginId(value.getId())))
                .findAny();
        if (exist.isPresent()) {
            return AdminConstants.NAMESPACE_PLUGIN_NOT_DELETE;
        }
        // delete plugins.
        if (this.pluginMapper.deleteByIds(ListUtil.map(plugins, PluginDO::getId)) > 0) {
            // publish deleted event. synchronously delete and link data[selector,rule,condition,resource]
            pluginEventPublisher.onDeleted(plugins);
        }
        return StringUtils.EMPTY;
    }

    /**
     * plugin enabled.
     *
     * @param ids     the ids
     * @param enabled enable
     * @return String
     */
    @Override
    public String enabled(final List<String> ids, final Boolean enabled) {
        List<PluginDO> plugins = pluginMapper.selectByIds(ids);
        if (CollectionUtils.isEmpty(plugins)) {
            return AdminConstants.SYS_PLUGIN_ID_NOT_EXIST;
        }
        plugins.forEach(pluginDO -> pluginDO.setEnabled(enabled));
        pluginMapper.updateEnableByIdList(ids, enabled);
        // publish change event.
        if (CollectionUtils.isNotEmpty(plugins)) {
            pluginEventPublisher.onEnabled(plugins);
        }
        return StringUtils.EMPTY;
    }

    /**
     * find plugin by id.
     *
     * @param id primary key.
     * @return {@linkplain PluginVO}
     */
    @Override
    public PluginVO findById(final String id) {
        return PluginVO.buildPluginVO(pluginMapper.selectById(id));
    }

    /**
     * find page of plugin by query.
     *
     * @param pluginQuery {@linkplain PluginQuery}
     * @return {@linkplain CommonPager}
     */
    @Override
    @Pageable
    public CommonPager<PluginVO> listByPage(final PluginQuery pluginQuery) {
        return PageResultUtils.result(pluginQuery.getPageParameter(), () -> pluginMapper.selectByQuery(pluginQuery)
                .stream()
                .map(PluginVO::buildPluginVO)
                .collect(Collectors.toList()));
    }

    /**
     * query all plugin.
     *
     * @return {@linkplain List}
     */
    @Override
    public List<PluginData> listAll() {
        return ListUtil.map(pluginMapper.selectAll(), PluginTransfer.INSTANCE::mapToData);
    }
    
    @Override
    public List<PluginVO> listAllData() {
        return pluginMapper.selectAll()
                .stream()
                .filter(Objects::nonNull)
                .map(PluginVO::buildPluginVO).collect(Collectors.toList());
    }
    
    @Override
    public List<PluginData> listAllNotInResource() {
        return ListUtil.map(pluginMapper.listAllNotInResource(), PluginTransfer.INSTANCE::mapToData);
    }

    @Override
    public String selectIdByName(final String name) {
        PluginDO pluginDO = pluginMapper.selectByName(name);
        Objects.requireNonNull(pluginDO);
        return pluginDO.getId();
    }

    /**
     * Find by name plugin do.
     *
     * @param name the name
     * @return the plugin do
     */
    @Override
    public PluginDO findByName(final String name) {
        return pluginMapper.selectByName(name);
    }

    /**
     * activate plugin snapshot.
     *
     * @return List of plugins snapshot
     */
    @Override
    public List<PluginSnapshotVO> activePluginSnapshot() {
        return pluginMapper.activePluginSnapshot(SessionUtil.isAdmin() ? null : SessionUtil.visitor().getUserId());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ConfigImportResult importData(final List<PluginDTO> pluginList, final ConfigsImportContext context) {
        if (CollectionUtils.isEmpty(pluginList)) {
            return ConfigImportResult.success();
        }
        Map<String, PluginDO> existPluginMap = pluginMapper.selectAll()
                .stream()
                .filter(Objects::nonNull)
                .collect(Collectors.toMap(PluginDO::getName, x -> x));
        StringBuilder errorMsgBuilder = new StringBuilder();
        int successCount = 0;
        for (PluginDTO pluginDTO : pluginList) {
            String pluginName = pluginDTO.getName();
            // check plugin base info
            if (existPluginMap.containsKey(pluginName)) {
                errorMsgBuilder
                        .append(pluginName)
                        .append(",");
                Optional.ofNullable(context).ifPresent(c -> c.getPluginTemplateIdMapping().put(pluginDTO.getId(), existPluginMap.get(pluginName).getId()));
            } else {
                String pluginId = UUIDUtils.getInstance().generateShortUuid();
                Optional.ofNullable(context).ifPresent(c -> c.getPluginTemplateIdMapping().put(pluginDTO.getId(), pluginId));
                pluginDTO.setId(pluginId);
                PluginDO pluginDO = PluginDO.buildPluginDO(pluginDTO);
                if (pluginMapper.insertSelective(pluginDO) > 0) {
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
    
    /**
     * create plugin.<br>
     * insert plugin and insert plugin data.
     *
     * @param pluginDTO plugin info
     * @return success is empty
     * @see ResourceServiceImpl#onPluginCreated(PluginCreatedEvent)
     * @see PluginCreatedEvent
     */
    private String create(final PluginDTO pluginDTO) {
        Assert.isNull(pluginMapper.nameExisted(pluginDTO.getName()), "create" + AdminConstants.PLUGIN_NAME_IS_EXIST + pluginDTO.getName());
        if (Objects.nonNull(pluginDTO.getFile())) {
            Assert.isTrue(checkFile(Base64.decode(pluginDTO.getFile())), AdminConstants.THE_PLUGIN_JAR_FILE_IS_NOT_CORRECT_OR_EXCEEDS_16_MB);
        }
        PluginDO pluginDO = PluginDO.buildPluginDO(pluginDTO);
        if (pluginMapper.insertSelective(pluginDO) > 0) {
            // publish create event. init plugin data
            pluginEventPublisher.onCreated(pluginDO);
        }
        return ShenyuResultMessage.CREATE_SUCCESS;
    }


    /**
     * update plugin.<br>
     *
     * @param pluginDTO plugin
     * @return success is empty
     */
    private String update(final PluginDTO pluginDTO) {
        Assert.isNull(pluginMapper.nameExistedExclude(pluginDTO.getName(), Collections.singletonList(pluginDTO.getId())), AdminConstants.PLUGIN_NAME_IS_EXIST + pluginDTO.getName());
        if (Objects.nonNull(pluginDTO.getFile())) {
            Assert.isTrue(checkFile(Base64.decode(pluginDTO.getFile())), AdminConstants.THE_PLUGIN_JAR_FILE_IS_NOT_CORRECT_OR_EXCEEDS_16_MB);
        }
        final PluginDO before = pluginMapper.selectById(pluginDTO.getId());
        PluginDO pluginDO = PluginDO.buildPluginDO(pluginDTO);
        if (pluginMapper.updateSelective(pluginDO) > 0) {
            // publish update event.
            pluginEventPublisher.onUpdated(pluginDO, before);
        }
        return ShenyuResultMessage.UPDATE_SUCCESS;
    }

    /**
     * check jar.
     *
     * @param file jar file
     * @return true is right
     */
    private boolean checkFile(final byte[] file) {
        try {
            // Check if the file size is greater than 16 megabytes
            if (file.length > 16 * Constants.BYTES_PER_MB) {
                LogUtils.warn(LOG, "File size is {}MB larger than 16MB", file.length / Constants.BYTES_PER_MB);
                return false;
            }
            Set<String> dependencyTree = JarDependencyUtils.getDependencyTree(file);
            return dependencyTree.contains(AdminConstants.PLUGIN_ABSTRACT_PATH) || dependencyTree.contains(AdminConstants.PLUGIN_INTERFACE_PATH);
        } catch (Exception e) {
            LogUtils.error(LOG, "check plugin jar error:{}", e.getMessage());
            throw new ShenyuException(e);
        }
    }
}
