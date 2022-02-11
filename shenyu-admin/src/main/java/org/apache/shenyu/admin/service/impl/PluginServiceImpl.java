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
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.PluginHandleMapper;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.RuleConditionMapper;
import org.apache.shenyu.admin.mapper.RuleMapper;
import org.apache.shenyu.admin.mapper.SelectorConditionMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.PluginDTO;
import org.apache.shenyu.admin.model.dto.ResourceDTO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.ResourceDO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.PluginQuery;
import org.apache.shenyu.admin.model.vo.PluginVO;
import org.apache.shenyu.admin.model.vo.ResourceVO;
import org.apache.shenyu.admin.service.PluginService;
import org.apache.shenyu.admin.service.ResourceService;
import org.apache.shenyu.admin.transfer.PluginTransfer;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.AdminPluginOperateEnum;
import org.apache.shenyu.common.enums.AdminResourceEnum;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.PluginService}.
 */
@Service
public class PluginServiceImpl implements PluginService {

    private final PluginMapper pluginMapper;

    private final PluginHandleMapper pluginHandleMapper;

    private final SelectorMapper selectorMapper;

    private final SelectorConditionMapper selectorConditionMapper;

    private final RuleMapper ruleMapper;

    private final RuleConditionMapper ruleConditionMapper;

    private final ApplicationEventPublisher eventPublisher;

    private final ResourceService resourceService;

    public PluginServiceImpl(final PluginMapper pluginMapper,
                             final PluginHandleMapper pluginHandleMapper,
                             final SelectorMapper selectorMapper,
                             final SelectorConditionMapper selectorConditionMapper,
                             final RuleMapper ruleMapper,
                             final RuleConditionMapper ruleConditionMapper,
                             final ApplicationEventPublisher eventPublisher,
                             final ResourceService resourceService) {
        this.pluginMapper = pluginMapper;
        this.pluginHandleMapper = pluginHandleMapper;
        this.selectorMapper = selectorMapper;
        this.selectorConditionMapper = selectorConditionMapper;
        this.ruleMapper = ruleMapper;
        this.ruleConditionMapper = ruleConditionMapper;
        this.eventPublisher = eventPublisher;
        this.resourceService = resourceService;
    }

    /**
     * create or update plugin.
     *
     * @param pluginDTO {@linkplain PluginDTO}
     * @return rows
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public String createOrUpdate(final PluginDTO pluginDTO) {
        final String msg = checkData(pluginDTO);
        if (StringUtils.isNoneBlank(msg)) {
            return msg;
        }

        PluginDO pluginDO = PluginDO.buildPluginDO(pluginDTO);
        DataEventTypeEnum eventType = DataEventTypeEnum.CREATE;
        if (StringUtils.isBlank(pluginDTO.getId())) {
            insertPluginDataToResource(pluginDTO);
            pluginMapper.insertSelective(pluginDO);
        } else {
            eventType = DataEventTypeEnum.UPDATE;
            pluginMapper.updateSelective(pluginDO);
        }

        // publish change event.
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.PLUGIN, eventType,
                Collections.singletonList(PluginTransfer.INSTANCE.mapToData(pluginDO))));
        return StringUtils.EMPTY;
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
        // 1. select plugin id.
        List<PluginDO> plugins = Optional.ofNullable(this.pluginMapper.selectByIds(ids))
                .orElse(Collections.emptyList());
        final List<String> pluginIds = plugins.stream()
                .map(PluginDO::getId).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(pluginIds)) {
            return AdminConstants.SYS_PLUGIN_ID_NOT_EXIST;
        }

        // 2. delete plugins.
        this.pluginMapper.deleteByIds(pluginIds);
        // 3. delete plugin handle.
        this.pluginHandleMapper.deleteByPluginIds(pluginIds);

        // 4. all selectors.
        final List<String> selectorIds = Optional.ofNullable(this.selectorMapper.findByPluginIds(pluginIds))
                .orElse(Collections.emptyList())
                .stream().map(SelectorDO::getId).collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(selectorIds)) {
            // delete all selectors
            this.selectorMapper.deleteByIds(selectorIds);
            // delete all selector conditions
            this.selectorConditionMapper.deleteBySelectorIds(selectorIds);
            // delete all rules
            final List<String> ruleIds = Optional.ofNullable(this.ruleMapper.findBySelectorIds(selectorIds))
                    .orElse(Collections.emptyList())
                    .stream()
                    .map(RuleDO::getId)
                    .collect(Collectors.toList());
            if (CollectionUtils.isNotEmpty(ruleIds)) {
                this.ruleMapper.deleteByIds(ruleIds);
                // delete all rule conditions
                this.ruleConditionMapper.deleteByRuleIds(ruleIds);
            }
        }

        // 5. delete resource & permission.
        final List<ResourceVO> resources = this.resourceService.listByTitles(plugins.stream()
                .map(PluginDO::getName).collect(Collectors.toList()));
        if (CollectionUtils.isNotEmpty(resources)) {
            this.resourceService.delete(resources.stream().map(ResourceVO::getId).collect(Collectors.toList()));
        }

        // 6. publish change event.
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.PLUGIN, DataEventTypeEnum.DELETE,
                plugins.stream().map(PluginTransfer.INSTANCE::mapToData).collect(Collectors.toList())));
        return StringUtils.EMPTY;
    }

    /**
     * plugin enabled.
     *
     * @param ids     the ids
     * @param enabled the enable
     * @return String
     */
    @Override
    public String enabled(final List<String> ids, final Boolean enabled) {

        if (CollectionUtils.isEmpty(ids)) {
            return AdminConstants.SYS_PLUGIN_ID_NOT_EXIST;
        }
        List<PluginDO> plugins = Optional.ofNullable(pluginMapper.selectByIds(ids)).orElseGet(ArrayList::new);
        Set<String> idSet = new HashSet<>(Optional.ofNullable(ids).orElseGet(ArrayList::new));
        if (idSet.size() > plugins.size()) {
            return AdminConstants.SYS_PLUGIN_ID_NOT_EXIST;
        }
        plugins.forEach(pluginDO -> pluginDO.setEnabled(enabled));
        pluginMapper.updateEnableByIdSet(idSet, enabled);
        // publish change event.
        if (CollectionUtils.isNotEmpty(plugins)) {
            eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.PLUGIN, DataEventTypeEnum.UPDATE,
                    plugins.stream().map(PluginTransfer.INSTANCE::mapToData).collect(Collectors.toList())));
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
                .stream().map(PluginVO::buildPluginVO).collect(Collectors.toList()));
    }

    /**
     * query all plugin.
     *
     * @return {@linkplain List}
     */
    @Override
    public List<PluginData> listAll() {
        return pluginMapper.selectAll().stream()
                .map(PluginTransfer.INSTANCE::mapToData)
                .collect(Collectors.toList());
    }

    @Override
    public List<PluginData> listAllNotInResource() {
        return pluginMapper.listAllNotInResource().stream()
                .map(PluginTransfer.INSTANCE::mapToData)
                .collect(Collectors.toList());
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
     * check plugin Data integrity.
     *
     * @param pluginDTO {@linkplain PluginDTO}
     * @return result description
     */
    private String checkData(final PluginDTO pluginDTO) {
        final PluginDO exist = pluginMapper.selectByName(pluginDTO.getName());
        if (StringUtils.isBlank(pluginDTO.getId())) {
            if (Objects.nonNull(exist)) {
                return AdminConstants.PLUGIN_NAME_IS_EXIST;
            }
        } else {
            if (Objects.isNull(exist) || !exist.getId().equals(pluginDTO.getId())) {
                return AdminConstants.PLUGIN_NAME_NOT_EXIST;
            }
        }
        return StringUtils.EMPTY;
    }

    /**
     * add plugin and add plugin resource.
     *
     * @param pluginDTO {@linkplain PluginDTO}
     */
    private void insertPluginDataToResource(final PluginDTO pluginDTO) {
        ResourceDO resourceDO = ResourceDO.buildResourceDO(ResourceDTO.builder()
                .parentId(AdminConstants.RESOURCE_PLUGIN_ID)
                .title(pluginDTO.getName())
                .name(pluginDTO.getName())
                .url(AdminConstants.RESOURCE_PLUGIN_URL_PREFIX + pluginDTO.getName())
                .component(pluginDTO.getName())
                .resourceType(AdminResourceEnum.SECOND_MENU.getCode())
                .sort(0)
                .icon(AdminConstants.RESOURCE_PLUGIN_DEFAULT_ICON)
                .isLeaf(Boolean.FALSE).isRoute(0).status(1).perms(StringUtils.EMPTY).build());
        insertPluginMenuResource(resourceDO);
        insertPluginButtonResource(resourceDO.getId(), pluginDTO.getName(), ConfigGroupEnum.SELECTOR, AdminPluginOperateEnum.ADD);
        insertPluginButtonResource(resourceDO.getId(), pluginDTO.getName(), ConfigGroupEnum.SELECTOR, AdminPluginOperateEnum.DELETE);
        insertPluginButtonResource(resourceDO.getId(), pluginDTO.getName(), ConfigGroupEnum.SELECTOR, AdminPluginOperateEnum.EDIT);
        insertPluginButtonResource(resourceDO.getId(), pluginDTO.getName(), ConfigGroupEnum.SELECTOR, AdminPluginOperateEnum.QUERY);
        insertPluginButtonResource(resourceDO.getId(), pluginDTO.getName(), ConfigGroupEnum.RULE, AdminPluginOperateEnum.ADD);
        insertPluginButtonResource(resourceDO.getId(), pluginDTO.getName(), ConfigGroupEnum.RULE, AdminPluginOperateEnum.DELETE);
        insertPluginButtonResource(resourceDO.getId(), pluginDTO.getName(), ConfigGroupEnum.RULE, AdminPluginOperateEnum.EDIT);
        insertPluginButtonResource(resourceDO.getId(), pluginDTO.getName(), ConfigGroupEnum.RULE, AdminPluginOperateEnum.QUERY);
        insertPluginButtonResource(resourceDO.getId(), pluginDTO.getName(), ConfigGroupEnum.PLUGIN, AdminPluginOperateEnum.SYNCHRONIZE);
    }

    /**
     * insert Resource Data.
     *
     * @param resourceDO {@linkplain ResourceDO}
     */
    private void insertPluginMenuResource(final ResourceDO resourceDO) {
        resourceService.createResource(resourceDO);
    }

    /**
     * insert Plugin Selector Button Resource.
     *
     * @param parentId               parent menu id
     * @param pluginName             plugin name
     * @param configGroupEnum        {@linkplain ConfigGroupEnum}
     * @param adminPluginOperateEnum {@linkplain AdminPluginOperateEnum}
     */
    private void insertPluginButtonResource(final String parentId, final String pluginName,
                                            final ConfigGroupEnum configGroupEnum, final AdminPluginOperateEnum adminPluginOperateEnum) {
        ResourceDO resourceDO = ResourceDO.buildResourceDO(ResourceDTO.builder()
                .parentId(parentId)
                .name(StringUtils.EMPTY).url(StringUtils.EMPTY).component(StringUtils.EMPTY)
                .resourceType(AdminResourceEnum.THREE_MENU.getCode())
                .isLeaf(Boolean.TRUE).status(1).sort(0).icon(StringUtils.EMPTY).isRoute(0).build());
        switch (configGroupEnum) {
            case SELECTOR:
                resourceDO.setTitle("SHENYU.BUTTON.PLUGIN." + ConfigGroupEnum.SELECTOR.name() + "." + adminPluginOperateEnum.name());
                resourceDO.setPerms("plugin:" + pluginName + "Selector:" + adminPluginOperateEnum.getName());
                break;
            case RULE:
                resourceDO.setTitle("SHENYU.BUTTON.PLUGIN." + ConfigGroupEnum.RULE.name() + "." + adminPluginOperateEnum.name());
                resourceDO.setPerms("plugin:" + pluginName + "Rule:" + adminPluginOperateEnum.getName());
                break;
            case PLUGIN:
                resourceDO.setTitle("SHENYU.BUTTON.PLUGIN." + adminPluginOperateEnum.name());
                resourceDO.setPerms("plugin:" + pluginName + ":" + adminPluginOperateEnum.getName());
                break;
            default:
                break;
        }

        insertPluginMenuResource(resourceDO);
    }
}
