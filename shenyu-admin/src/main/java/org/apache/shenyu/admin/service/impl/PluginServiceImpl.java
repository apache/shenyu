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

import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.RuleConditionMapper;
import org.apache.shenyu.admin.mapper.RuleMapper;
import org.apache.shenyu.admin.mapper.SelectorConditionMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.service.ResourceService;
import org.apache.shenyu.admin.model.dto.PluginDTO;
import org.apache.shenyu.admin.model.dto.ResourceDTO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.ResourceDO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.PluginQuery;
import org.apache.shenyu.admin.model.query.RuleConditionQuery;
import org.apache.shenyu.admin.model.query.RuleQuery;
import org.apache.shenyu.admin.model.query.SelectorConditionQuery;
import org.apache.shenyu.admin.model.query.SelectorQuery;
import org.apache.shenyu.admin.service.PluginService;
import org.apache.shenyu.admin.transfer.PluginTransfer;
import org.apache.shenyu.admin.model.vo.PluginVO;
import org.apache.shenyu.admin.model.vo.ResourceVO;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.AdminPluginOperateEnum;
import org.apache.shenyu.common.enums.AdminResourceEnum;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.enums.PluginRoleEnum;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.sql.Timestamp;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.PluginService}.
 */
@RequiredArgsConstructor
@Service
public class PluginServiceImpl implements PluginService {

    private final PluginMapper pluginMapper;

    private final SelectorMapper selectorMapper;

    private final SelectorConditionMapper selectorConditionMapper;

    private final RuleMapper ruleMapper;

    private final RuleConditionMapper ruleConditionMapper;

    private final ApplicationEventPublisher eventPublisher;

    private final ResourceService resourceService;

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
        for (String id : ids) {
            PluginDO pluginDO = pluginMapper.selectById(id);
            if (Objects.isNull(pluginDO)) {
                return AdminConstants.SYS_PLUGIN_ID_NOT_EXIST;
            }
            // if sys plugin not delete
            if (pluginDO.getRole().equals(PluginRoleEnum.SYS.getCode())) {
                return AdminConstants.SYS_PLUGIN_NOT_DELETE;
            }
            pluginMapper.delete(id);
            deletePluginDataFromResourceAndPermission(pluginDO.getName());
            final List<SelectorDO> selectorDOList = selectorMapper.selectByQuery(new SelectorQuery(id, null, null));
            selectorDOList.forEach(selectorDO -> {
                final List<RuleDO> ruleDOS = ruleMapper.selectByQuery(new RuleQuery(selectorDO.getId(), null, null));
                ruleDOS.forEach(ruleDO -> {
                    ruleMapper.delete(ruleDO.getId());
                    ruleConditionMapper.deleteByQuery(new RuleConditionQuery(ruleDO.getId()));
                });
                selectorMapper.delete(selectorDO.getId());
                selectorConditionMapper.deleteByQuery(new SelectorConditionQuery(selectorDO.getId()));
            });
            // publish change event.
            eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.PLUGIN, DataEventTypeEnum.DELETE,
                    Collections.singletonList(PluginTransfer.INSTANCE.mapToData(pluginDO))));
        }
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
        for (String id : ids) {
            PluginDO pluginDO = pluginMapper.selectById(id);
            if (Objects.isNull(pluginDO)) {
                return AdminConstants.SYS_PLUGIN_ID_NOT_EXIST;
            }
            pluginDO.setDateUpdated(new Timestamp(System.currentTimeMillis()));
            pluginDO.setEnabled(enabled);
            pluginMapper.updateEnable(pluginDO);

            // publish change event.
            eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.PLUGIN, DataEventTypeEnum.UPDATE,
                    Collections.singletonList(PluginTransfer.INSTANCE.mapToData(pluginDO))));
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
    public CommonPager<PluginVO> listByPage(final PluginQuery pluginQuery) {
        return PageResultUtils.result(pluginQuery.getPageParameter(),
            () -> pluginMapper.countByQuery(pluginQuery),
            () -> pluginMapper.selectByQuery(pluginQuery).stream().map(PluginVO::buildPluginVO).collect(Collectors.toList()));
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
    public String selectIdByName(final String name) {
        PluginDO pluginDO = pluginMapper.selectByName(name);
        Objects.requireNonNull(pluginDO);
        return pluginDO.getId();
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
     * delete plugin resource from resource and permission.
     *
     * @param pluginName plugin name
     */
    private void deletePluginDataFromResourceAndPermission(final String pluginName) {
        ResourceVO resourceVO = resourceService.findByTitle(pluginName);
        if (!ObjectUtils.isEmpty(resourceVO)) {
            resourceService.delete(Collections.singletonList(resourceVO.getId()));
        }
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
     * @param parentId parent menu id
     * @param pluginName plugin name
     * @param configGroupEnum {@linkplain ConfigGroupEnum}
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
