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
import org.apache.shenyu.admin.aspect.annotation.Pageable;
import org.apache.shenyu.admin.config.properties.DashboardProperties;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.ResourceMapper;
import org.apache.shenyu.admin.model.dto.CreateResourceDTO;
import org.apache.shenyu.admin.model.dto.ResourceDTO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.ResourceDO;
import org.apache.shenyu.admin.model.event.plugin.BatchPluginDeletedEvent;
import org.apache.shenyu.admin.model.event.plugin.PluginCreatedEvent;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.ResourceQuery;
import org.apache.shenyu.admin.model.vo.PermissionMenuVO.MenuInfo;
import org.apache.shenyu.admin.model.vo.ResourceVO;
import org.apache.shenyu.admin.service.ResourceService;
import org.apache.shenyu.admin.service.publish.ResourceEventPublisher;
import org.apache.shenyu.admin.utils.Assert;
import org.apache.shenyu.admin.utils.ResourceUtil;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.enums.AdminResourceEnum;
import org.apache.shenyu.common.utils.ListUtil;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Queue;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.ResourceService}.
 */
@Service
public class ResourceServiceImpl implements ResourceService {

    private final ResourceMapper resourceMapper;

    private final PluginMapper pluginMapper;

    private final ResourceEventPublisher publisher;

    private final DashboardProperties properties;

    public ResourceServiceImpl(final ResourceMapper resourceMapper,
                               final PluginMapper pluginMapper,
                               final ResourceEventPublisher publisher,
                               final DashboardProperties properties) {
        this.resourceMapper = resourceMapper;
        this.pluginMapper = pluginMapper;
        this.publisher = publisher;
        this.properties = properties;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void createPluginResource(final String pluginId) {
        // The database lock serializes resource repair requests for this plugin ID across admin instances.
        final PluginDO plugin = pluginMapper.selectByIdForUpdate(pluginId);
        Assert.notNull(plugin, AdminConstants.SYS_PLUGIN_ID_NOT_EXIST);
        final ResourceDO root = getOrCreatePluginResource(plugin.getName());
        final List<ResourceDO> expected = ResourceUtil.buildPluginDataPermissionResource(root.getId(), plugin.getName());
        final List<String> permissions = ListUtil.map(expected, ResourceDO::getPerms);
        final List<ResourceDO> resources = resourceMapper.selectByPerms(permissions);
        // Database collations may match noncanonical permission values; never ignore these conflicts.
        Assert.isTrue(resources.stream().allMatch(resource -> permissions.contains(resource.getPerms())), AdminConstants.RESOURCE_NAME_IS_EXIST);
        final Map<String, List<ResourceDO>> existing = resources.stream().collect(Collectors.groupingBy(ResourceDO::getPerms));
        final List<ResourceDO> missing = new ArrayList<>();
        for (ResourceDO button : expected) {
            List<ResourceDO> matches = existing.get(button.getPerms());
            if (CollectionUtils.isEmpty(matches)) {
                missing.add(button);
                continue;
            }
            Assert.isTrue(matches.size() == 1, AdminConstants.RESOURCE_NAME_IS_EXIST);
            ResourceDO match = matches.get(0);
            Assert.isTrue(Objects.equals(match.getParentId(), root.getId())
                    && Objects.equals(match.getResourceType(), AdminResourceEnum.THREE_MENU.getCode()), AdminConstants.RESOURCE_NAME_IS_EXIST);
        }
        insertResourceBatch(missing);
    }

    private ResourceDO getOrCreatePluginResource(final String pluginName) {
        final List<ResourceDO> resources = resourceMapper.selectByName(pluginName);
        Assert.isTrue(resources.size() <= 1, AdminConstants.RESOURCE_NAME_IS_EXIST);
        if (resources.isEmpty()) {
            ResourceDO resource = ResourceUtil.buildPluginResource(pluginName);
            createOne(resource);
            return resource;
        }
        final ResourceDO resource = resources.get(0);
        Assert.isTrue(Objects.equals(resource.getParentId(), AdminConstants.RESOURCE_PLUGIN_ID)
                && Objects.equals(resource.getResourceType(), AdminResourceEnum.SECOND_MENU.getCode()), AdminConstants.RESOURCE_NAME_IS_EXIST);
        Assert.isTrue(Objects.equals(resource.getUrl(), AdminConstants.RESOURCE_PLUGIN_URL_PREFIX + pluginName)
                && Objects.equals(resource.getComponent(), pluginName), AdminConstants.RESOURCE_NAME_IS_EXIST);
        return resource;
    }

    /**
     * create Resources.
     *
     * @param resourceDOList list of {@linkplain ResourceDO}
     * @return rows int
     */
    @Override
    public int createResourceBatch(final List<ResourceDO> resourceDOList) {
        return this.insertResourceBatch(resourceDOList);
    }

    /**
     * create Resource.
     *
     * @param createResourceDTO list of {@linkplain CreateResourceDTO}
     * @return rows int
     */
    @Override
    public int create(final CreateResourceDTO createResourceDTO) {
        return this.createOne(ResourceDO.buildResourceDO(createResourceDTO));
    }

    /**
     * update resource.
     *
     * @param resourceDTO {@linkplain ResourceDTO}
     * @return rows int
     */
    @Override
    public int update(final ResourceDTO resourceDTO) {
        final ResourceDO before = resourceMapper.selectById(resourceDTO.getId());
        final ResourceDO resource = ResourceDO.buildResourceDO(resourceDTO);
        final int updateCount = resourceMapper.updateSelective(resource);
        if (updateCount > 0) {
            publisher.onUpdated(resource, before);
        }
        return updateCount;
    }

    /**
     * delete resource info.
     *
     * @param ids {@linkplain List}
     * @return rows int
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public int delete(final List<String> ids) {
        List<ResourceDO> deleteResource = ResourceUtil.getDeleteResourceIds(ids, resourceMapper.selectAll());
        final List<String> deleteIds = ListUtil.map(deleteResource, ResourceDO::getId);
        int deleteCount = resourceMapper.delete(deleteIds);
        if (deleteCount > 0) {
            publisher.onDeleted(deleteResource);
        }
        return deleteCount;
    }

    /**
     * find resource info by id.
     *
     * @param id resource id
     * @return {@linkplain ResourceVO}
     */
    @Override
    public ResourceVO findById(final String id) {
        return ResourceVO.buildResourceVO(resourceMapper.selectById(id));
    }

    /**
     * find resource info by title.
     *
     * @param title resource title
     * @return {@linkplain ResourceVO}
     */
    @Override
    public ResourceVO findByTitle(final String title) {
        return ResourceVO.buildResourceVO(resourceMapper.selectByTitle(title));
    }

    /**
     * find by title.
     *
     * @param titles resource titles
     * @return {@linkplain List}
     */
    @Override
    public List<ResourceVO> listByTitles(final List<String> titles) {
        return ListUtil.map(resourceMapper.selectByTitles(titles), ResourceVO::buildResourceVO);
    }

    /**
     * find page of role by query.
     *
     * @param resourceQuery {@linkplain ResourceQuery}
     * @return {@linkplain CommonPager}
     */
    @Override
    @Pageable
    public CommonPager<ResourceVO> listByPage(final ResourceQuery resourceQuery) {
        return PageResultUtils.result(resourceQuery.getPageParameter(), () -> resourceMapper.selectByQuery(resourceQuery)
                .stream()
                .map(ResourceVO::buildResourceVO)
                .collect(Collectors.toList()));
    }

    /**
     * get menu info.
     *
     * @return {@linkplain List}
     */
    @Override
    public List<MenuInfo> getMenuTree() {
        List<ResourceDO> resources = resourceMapper.selectAll();
        if (CollectionUtils.isEmpty(resources)) {
            return null;
        }

        Set<String> disabledPluginNames = pluginMapper.selectAll().stream()
                .filter(plugin -> !Boolean.TRUE.equals(plugin.getEnabled()))
                .map(PluginDO::getName)
                .collect(Collectors.toSet());

        Set<String> hiddenResourceIds = getDisabledPluginResourceIds(resources, disabledPluginNames);

        // Hide super administrator special privileges and disabled plugin resources.
        List<ResourceVO> resourceVOList = resources.stream()
                .filter(resource -> !properties.getOnlySuperAdminPermission().contains(resource.getPerms()))
                .filter(resource -> !hiddenResourceIds.contains(resource.getId()))
                .map(ResourceVO::buildResourceVO)
                .collect(Collectors.toList());

        return CollectionUtils.isEmpty(resourceVOList) ? null : ResourceUtil.buildMenu(resourceVOList);
    }

    /**
     * Get all resource IDs belonging to disabled plugins, including their descendants.
     *
     * @param resources all resources
     * @param disabledPluginNames names of disabled plugins
     * @return resource IDs to hide
     */
    private Set<String> getDisabledPluginResourceIds(final List<ResourceDO> resources, final Set<String> disabledPluginNames) {
        Set<String> hiddenResourceIds = new HashSet<>();
        if (CollectionUtils.isEmpty(disabledPluginNames)) {
            return hiddenResourceIds;
        }

        Map<String, List<ResourceDO>> childrenByParentId = new HashMap<>();
        for (ResourceDO resource : resources) {
            childrenByParentId.computeIfAbsent(resource.getParentId(), key -> new ArrayList<>()).add(resource);
        }

        Queue<String> resourceIdsToProcess = new ArrayDeque<>();
        for (ResourceDO resource : resources) {
            if (AdminConstants.RESOURCE_PLUGIN_ID.equals(resource.getParentId())
                    && disabledPluginNames.contains(resource.getName())) {
                if (hiddenResourceIds.add(resource.getId())) {
                    resourceIdsToProcess.offer(resource.getId());
                }
            }
        }

        while (!resourceIdsToProcess.isEmpty()) {
            String parentId = resourceIdsToProcess.poll();
            List<ResourceDO> children = childrenByParentId.get(parentId);
            if (CollectionUtils.isNotEmpty(children)) {
                for (ResourceDO child : children) {
                    if (hiddenResourceIds.add(child.getId())) {
                        resourceIdsToProcess.offer(child.getId());
                    }
                }
            }
        }

        return hiddenResourceIds;
    }

    /**
     * get button by parent id.
     *
     * @param id resource id
     * @return {@linkplain List}
     */
    @Override
    public List<ResourceVO> findByParentId(final String id) {
        return resourceMapper.selectByParentId(id)
                .stream()
                .filter(item -> item.getResourceType().equals(AdminResourceEnum.THREE_MENU.getCode()))
                .map(ResourceVO::buildResourceVO)
                .collect(Collectors.toList());
    }

    /**
     * The associated Handle needs to be created synchronously.
     * add plugin and add plugin resource.
     *
     * @param event event
     */
    @Transactional(rollbackFor = Exception.class)
    @EventListener(value = PluginCreatedEvent.class)
    public void onPluginCreated(final PluginCreatedEvent event) {
        Assert.isNull(resourceMapper.nameExisted(event.getPlugin().getName()), AdminConstants.RESOURCE_NAME_IS_EXIST);
        ResourceDO resourceDO = ResourceUtil.buildPluginResource(event.getPlugin().getName());
        this.createOne(resourceDO);
        insertResourceBatch(ResourceUtil.buildPluginDataPermissionResource(resourceDO.getId(), event.getPlugin().getName()));
    }

    /**
     * The associated Handle needs to be deleted synchronously.
     *
     * @param event event
     */
    @Transactional(rollbackFor = Exception.class)
    @EventListener(value = BatchPluginDeletedEvent.class)
    public void onPluginDeleted(final BatchPluginDeletedEvent event) {
        // 5. delete resource & permission.
        final List<ResourceVO> resources = listByTitles(ListUtil.map((List<?>) event.getSource(), s -> ((PluginDO) s).getName()));
        if (CollectionUtils.isNotEmpty(resources)) {
            delete(ListUtil.map(resources, ResourceVO::getId));
        }
    }

    private int createOne(final ResourceDO resource) {
        final int insertCount = resourceMapper.insertSelective(resource);
        if (insertCount > 0) {
            publisher.onCreated(resource);
        }
        return insertCount;
    }

    /**
     * insert Resources.
     *
     * @param resourceDOList list of {@linkplain ResourceDO}
     * @return row int
     */
    private int insertResourceBatch(final List<ResourceDO> resourceDOList) {
        if (CollectionUtils.isEmpty(resourceDOList)) {
            return 0;
        }
        final int insertCount = resourceMapper.insertBatch(resourceDOList);
        if (insertCount > 0) {
            publisher.onCreated(resourceDOList);
        }
        return insertCount;
    }
}
