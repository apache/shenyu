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
import org.apache.shenyu.admin.utils.ListUtil;
import org.apache.shenyu.admin.utils.ResourceUtil;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.enums.AdminResourceEnum;

import java.util.List;
import java.util.stream.Collectors;

import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.ResourceService}.
 */
@Service
public class ResourceServiceImpl implements ResourceService {
    
    private final ResourceMapper resourceMapper;
    
    private final ResourceEventPublisher publisher;
    
    private final DashboardProperties properties;
    
    public ResourceServiceImpl(final ResourceMapper resourceMapper,
                               final ResourceEventPublisher publisher,
                               final DashboardProperties properties) {
        this.resourceMapper = resourceMapper;
        this.publisher = publisher;
        this.properties = properties;
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
     * @return {@linkplain ResourceVO}
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
        // Hide super administrator special privileges
        List<ResourceVO> resourceVOList = resourceMapper.selectAll()
                .stream()
                .filter(r -> !properties.getOnlySuperAdminPermission().contains(r.getPerms()))
                .map(ResourceVO::buildResourceVO)
                .collect(Collectors.toList());
        return CollectionUtils.isEmpty(resourceVOList) ? null : ResourceUtil.buildMenu(resourceVOList);
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
