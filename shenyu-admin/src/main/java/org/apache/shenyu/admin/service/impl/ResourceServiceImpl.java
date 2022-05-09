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
import org.apache.shenyu.admin.mapper.PermissionMapper;
import org.apache.shenyu.admin.mapper.ResourceMapper;
import org.apache.shenyu.admin.model.dto.PermissionDTO;
import org.apache.shenyu.admin.model.dto.ResourceDTO;
import org.apache.shenyu.admin.model.entity.PermissionDO;
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
import org.apache.shenyu.admin.utils.ListUtil;
import org.apache.shenyu.admin.utils.ResourceUtil;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.enums.AdminResourceEnum;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.ResourceService}.
 */
@Service
public class ResourceServiceImpl implements ResourceService {
    
    private final ResourceMapper resourceMapper;
    
    private final PermissionMapper permissionMapper;
    
    public ResourceServiceImpl(final ResourceMapper resourceMapper,
                               final PermissionMapper permissionMapper) {
        this.resourceMapper = resourceMapper;
        this.permissionMapper = permissionMapper;
    }
    
    /**
     * create resource and return data.
     *
     * @param resourceDO {@linkplain ResourceDO}
     */
    @Override
    public void createResource(final ResourceDO resourceDO) {
        insertResource(resourceDO);
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
     * create or update resource.
     *
     * @param resourceDTO {@linkplain ResourceDTO}
     * @return rows int
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public int createOrUpdate(final ResourceDTO resourceDTO) {
        ResourceDO resourceDO = ResourceDO.buildResourceDO(resourceDTO);
        if (StringUtils.isEmpty(resourceDTO.getId())) {
            return insertResource(resourceDO);
        } else {
            return resourceMapper.updateSelective(resourceDO);
        }
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
        int ret = 0;
        List<ResourceVO> resourceVOList = ListUtil.map(resourceMapper.selectAll(), ResourceVO::buildResourceVO);
        List<String> deleteResourceIds = ResourceUtil.getDeleteResourceIds(ids, resourceVOList);
        if (CollectionUtils.isNotEmpty(deleteResourceIds)) {
            permissionMapper.deleteByResourceId(deleteResourceIds);
            ret = resourceMapper.delete(deleteResourceIds);
        }
        
        return ret;
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
        List<ResourceVO> resourceVOList = ListUtil.map(resourceMapper.selectAll(), ResourceVO::buildResourceVO);
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
        return resourceMapper.selectByParentId(id).stream()
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
        ResourceDO resourceDO = ResourceUtil.buildPluginResource(event.getPlugin().getName());
        createResource(resourceDO);
        for (ResourceDO resource : ResourceUtil.buildPluginDataPermissionResource(resourceDO.getId(), event.getPlugin().getName())) {
            createResource(resource);
        }
    }
    
    /**
     * The associated Handle needs to be deleted synchronously.
     *
     * @param event event
     */
    @EventListener(value = BatchPluginDeletedEvent.class)
    public void onPluginDeleted(final BatchPluginDeletedEvent event) {
        // 5. delete resource & permission.
        final List<ResourceVO> resources = listByTitles(ListUtil.map((List<?>) event.getSource(), s -> ((PluginDO) s).getName()));
        if (CollectionUtils.isNotEmpty(resources)) {
            delete(ListUtil.map(resources, ResourceVO::getId));
        }
    }
    
    /**
     * insert Resource.
     *
     * @param resourceDO {@linkplain ResourceDO}
     * @return row int
     */
    private int insertResource(final ResourceDO resourceDO) {
        permissionMapper.insertSelective(PermissionDO.buildPermissionDO(PermissionDTO.builder()
                .objectId(AdminConstants.ROLE_SUPER_ID)
                .resourceId(resourceDO.getId()).build()));
        return resourceMapper.insertSelective(resourceDO);
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
        List<PermissionDO> permissionDOList = resourceDOList.stream().filter(Objects::nonNull).map(resourceDO -> PermissionDO.buildPermissionDO(PermissionDTO.builder()
                .objectId(AdminConstants.ROLE_SUPER_ID)
                .resourceId(resourceDO.getId()).build())).collect(Collectors.toList());
        
        permissionMapper.insertBatch(permissionDOList);
        
        return resourceMapper.insertBatch(resourceDOList);
    }
}
