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
import org.apache.shenyu.admin.model.entity.ResourceDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.ResourceQuery;
import org.apache.shenyu.admin.model.vo.PermissionMenuVO.MenuInfo;
import org.apache.shenyu.admin.model.vo.ResourceVO;
import org.apache.shenyu.admin.service.ResourceService;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.enums.AdminResourceEnum;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Deque;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
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
        List<ResourceVO> resourceVOList = resourceMapper.selectAll().stream().map(ResourceVO::buildResourceVO).collect(Collectors.toList());
        List<String> deleteResourceIds = getDeleteResourceIds(ids, resourceVOList);
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
        final List<ResourceDO> resources = this.resourceMapper.selectByTitles(titles);
        if (CollectionUtils.isEmpty(resources)) {
            return Collections.emptyList();
        }
        return resources.stream().map(ResourceVO::buildResourceVO)
                .filter(Objects::nonNull).collect(Collectors.toList());
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
        List<ResourceVO> resourceVOList = resourceMapper.selectAll().stream().map(ResourceVO::buildResourceVO).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(resourceVOList)) {
            return null;
        }

        return this.getMenuInfo(resourceVOList);
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
                .map(ResourceVO::buildResourceVO).collect(Collectors.toList());
    }

    /**
     * get Menu Info.
     *
     * @param metaList {@linkplain List} resource list
     * @return {@linkplain List} menu infos.
     */
    @Override
    public List<MenuInfo> getMenuInfo(final List<ResourceVO> metaList) {

        List<MenuInfo> retList = new ArrayList<>();
        if (CollectionUtils.isEmpty(metaList)) {
            return retList;
        }
        Map<String, MenuInfo> menuInfoMap = metaList.stream().map(MenuInfo::buildMenuInfo).filter(menuInfo -> Objects.nonNull(menuInfo) && StringUtils.isNotEmpty(menuInfo.getId()))
                .collect(Collectors.toMap(MenuInfo::getId, Function.identity(), (value1, value2) -> value1));
        Map<String, Set<String>> metaChildrenMap = this.dealChildrenMap(metaList);

        metaChildrenMap.forEach((parent, children) -> {
            MenuInfo menuInfo = menuInfoMap.get(parent);
            if (CollectionUtils.isNotEmpty(children)) {
                List<MenuInfo> targetList;
                if (Objects.isNull(menuInfo)) {
                    targetList = retList;
                } else {
                    targetList = menuInfo.getChildren();
                }
                children.forEach(child -> {
                    MenuInfo data = menuInfoMap.get(child);
                    if (Objects.nonNull(data)) {
                        targetList.add(data);
                    }
                });
            }
        });

        return retList;
    }

    /**
     * convert the list to a map, the key is the parent id, and the value is the set of child ids.
     *
     * @param metaList the list to be converted
     * @return the map
     */
    private Map<String, Set<String>> dealChildrenMap(final List<ResourceVO> metaList) {
        return metaList.stream().filter(meta -> Objects.nonNull(meta) && StringUtils.isNotEmpty(meta.getId()))
                .collect(Collectors.toMap(ResourceVO::getParentId,
                    resourceVO -> {
                        Set<String> idSet = new HashSet<>();
                        idSet.add(resourceVO.getId());
                        return idSet;
                    }, (set1, set2) -> {
                        set1.addAll(set2);
                        return set1;
                    }));
    }


    /**
     * get delete resource ids.
     *
     * @param resourceIds resource ids
     * @param metaList    all resource object
     * @return the list of ids to delete
     */
    private List<String> getDeleteResourceIds(final List<String> resourceIds, final List<ResourceVO> metaList) {

        List<String> deleteResourceIds = null;
        if (CollectionUtils.isEmpty(metaList) || CollectionUtils.isEmpty(resourceIds)) {
            return deleteResourceIds;
        }
        deleteResourceIds = new ArrayList<>();
        Map<String, ResourceVO> metaMap = metaList.stream().filter(Objects::nonNull)
                .collect(Collectors.toMap(ResourceVO::getId, Function.identity(), (value1, value2) -> value1));

        Map<String, Set<String>> metaChildrenMap = this.dealChildrenMap(metaList);

        Deque<String> cacheDatas = new ArrayDeque<>(resourceIds);
        while (!cacheDatas.isEmpty()) {
            String resourceId = cacheDatas.pollFirst();
            ResourceVO resourceVO = metaMap.get(resourceId);
            Set<String> children = metaChildrenMap.get(resourceId);
            if (Objects.nonNull(resourceVO)) {
                deleteResourceIds.add(resourceVO.getId());
                metaMap.remove(resourceId);
            }
            if (CollectionUtils.isNotEmpty(children)) {
                children.forEach(cacheDatas::addFirst);
                metaChildrenMap.remove(resourceId);
            }
        }
        return deleteResourceIds;
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
