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
import org.apache.shenyu.admin.mapper.RoleMapper;
import org.apache.shenyu.admin.model.dto.ResourceDTO;
import org.apache.shenyu.admin.model.dto.RoleDTO;
import org.apache.shenyu.admin.model.entity.PermissionDO;
import org.apache.shenyu.admin.model.entity.RoleDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.RoleQuery;
import org.apache.shenyu.admin.model.vo.ResourceVO;
import org.apache.shenyu.admin.model.vo.RoleEditVO;
import org.apache.shenyu.admin.model.vo.RoleEditVO.PermissionInfo;
import org.apache.shenyu.admin.model.vo.RoleEditVO.ResourceInfo;
import org.apache.shenyu.admin.model.vo.RoleVO;
import org.apache.shenyu.admin.service.RoleService;
import org.apache.shenyu.admin.service.publish.RoleEventPublisher;
import org.apache.shenyu.admin.utils.ListUtil;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.RoleService}.
 */
@Service
public class RoleServiceImpl implements RoleService {
    
    private final RoleMapper roleMapper;
    
    private final PermissionMapper permissionMapper;
    
    private final ResourceMapper resourceMapper;
    
    private final RoleEventPublisher roleEventPublisher;
    
    public RoleServiceImpl(final RoleMapper roleMapper,
                           final PermissionMapper permissionMapper,
                           final ResourceMapper resourceMapper,
                           final RoleEventPublisher roleEventPublisher) {
        this.roleMapper = roleMapper;
        this.permissionMapper = permissionMapper;
        this.resourceMapper = resourceMapper;
        this.roleEventPublisher = roleEventPublisher;
    }
    
    /**
     * create or update role info.
     *
     * @param roleDTO {@linkplain RoleDTO}
     * @return rows
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public int createOrUpdate(final RoleDTO roleDTO) {
        return RoleService.super.createOrUpdate(roleDTO);
    }
    
    @Override
    public int create(final RoleDTO roleDTO) {
        final RoleDO role = RoleDO.buildRoleDO(roleDTO);
        final int insertCount = roleMapper.insertSelective(role);
        if (insertCount > 0) {
            roleEventPublisher.onCreated(role);
        }
        return insertCount;
    }
    
    @Override
    public int update(final RoleDTO roleDTO) {
        final RoleDO before = roleMapper.selectById(roleDTO.getId());
        final RoleDO role = RoleDO.buildRoleDO(roleDTO);
        final int updateCount = roleMapper.updateSelective(role);
        if (updateCount > 0) {
            // add new permission
            roleEventPublisher.onUpdated(role, before, roleDTO.getCurrentPermissionIds());
        }
        return updateCount;
    }
    
    /**
     * delete role info.
     *
     * @param ids primary key
     * @return rows
     */
    @Override
    public int delete(final List<String> ids) {
        final List<RoleDO> roles = roleMapper.selectByIds(ids);
        final int deleteCount = roleMapper.delete(ids);
        if (deleteCount > 0) {
            roleEventPublisher.onDeleted(roles);
        }
        return deleteCount;
    }
    
    /**
     * find role info by id.
     *
     * @param id primary key
     * @return {@linkplain RoleEditVO}
     */
    @Override
    public RoleEditVO findById(final String id) {
        RoleVO sysRole = RoleVO.buildRoleVO(roleMapper.selectById(id));
        return Optional.ofNullable(sysRole)
                .map(item -> new RoleEditVO(getPermissionIdsByRoleId(item.getId()), item, getAllPermissions()))
                .orElse(null);
    }
    
    /**
     * find role by query.
     *
     * @param roleName role name
     * @return {@linkplain RoleVO}
     */
    @Override
    public RoleVO findByQuery(final String roleName) {
        return RoleVO.buildRoleVO(roleMapper.findByRoleName(roleName));
    }
    
    /**
     * find page of role by query.
     *
     * @param roleQuery {@linkplain RoleQuery}
     * @return {@linkplain CommonPager}
     */
    @Override
    @Pageable
    public CommonPager<RoleVO> listByPage(final RoleQuery roleQuery) {
        return PageResultUtils.result(roleQuery.getPageParameter(), () -> roleMapper.selectByQuery(roleQuery)
                .stream()
                .map(RoleVO::buildRoleVO)
                .collect(Collectors.toList()));
    }
    
    /**
     * select all roles.
     *
     * @return {@linkplain List}
     */
    @Override
    public List<RoleVO> selectAll() {
        return ListUtil.map(roleMapper.selectAll(), RoleVO::buildRoleVO);
    }
    
    /**
     * get all permissions.
     *
     * @return {@linkplain PermissionInfo}
     */
    private PermissionInfo getAllPermissions() {
        final List<ResourceVO> resourceVOList = ListUtil.map(resourceMapper.selectAll(), ResourceVO::buildResourceVO);
        return PermissionInfo.builder()
                .treeList(getTreeModelList(resourceVOList))
                .permissionIds(ListUtil.map(resourceVOList, ResourceVO::getId))
                .build();
    }
    
    /**
     * get permission ids by role id.
     *
     * @param roleId role id
     * @return {@linkplain List}
     */
    private List<String> getPermissionIdsByRoleId(final String roleId) {
        return ListUtil.map(permissionMapper.findByObjectId(roleId), PermissionDO::getResourceId);
    }
    
    /**
     * get menu list.
     *
     * @param metaList {@linkplain ResourceDTO}
     * @return list of {@linkplain ResourceInfo}
     */
    private List<ResourceInfo> getTreeModelList(final List<ResourceVO> metaList) {
        final List<ResourceInfo> retList = new ArrayList<>();
        if (CollectionUtils.isEmpty(metaList)) {
            return retList;
        }
        final Map<String, ResourceInfo> resourceInfoMap = metaList.stream()
                .map(ResourceInfo::buildResourceInfo)
                .filter(resourceInfo -> Objects.nonNull(resourceInfo) && StringUtils.isNotEmpty(resourceInfo.getId()))
                .collect(Collectors.toMap(ResourceInfo::getId, Function.identity(), (value1, value2) -> value1));
        final Map<String, Set<String>> metaChildrenMap = metaList.stream()
                .filter(meta -> Objects.nonNull(meta) && StringUtils.isNotEmpty(meta.getId()))
                .collect(Collectors.toMap(ResourceVO::getParentId, resourceVO -> new LinkedHashSet<>(Collections.singletonList(resourceVO.getId())), ListUtil::mergeSet, LinkedHashMap::new));
        metaChildrenMap.forEach((parent, children) -> {
            if (CollectionUtils.isNotEmpty(children)) {
                ResourceInfo resourceInfo = resourceInfoMap.get(parent);
                List<ResourceInfo> targetList = Objects.isNull(resourceInfo) ? retList : resourceInfo.getChildren();
                children.forEach(child -> {
                    ResourceInfo data = resourceInfoMap.get(child);
                    if (Objects.nonNull(data)) {
                        targetList.add(data);
                    }
                });
            }
        });
        return retList;
    }
    
}
