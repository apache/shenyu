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
import org.apache.shenyu.admin.model.dto.PermissionDTO;
import org.apache.shenyu.admin.model.dto.ResourceDTO;
import org.apache.shenyu.admin.model.dto.RoleDTO;
import org.apache.shenyu.admin.model.entity.PermissionDO;
import org.apache.shenyu.admin.model.entity.RoleDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.PermissionQuery;
import org.apache.shenyu.admin.model.query.RoleQuery;
import org.apache.shenyu.admin.model.vo.ResourceVO;
import org.apache.shenyu.admin.model.vo.RoleEditVO;
import org.apache.shenyu.admin.model.vo.RoleEditVO.PermissionInfo;
import org.apache.shenyu.admin.model.vo.RoleEditVO.ResourceInfo;
import org.apache.shenyu.admin.model.vo.RoleVO;
import org.apache.shenyu.admin.service.RoleService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
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

    public RoleServiceImpl(final RoleMapper roleMapper,
                           final PermissionMapper permissionMapper,
                           final ResourceMapper resourceMapper) {
        this.roleMapper = roleMapper;
        this.permissionMapper = permissionMapper;
        this.resourceMapper = resourceMapper;
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
        RoleDO roleDO = RoleDO.buildRoleDO(roleDTO);
        if (StringUtils.isEmpty(roleDTO.getId())) {
            return roleMapper.insertSelective(roleDO);
        } else {
            manageRolePermission(roleDTO.getId(), roleDTO.getCurrentPermissionIds());
            return roleMapper.updateSelective(roleDO);
        }
    }

    /**
     * delete role info.
     *
     * @param ids primary key
     * @return rows
     */
    @Override
    public int delete(final List<String> ids) {
        permissionMapper.deleteByObjectIds(ids);
        return roleMapper.delete(ids);
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
        return roleMapper.selectAll()
                .stream()
                .map(RoleVO::buildRoleVO)
                .collect(Collectors.toList());
    }

    /**
     * get all permissions.
     *
     * @return {@linkplain PermissionInfo}
     */
    private PermissionInfo getAllPermissions() {
        List<ResourceVO> resourceVOList = resourceMapper.selectAll()
                .stream()
                .map(ResourceVO::buildResourceVO)
                .collect(Collectors.toList());
        List<String> permissionIds = resourceVOList.stream().map(ResourceVO::getId).collect(Collectors.toList());

        List<ResourceInfo> treeList = getTreeModelList(resourceVOList);
        return PermissionInfo.builder().treeList(treeList).permissionIds(permissionIds).build();
    }

    /**
     * get permission ids by role id.
     *
     * @param roleId role id
     * @return {@linkplain List}
     */
    private List<String> getPermissionIdsByRoleId(final String roleId) {
        return permissionMapper.findByObjectId(roleId)
                .stream()
                .map(PermissionDO::getResourceId)
                .collect(Collectors.toList());
    }

    /**
     * get menu list.
     *
     * @param metaList {@linkplain ResourceDTO}
     * @return list of {@linkplain ResourceInfo}
     */
    private List<ResourceInfo> getTreeModelList(final List<ResourceVO> metaList) {

        List<ResourceInfo> retList = new ArrayList<>();
        if (CollectionUtils.isEmpty(metaList)) {
            return retList;
        }
        Map<String, ResourceInfo> resourceInfoMap = metaList.stream().map(ResourceInfo::buildResourceInfo)
                .filter(resourceInfo -> Objects.nonNull(resourceInfo) && StringUtils.isNotEmpty(resourceInfo.getId()))
                .collect(Collectors.toMap(ResourceInfo::getId, Function.identity(), (value1, value2) -> value1));
        Map<String, Set<String>> metaChildrenMap = metaList.stream().filter(meta -> Objects.nonNull(meta) && StringUtils.isNotEmpty(meta.getId()))
                .collect(Collectors.toMap(ResourceVO::getParentId,
                    resourceVO -> {
                        Set<String> idSet = new LinkedHashSet<>();
                        idSet.add(resourceVO.getId());
                        return idSet;
                    }, (set1, set2) -> {
                        set1.addAll(set2);
                        return set1;
                    }, LinkedHashMap::new));
        metaChildrenMap.forEach((parent, children) -> {
            ResourceInfo resourceInfo = resourceInfoMap.get(parent);
            if (CollectionUtils.isNotEmpty(children)) {
                List<ResourceInfo> targetList;
                if (Objects.isNull(resourceInfo)) {
                    targetList = retList;
                } else {
                    targetList = resourceInfo.getChildren();
                }
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

    /**
     * get two list different.
     *
     * @param preList  {@linkplain List}
     * @param lastList {@linkplain List}
     * @return {@linkplain List}
     */
    private List<String> getListDiff(final List<String> preList, final List<String> lastList) {
        if (CollectionUtils.isEmpty(lastList)) {
            return null;
        }

        if (CollectionUtils.isEmpty(preList)) {
            return lastList;
        }

        Map<String, Integer> map = preList.stream().distinct()
                .collect(Collectors.toMap(source -> source, source -> 1));
        return lastList.stream().filter(item -> !map.containsKey(item)).collect(Collectors.toList());
    }

    /**
     * batch save permission.
     *
     * @param permissionDOList {@linkplain List}
     */
    private void batchSavePermission(final List<PermissionDO> permissionDOList) {
        permissionDOList.forEach(permissionMapper::insertSelective);
    }

    /**
     * delete by object and resource id.
     *
     * @param permissionQuery permission query
     */
    private void deleteByObjectIdAndResourceId(final PermissionQuery permissionQuery) {
        permissionMapper.deleteByObjectIdAndResourceId(permissionQuery);
    }

    /**
     * manger role permission.
     *
     * @param roleId                role id.
     * @param currentPermissionList {@linkplain List} current role permission ids
     */
    private void manageRolePermission(final String roleId, final List<String> currentPermissionList) {
        List<String> lastPermissionList = permissionMapper.findByObjectId(roleId).stream().map(PermissionDO::getResourceId).collect(Collectors.toList());
        List<String> addPermission = getListDiff(lastPermissionList, currentPermissionList);
        if (CollectionUtils.isNotEmpty(addPermission)) {
            batchSavePermission(addPermission.stream().map(node -> PermissionDO.buildPermissionDO(PermissionDTO.builder().objectId(roleId).resourceId(node).build())).collect(Collectors.toList()));
        }

        List<String> deletePermission = getListDiff(currentPermissionList, lastPermissionList);
        if (CollectionUtils.isNotEmpty(deletePermission)) {
            deletePermission.forEach(node -> deleteByObjectIdAndResourceId(new PermissionQuery(roleId, node)));
        }
    }
}
