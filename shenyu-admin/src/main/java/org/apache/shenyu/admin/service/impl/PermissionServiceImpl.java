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
import org.apache.shenyu.admin.config.properties.DashboardProperties;
import org.apache.shenyu.admin.mapper.PermissionMapper;
import org.apache.shenyu.admin.mapper.ResourceMapper;
import org.apache.shenyu.admin.model.custom.UserInfo;
import org.apache.shenyu.admin.model.dto.PermissionDTO;
import org.apache.shenyu.admin.model.entity.PermissionDO;
import org.apache.shenyu.admin.model.event.resource.BatchResourceCreatedEvent;
import org.apache.shenyu.admin.model.event.resource.BatchResourceDeletedEvent;
import org.apache.shenyu.admin.model.event.resource.ResourceCreatedEvent;
import org.apache.shenyu.admin.model.event.role.BatchRoleDeletedEvent;
import org.apache.shenyu.admin.model.event.role.RoleUpdatedEvent;
import org.apache.shenyu.admin.model.query.PermissionQuery;
import org.apache.shenyu.admin.model.vo.PermissionMenuVO;
import org.apache.shenyu.admin.model.vo.PermissionMenuVO.AuthPerm;
import org.apache.shenyu.admin.model.vo.ResourceVO;
import org.apache.shenyu.admin.service.PermissionService;
import org.apache.shenyu.admin.utils.JwtUtils;
import org.apache.shenyu.admin.utils.ListUtil;
import org.apache.shenyu.admin.utils.ResourceUtil;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.constant.ResourceTypeConstants;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.PermissionService}.
 */
@Service
public class PermissionServiceImpl implements PermissionService {
    
    private final PermissionMapper permissionMapper;
    
    private final ResourceMapper resourceMapper;
    
    private final DashboardProperties dashboardProperties;
    
    public PermissionServiceImpl(final PermissionMapper permissionMapper,
                                 final ResourceMapper resourceMapper,
                                 final DashboardProperties dashboardProperties) {
        this.permissionMapper = permissionMapper;
        this.resourceMapper = resourceMapper;
        this.dashboardProperties = dashboardProperties;
    }
    
    /**
     * get user permission menu by token.
     *
     * @param token logon ack token.
     * @return {@linkplain PermissionMenuVO}
     */
    @Override
    public PermissionMenuVO getPermissionMenu(final String token) {
        UserInfo userInfo = JwtUtils.getUserInfo();
        if (Objects.isNull(userInfo)) {
            return null;
        }
        
        List<ResourceVO> resourceVOList = getResourceListByUserName(userInfo.getUserName());
        if (CollectionUtils.isEmpty(resourceVOList)) {
            return null;
        }
        return new PermissionMenuVO(ResourceUtil.buildMenu(resourceVOList), getAuthPerm(resourceVOList), getAllAuthPerms());
    }
    
    /**
     * get Auth perm by username for shiro.
     *
     * @param userName username.
     * @return {@linkplain Set}
     */
    @Override
    public Set<String> getAuthPermByUserName(final String userName) {
        List<ResourceVO> resourceVOList = getResourceListByUserName(userName);
        if (CollectionUtils.isNotEmpty(resourceVOList)) {
            return getAuthPerm(resourceVOList).stream()
                    .map(AuthPerm::getPerms)
                    .collect(Collectors.toSet());
        }
        return Collections.emptySet();
    }
    
    
    /**
     * listen {@link ResourceCreatedEvent} add  permission.
     *
     * @param event event
     */
    @EventListener(ResourceCreatedEvent.class)
    public void onResourcesCreated(final ResourceCreatedEvent event) {
        permissionMapper.insertSelective(buildPermissionFromResourceId(event.getResource().getId()));
    }
    
    /**
     * listen {@link BatchResourceCreatedEvent} add  permission.
     *
     * @param event event
     */
    @EventListener(BatchResourceCreatedEvent.class)
    public void onResourcesCreated(final BatchResourceCreatedEvent event) {
        permissionMapper.insertBatch(ListUtil.map(event.getDeletedIds(), this::buildPermissionFromResourceId));
    }
    
    /**
     * listen {@link BatchResourceDeletedEvent} delete  permission.
     *
     * @param event event
     */
    @EventListener(BatchResourceDeletedEvent.class)
    public void onResourcesCreated(final BatchResourceDeletedEvent event) {
        permissionMapper.deleteByResourceId(event.getDeletedIds());
    }
    
    /**
     * listen {@link BatchRoleDeletedEvent} delete  permission.
     *
     * @param event event
     */
    @EventListener(BatchRoleDeletedEvent.class)
    public void onRoleDeleted(final BatchRoleDeletedEvent event) {
        permissionMapper.deleteByObjectIds(event.getDeletedIds());
    }
    
    /**
     * listen {@link RoleUpdatedEvent} delete  permission.
     *
     * @param event event
     */
    @EventListener(RoleUpdatedEvent.class)
    public void onRoleUpdated(final RoleUpdatedEvent event) {
        manageRolePermission(event.getRole().getId(), event.getNewPermission());
    }
    
    /**
     * get resource by username.
     *
     * @param userName username
     * @return {@linkplain List}
     */
    private List<ResourceVO> getResourceListByUserName(final String userName) {
        if (SessionUtil.isAdmin()) {
            return ListUtil.map(resourceMapper.selectByUserName(userName), ResourceVO::buildResourceVO);
        }
        // filter [Only the super administrator root user has the privileges]
        return resourceMapper.selectByUserName(userName)
                .stream()
                .filter(r -> !dashboardProperties.getOnlySuperAdminPermission().contains(r.getPerms()))
                .map(ResourceVO::buildResourceVO)
                .collect(Collectors.toList());
    }
    
    private PermissionDO buildPermissionFromResourceId(final String resourceId) {
        return PermissionDO.buildPermissionDO(PermissionDTO.builder()
                .objectId(AdminConstants.ROLE_SUPER_ID)
                .resourceId(resourceId)
                .build());
    }
    
    /**
     * get AuthPerm by username.
     *
     * @param resourceVOList {@linkplain ResourceVO} resource list by username
     * @return {@linkplain List} Auth perms
     */
    private List<AuthPerm> getAuthPerm(final List<ResourceVO> resourceVOList) {
        return resourceVOList.stream()
                .filter(item -> item.getResourceType().equals(ResourceTypeConstants.MENU_TYPE_2))
                .map(AuthPerm::buildAuthPerm)
                .collect(Collectors.toList());
    }
    
    /**
     * get All AuthPerm.
     *
     * @return {@linkplain List}
     */
    private List<AuthPerm> getAllAuthPerms() {
        return resourceMapper.selectByResourceType(ResourceTypeConstants.MENU_TYPE_2)
                .stream()
                .map(item -> AuthPerm.buildAuthPerm(ResourceVO.buildResourceVO(item)))
                .collect(Collectors.toList());
    }
    
    
    /**
     * manger role permission.
     *
     * @param roleId                role id.
     * @param currentPermissionList {@linkplain List} current role permission ids
     */
    private void manageRolePermission(final String roleId, final List<String> currentPermissionList) {
        List<String> lastPermissionList = permissionMapper.findByObjectId(roleId)
                .stream()
                .map(PermissionDO::getResourceId)
                .collect(Collectors.toList());
        List<String> addPermission = getListDiff(lastPermissionList, currentPermissionList);
        if (CollectionUtils.isNotEmpty(addPermission)) {
            batchSavePermission(addPermission.stream()
                    .map(node -> PermissionDO.buildPermissionDO(PermissionDTO
                            .builder()
                            .objectId(roleId)
                            .resourceId(node)
                            .build()))
                    .collect(Collectors.toList()));
        }
        
        List<String> deletePermission = getListDiff(currentPermissionList, lastPermissionList);
        if (CollectionUtils.isNotEmpty(deletePermission)) {
            deletePermission.forEach(node -> deleteByObjectIdAndResourceId(new PermissionQuery(roleId, node)));
        }
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
            return Collections.emptyList();
        }
        
        if (CollectionUtils.isEmpty(preList)) {
            return lastList;
        }
        
        Map<String, Integer> map = preList.stream()
                .distinct()
                .collect(Collectors.toMap(Function.identity(), source -> 1));
        return lastList.stream()
                .filter(item -> !map.containsKey(item))
                .collect(Collectors.toList());
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
}
