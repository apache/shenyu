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
import org.apache.shenyu.admin.mapper.DashboardUserMapper;
import org.apache.shenyu.admin.mapper.PermissionMapper;
import org.apache.shenyu.admin.mapper.ResourceMapper;
import org.apache.shenyu.admin.mapper.UserRoleMapper;
import org.apache.shenyu.admin.model.custom.UserInfo;
import org.apache.shenyu.admin.model.entity.PermissionDO;
import org.apache.shenyu.admin.model.entity.UserRoleDO;
import org.apache.shenyu.admin.model.vo.PermissionMenuVO;
import org.apache.shenyu.admin.model.vo.PermissionMenuVO.AuthPerm;
import org.apache.shenyu.admin.model.vo.PermissionMenuVO.MenuInfo;
import org.apache.shenyu.admin.model.vo.ResourceVO;
import org.apache.shenyu.admin.service.PermissionService;
import org.apache.shenyu.admin.service.ResourceService;
import org.apache.shenyu.admin.utils.JwtUtils;
import org.apache.shenyu.common.constant.ResourceTypeConstants;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.PermissionService}.
 */
@Service
public class PermissionServiceImpl implements PermissionService {

    private final DashboardUserMapper dashboardUserMapper;

    private final UserRoleMapper userRoleMapper;

    private final PermissionMapper permissionMapper;

    private final ResourceMapper resourceMapper;

    private final ResourceService resourceService;

    public PermissionServiceImpl(final DashboardUserMapper dashboardUserMapper,
                                 final UserRoleMapper userRoleMapper,
                                 final PermissionMapper permissionMapper,
                                 final ResourceMapper resourceMapper,
                                 final ResourceService resourceService) {
        this.dashboardUserMapper = dashboardUserMapper;
        this.userRoleMapper = userRoleMapper;
        this.permissionMapper = permissionMapper;
        this.resourceMapper = resourceMapper;
        this.resourceService = resourceService;
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

        List<MenuInfo> menuInfoList = resourceService.getMenuInfo(resourceVOList);
        return new PermissionMenuVO(menuInfoList, getAuthPerm(resourceVOList), getAllAuthPerms());
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
            return getAuthPerm(resourceVOList).stream().map(AuthPerm::getPerms).collect(Collectors.toSet());
        }
        return Collections.emptySet();
    }

    /**
     * get resource by username.
     *
     * @param userName username
     * @return {@linkplain List}
     */
    private List<ResourceVO> getResourceListByUserName(final String userName) {
        List<UserRoleDO> userRoleDOList = userRoleMapper.findByUserId(dashboardUserMapper.selectByUserName(userName).getId());
        List<String> roleIds = userRoleDOList.stream().filter(Objects::nonNull)
                .map(UserRoleDO::getRoleId)
                .collect(Collectors.toList());

        Set<String> resourceIds = permissionMapper.findByObjectIds(roleIds).stream()
                .map(PermissionDO::getResourceId)
                .filter(StringUtils::isNoneBlank)
                .collect(Collectors.toSet());

        if (CollectionUtils.isEmpty(resourceIds)) {
            return Collections.emptyList();
        }

        return Optional.ofNullable(resourceMapper.selectByIdsBatch(resourceIds)).orElseGet(ArrayList::new)
                .stream().map(ResourceVO::buildResourceVO).collect(Collectors.toList());
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

        return resourceMapper.selectByResourceType(ResourceTypeConstants.MENU_TYPE_2).stream()
                .map(item -> AuthPerm.buildAuthPerm(ResourceVO.buildResourceVO(item)))
                .collect(Collectors.toList());
    }
}
