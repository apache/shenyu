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

package org.dromara.soul.admin.service.impl;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.dromara.soul.admin.entity.PermissionDO;
import org.dromara.soul.admin.entity.UserRoleDO;
import org.dromara.soul.admin.mapper.DashboardUserMapper;
import org.dromara.soul.admin.mapper.PermissionMapper;
import org.dromara.soul.admin.mapper.ResourceMapper;
import org.dromara.soul.admin.mapper.UserRoleMapper;
import org.dromara.soul.admin.service.PermissionService;
import org.dromara.soul.admin.utils.JwtUtils;
import org.dromara.soul.admin.vo.PermissionMenuVO;
import org.dromara.soul.admin.vo.PermissionMenuVO.AuthPerm;
import org.dromara.soul.admin.vo.PermissionMenuVO.MenuInfo;
import org.dromara.soul.admin.vo.ResourceVO;
import org.dromara.soul.common.constant.ResourceTypeConstants;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;
import reactor.util.StringUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * this is permission service impl.
 *
 * @author nuo-promise
 */
@Service("permissionService")
public class PermissionServiceImpl implements PermissionService {

    private final UserRoleMapper userRoleMapper;

    private final DashboardUserMapper dashboardUserMapper;

    private final PermissionMapper permissionMapper;

    private final ResourceMapper resourceMapper;

    public PermissionServiceImpl(final DashboardUserMapper dashboardUserMapper, final UserRoleMapper userRoleMapper,
                                 final PermissionMapper permissionMapper, final ResourceMapper resourceMapper) {
        this.dashboardUserMapper = dashboardUserMapper;
        this.userRoleMapper = userRoleMapper;
        this.permissionMapper = permissionMapper;
        this.resourceMapper = resourceMapper;
    }

    /**
     * get user permission menu by token.
     *
     * @param token logon ack token.
     * @return {@linkplain PermissionMenuVO}
     */
    @Override
    public PermissionMenuVO getPermissionMenu(final String token) {
        String userName = JwtUtils.getIssuer(token);
        List<ResourceVO> resourceVOList = getResourceListByUserName(userName);
        if (CollectionUtils.isNotEmpty(resourceVOList)) {
            List<MenuInfo> menuInfoList = new ArrayList<>();
            getMenuInfo(menuInfoList, resourceVOList, null);
            return new PermissionMenuVO(menuInfoList, getAuthPerm(resourceVOList), getAllAuthPerms());
        }
        return null;
    }

    /**
     * get Auth perm by user name for shiro.
     *
     * @param userName user name.
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
     * @param userName user name
     * @return {@linkplain List}
     */
    private List<ResourceVO> getResourceListByUserName(final String userName) {
        Map<String, Integer> resourceMap = new HashMap<>();
        List<UserRoleDO> userRoleDOList = userRoleMapper.findByUserId(dashboardUserMapper.selectByUserName(userName).getId());
        for (UserRoleDO userRoleDO : userRoleDOList) {
            permissionMapper.findByObjectId(userRoleDO.getRoleId()).stream().map(PermissionDO::getResourceId).collect(Collectors.toList()).forEach(resource -> {
                resourceMap.put(resource, 1);
            });
        }
        if (MapUtils.isNotEmpty(resourceMap)) {
            return new ArrayList<>(resourceMap.keySet()).stream()
                    .map(resource -> ResourceVO.buildResourceVO(resourceMapper.selectById(resource)))
                    .collect(Collectors.toList());
        }
        return Collections.emptyList();
    }

    /**
     * get Menu Info.
     *
     * @param menuInfoList {@linkplain List} menu info.
     * @param metaList {@linkplain List} resource list
     * @param menuInfo {@linkplain MenuInfo}
     */
    private void getMenuInfo(final List<MenuInfo> menuInfoList, final List<ResourceVO> metaList, final MenuInfo menuInfo) {
        for (ResourceVO resourceVO : metaList) {
            String parentId = resourceVO.getParentId();
            MenuInfo tempMenuInfo = MenuInfo.buildMenuInfo(resourceVO);
            if (ObjectUtils.isEmpty(tempMenuInfo)) {
                continue;
            }
            if (ObjectUtils.isEmpty(menuInfo) && StringUtils.isEmpty(parentId)) {
                menuInfoList.add(tempMenuInfo);
                if (resourceVO.getIsLeaf().equals(Boolean.FALSE)) {
                    getMenuInfo(menuInfoList, metaList, tempMenuInfo);
                }
            } else if (!ObjectUtils.isEmpty(menuInfo) && !StringUtils.isEmpty(parentId) && parentId.equals(menuInfo.getId())) {
                menuInfo.getChildren().add(tempMenuInfo);
                if (resourceVO.getIsLeaf().equals(Boolean.FALSE)) {
                    getMenuInfo(menuInfoList, metaList, tempMenuInfo);
                }
            }
        }
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
        return resourceMapper.selectAll().stream()
               .filter(item -> item.getResourceType().equals(ResourceTypeConstants.MENU_TYPE_2))
               .map(item -> AuthPerm.buildAuthPerm(ResourceVO.buildResourceVO(item))).collect(Collectors.toList());
    }
}
