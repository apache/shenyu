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

import com.google.common.collect.Lists;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.config.properties.LdapProperties;
import org.apache.shenyu.admin.config.properties.SecretProperties;
import org.apache.shenyu.admin.mapper.DashboardUserMapper;
import org.apache.shenyu.admin.mapper.DataPermissionMapper;
import org.apache.shenyu.admin.mapper.RoleMapper;
import org.apache.shenyu.admin.mapper.UserRoleMapper;
import org.apache.shenyu.admin.service.DashboardUserService;
import org.apache.shenyu.admin.utils.AesUtils;
import org.apache.shenyu.admin.model.dto.DashboardUserDTO;
import org.apache.shenyu.admin.model.dto.UserRoleDTO;
import org.apache.shenyu.admin.model.entity.DashboardUserDO;
import org.apache.shenyu.admin.model.entity.RoleDO;
import org.apache.shenyu.admin.model.entity.UserRoleDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.DashboardUserQuery;
import org.apache.shenyu.admin.model.vo.DashboardUserEditVO;
import org.apache.shenyu.admin.model.vo.DashboardUserVO;
import org.apache.shenyu.admin.model.vo.LoginDashboardUserVO;
import org.apache.shenyu.admin.model.vo.RoleVO;
import org.apache.shenyu.common.constant.AdminConstants;
import org.springframework.beans.BeanUtils;
import org.springframework.ldap.NameNotFoundException;
import org.springframework.ldap.core.LdapTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import javax.annotation.Nullable;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.DashboardUserService}.
 */
@Slf4j
@RequiredArgsConstructor
@Service
public class DashboardUserServiceImpl implements DashboardUserService {

    private final SecretProperties secretProperties;

    private final DashboardUserMapper dashboardUserMapper;

    private final UserRoleMapper userRoleMapper;

    private final RoleMapper roleMapper;

    private final DataPermissionMapper dataPermissionMapper;

    @Nullable
    private final LdapProperties ldapProperties;

    @Nullable
    private final LdapTemplate ldapTemplate;

    /**
     * create or update dashboard user.
     *
     * @param dashboardUserDTO {@linkplain DashboardUserDTO}
     * @return rows
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public int createOrUpdate(final DashboardUserDTO dashboardUserDTO) {
        DashboardUserDO dashboardUserDO = DashboardUserDO.buildDashboardUserDO(dashboardUserDTO);
        if (StringUtils.isEmpty(dashboardUserDTO.getId())) {
            bindUserRole(dashboardUserDO.getId(), dashboardUserDTO.getRoles());
            return dashboardUserMapper.insertSelective(dashboardUserDO);
        }
        if (!AdminConstants.ADMIN_NAME.equals(dashboardUserDTO.getUserName())) {
            userRoleMapper.deleteByUserId(dashboardUserDTO.getId());
        }
        if (CollectionUtils.isNotEmpty(dashboardUserDTO.getRoles())) {
            bindUserRole(dashboardUserDTO.getId(), dashboardUserDTO.getRoles());
        }
        return dashboardUserMapper.updateSelective(dashboardUserDO);
    }

    /**
     * delete dashboard users.
     *
     * @param ids primary key.
     * @return rows
     */
    @Override
    public int delete(final List<String> ids) {
        int dashboardUserCount = 0;
        for (String id : ids) {
            DashboardUserDO dashboardUserDO = dashboardUserMapper.selectById(id);
            if (!ObjectUtils.isEmpty(dashboardUserDO) && AdminConstants.ADMIN_NAME.equals(dashboardUserDO.getUserName())) {
                continue;
            }
            dashboardUserCount += dashboardUserMapper.delete(id);
            userRoleMapper.deleteByUserId(id);
            dataPermissionMapper.deleteByUserId(id);
        }
        return dashboardUserCount;
    }

    /**
     * find dashboard user by id.
     *
     * @param id primary key..
     * @return {@linkplain DashboardUserVO}
     */
    @Override
    public DashboardUserEditVO findById(final String id) {
        return DashboardUserEditVO.buildDashboardUserEditVO(DashboardUserVO.buildDashboardUserVO(dashboardUserMapper.selectById(id)),
                userRoleMapper.findByUserId(id).stream()
                        .map(item -> RoleVO.buildRoleVO(roleMapper.selectById(item.getRoleId()))).filter(Objects::nonNull).collect(Collectors.toList()),
                roleMapper.selectAll().stream().map(RoleVO::buildRoleVO).collect(Collectors.toList()));
    }

    /**
     * find dashboard user by query.
     *
     * @param userName user name
     * @param password user password
     * @return {@linkplain DashboardUserVO}
     */
    @Override
    public DashboardUserVO findByQuery(final String userName, final String password) {
        return DashboardUserVO.buildDashboardUserVO(dashboardUserMapper.findByQuery(userName, password));
    }

    /**
     * find dashboard user by username.
     *
     * @param userName user name
     * @return {@linkplain DashboardUserVO}
     */
    @Override
    public DashboardUserVO findByUserName(final String userName) {
        return DashboardUserVO.buildDashboardUserVO(dashboardUserMapper.selectByUserName(userName));
    }

    /**
     * find page of dashboard user by query.
     *
     * @param dashboardUserQuery {@linkplain DashboardUserQuery}
     * @return {@linkplain CommonPager}
     */
    @Override
    public CommonPager<DashboardUserVO> listByPage(final DashboardUserQuery dashboardUserQuery) {
        return PageResultUtils.result(dashboardUserQuery.getPageParameter(),
            () -> dashboardUserMapper.countByQuery(dashboardUserQuery),
            () -> dashboardUserMapper.selectByQuery(dashboardUserQuery)
                        .stream()
                        .map(DashboardUserVO::buildDashboardUserVO)
                        .collect(Collectors.toList()));
    }

    /**
     * To deal with the admin login.
     *
     * @param userName default username is admin
     * @param password admin password
     * @return {@linkplain LoginDashboardUserVO}
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public LoginDashboardUserVO login(final String userName, final String password) {
        DashboardUserVO dashboardUserVO = null;
        if (Objects.nonNull(ldapTemplate)) {
            dashboardUserVO = loginByLdap(userName, password);
        }
        if (Objects.isNull(dashboardUserVO)) {
            dashboardUserVO = loginByDatabase(userName, password);
        }
        return LoginDashboardUserVO.buildLoginDashboardUserVO(dashboardUserVO);
    }

    private DashboardUserVO loginByLdap(final String userName, final String password) {
        String key = secretProperties.getKey();
        String searchBase = String.format("%s=%s,%s", ldapProperties.getLoginField(), userName, ldapProperties.getBaseDn());
        String filter = String.format("(objectClass=%s)", ldapProperties.getObjectClass());
        try {
            DashboardUserVO dashboardUserVO = null;
            if (ldapTemplate.authenticate(searchBase, filter, password)) {
                dashboardUserVO = findByUserName(userName);
                if (Objects.isNull(dashboardUserVO)) {
                    RoleDO role = roleMapper.findByRoleName("default");
                    DashboardUserDTO dashboardUserDTO = DashboardUserDTO.builder()
                            .userName(userName)
                            .password(AesUtils.aesEncryption(password, key))
                            .role(1)
                            .roles(Lists.newArrayList(role.getId()))
                            .enabled(true)
                            .build();
                    createOrUpdate(dashboardUserDTO);
                    dashboardUserVO = new DashboardUserVO();
                    BeanUtils.copyProperties(dashboardUserDTO, dashboardUserVO);
                }
            }
            return dashboardUserVO;
        } catch (NameNotFoundException e) {
            return null;
        } catch (Exception e) {
            log.error("ldap verify error.", e);
            return null;
        }
    }

    private DashboardUserVO loginByDatabase(final String userName, final String password) {
        String key = secretProperties.getKey();
        DashboardUserVO dashboardUserVO = findByQuery(userName, password);
        if (!ObjectUtils.isEmpty(dashboardUserVO)) {
            DashboardUserDTO dashboardUserDTO = DashboardUserDTO.builder()
                    .id(dashboardUserVO.getId())
                    .userName(dashboardUserVO.getUserName())
                    .password(AesUtils.aesEncryption(dashboardUserVO.getPassword(), key))
                    .role(dashboardUserVO.getRole())
                    .enabled(dashboardUserVO.getEnabled()).build();
            createOrUpdate(dashboardUserDTO);
        } else {
            dashboardUserVO = findByQuery(userName, AesUtils.aesEncryption(password, key));
        }
        return dashboardUserVO;
    }

    /**
     * bind user and role id.
     *
     * @param userId user id
     * @param roleIds role ids.
     */
    private void bindUserRole(final String userId, final List<String> roleIds) {
        roleIds.forEach(item -> userRoleMapper.insertSelective(UserRoleDO.buildUserRoleDO(UserRoleDTO.builder().userId(userId).roleId(item).build())));
    }
}
