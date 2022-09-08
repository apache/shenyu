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
import org.apache.shenyu.admin.config.properties.JwtProperties;
import org.apache.shenyu.admin.config.properties.LdapProperties;
import org.apache.shenyu.admin.mapper.DashboardUserMapper;
import org.apache.shenyu.admin.mapper.RoleMapper;
import org.apache.shenyu.admin.mapper.UserRoleMapper;
import org.apache.shenyu.admin.model.dto.DashboardUserDTO;
import org.apache.shenyu.admin.model.dto.DashboardUserModifyPasswordDTO;
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
import org.apache.shenyu.admin.service.DashboardUserService;
import org.apache.shenyu.admin.service.publish.UserEventPublisher;
import org.apache.shenyu.admin.transfer.DashboardUserTransfer;
import org.apache.shenyu.admin.utils.Assert;
import org.apache.shenyu.admin.utils.JwtUtils;
import org.apache.shenyu.admin.utils.ListUtil;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.utils.ShaUtils;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import javax.annotation.Nullable;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ldap.NameNotFoundException;
import org.springframework.ldap.core.LdapTemplate;
import org.springframework.ldap.support.LdapEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.Lists;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.DashboardUserService}.
 */
@Service
public class DashboardUserServiceImpl implements DashboardUserService {
    
    private static final Logger LOG = LoggerFactory.getLogger(DashboardUserServiceImpl.class);
    
    private final DashboardUserMapper dashboardUserMapper;
    
    private final UserRoleMapper userRoleMapper;
    
    private final RoleMapper roleMapper;
    
    @Nullable
    private final LdapProperties ldapProperties;
    
    @Nullable
    private final LdapTemplate ldapTemplate;
    
    private final JwtProperties jwtProperties;
    
    private final UserEventPublisher publisher;
    
    public DashboardUserServiceImpl(final DashboardUserMapper dashboardUserMapper,
                                    final UserRoleMapper userRoleMapper,
                                    final RoleMapper roleMapper,
                                    @Nullable final LdapProperties ldapProperties,
                                    @Nullable final LdapTemplate ldapTemplate,
                                    final JwtProperties jwtProperties,
                                    final UserEventPublisher publisher) {
        this.dashboardUserMapper = dashboardUserMapper;
        this.userRoleMapper = userRoleMapper;
        this.roleMapper = roleMapper;
        this.ldapProperties = ldapProperties;
        this.ldapTemplate = ldapTemplate;
        this.jwtProperties = jwtProperties;
        this.publisher = publisher;
    }
    
    /**
     * create or update dashboard user.
     *
     * @param dashboardUserDTO {@linkplain DashboardUserDTO}
     * @return rows
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public int createOrUpdate(final DashboardUserDTO dashboardUserDTO) {
        return StringUtils.isBlank(dashboardUserDTO.getId()) ? create(dashboardUserDTO) : update(dashboardUserDTO);
    }
    
    @Override
    public int create(final DashboardUserDTO dashboardUserDTO) {
        DashboardUserDO dashboardUserDO = DashboardUserDO.buildDashboardUserDO(dashboardUserDTO);
        // create new user
        final int insertCount = dashboardUserMapper.insertSelective(dashboardUserDO);
        bindUserRole(dashboardUserDO.getId(), dashboardUserDTO.getRoles());
        if (insertCount > 0) {
            publisher.onCreated(dashboardUserDO);
        }
        return insertCount;
    }
    
    @Override
    public int update(final DashboardUserDTO dashboardUserDTO) {
        DashboardUserDO dashboardUserDO = DashboardUserDO.buildDashboardUserDO(dashboardUserDTO);
        // update old user
        if (CollectionUtils.isNotEmpty(dashboardUserDTO.getRoles())) {
            if (!AdminConstants.ADMIN_NAME.equals(dashboardUserDTO.getUserName())) {
                userRoleMapper.deleteByUserId(dashboardUserDTO.getId());
            }
            bindUserRole(dashboardUserDTO.getId(), dashboardUserDTO.getRoles());
        }
        final DashboardUserDO before = dashboardUserMapper.selectById(dashboardUserDO.getId());
        final int updateCount = dashboardUserMapper.updateSelective(dashboardUserDO);
        if (updateCount > 0) {
            publisher.onUpdated(dashboardUserDO, before);
        }
        return updateCount;
    }
    
    /**
     * delete dashboard users.
     *
     * @param ids primary key of dashboard_user.
     * @return the count of deleted dashboard users
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public int delete(final Set<String> ids) {
        final List<DashboardUserDO> deletedUser = dashboardUserMapper.selectByIds(ids)
                .stream()
                // skip default admin user
                .filter(u -> !Objects.equals(u.getUserName(), AdminConstants.ADMIN_NAME))
                .collect(Collectors.toList());
        final List<String> deletedIds = ListUtil.map(deletedUser, DashboardUserDO::getId);
        int deleteCount = dashboardUserMapper.deleteByIdList(deletedIds);
        if (deleteCount > 0) {
            userRoleMapper.deleteByUserIdList(deletedIds);
            publisher.onDeleted(deletedUser);
            
        }
        return deleteCount;
    }
    
    /**
     * find dashboard user by id.
     *
     * @param id primary key..
     * @return {@linkplain DashboardUserVO}
     */
    @Override
    public DashboardUserEditVO findById(final String id) {
        
        DashboardUserVO dashboardUserVO = DashboardUserVO.buildDashboardUserVO(dashboardUserMapper.selectById(id));
        
        Set<String> roleIdSet = userRoleMapper.findByUserId(id)
                .stream()
                .map(UserRoleDO::getRoleId)
                .collect(Collectors.toSet());
        
        List<RoleDO> allRoleDOList = roleMapper.selectAll();
        List<RoleVO> allRoles = ListUtil.map(allRoleDOList, RoleVO::buildRoleVO);
        
        List<RoleDO> roleDOList = allRoleDOList.stream()
                .filter(roleDO -> roleIdSet.contains(roleDO.getId()))
                .collect(Collectors.toList());
        List<RoleVO> roles = ListUtil.map(roleDOList, RoleVO::buildRoleVO);
        
        return DashboardUserEditVO.buildDashboardUserEditVO(dashboardUserVO, roles, allRoles);
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
            () -> ListUtil.map(dashboardUserMapper.selectByQuery(dashboardUserQuery), DashboardUserVO::buildDashboardUserVO));
    }
    
    /**
     * To deal with the admin login.
     *
     * @param userName default username is admin
     * @param password admin password
     * @return {@linkplain LoginDashboardUserVO}
     */
    @Override
    public LoginDashboardUserVO login(final String userName, final String password) {
        DashboardUserVO dashboardUserVO = null;
        if (Objects.nonNull(ldapTemplate)) {
            dashboardUserVO = loginByLdap(userName, password);
        }
        
        if (Objects.isNull(dashboardUserVO)) {
            dashboardUserVO = loginByDatabase(userName, password);
        }
        
        final LoginDashboardUserVO loginDashboardUserVO = LoginDashboardUserVO.buildLoginDashboardUserVO(dashboardUserVO);
        final DashboardUserVO finalDashboardUserVO = dashboardUserVO;
        return Optional.ofNullable(loginDashboardUserVO).map(loginUser -> {
            if (Boolean.FALSE.equals(loginUser.getEnabled())) {
                return loginUser;
            }
            return loginUser.setToken(JwtUtils.generateToken(finalDashboardUserVO.getUserName(), finalDashboardUserVO.getPassword(),
                    jwtProperties.getExpiredSeconds())).setExpiredTime(jwtProperties.getExpiredSeconds());
        }).orElse(null);
    }

    /**
     * modify password.
     *
     * @param dashboardUserModifyPasswordDTO {@linkplain DashboardUserModifyPasswordDTO}
     * @return rows
     */
    @Override
    public int modifyPassword(final DashboardUserModifyPasswordDTO dashboardUserModifyPasswordDTO) {
        DashboardUserDO dashboardUserDO = DashboardUserDO.buildDashboardUserDO(dashboardUserModifyPasswordDTO);
        DashboardUserDO before = dashboardUserMapper.selectById(dashboardUserDO.getId());
        int updateCount = dashboardUserMapper.updateSelective(dashboardUserDO);
        if (updateCount > 0) {
            publisher.onUpdated(dashboardUserDO, before);
        }
        return updateCount;
    }

    private DashboardUserVO loginByLdap(final String userName, final String password) {
        Assert.notNull(ldapProperties, "ldap config is not enable");
        String searchBase = String.format("%s=%s,%s", ldapProperties.getLoginField(), LdapEncoder.nameEncode(userName), ldapProperties.getBaseDn());
        String filter = String.format("(objectClass=%s)", ldapProperties.getObjectClass());
        try {
            DashboardUserVO dashboardUserVO = null;
            if (Objects.nonNull(ldapTemplate) && ldapTemplate.authenticate(searchBase, filter, password)) {
                dashboardUserVO = findByUserName(userName);
                if (Objects.isNull(dashboardUserVO)) {
                    RoleDO role = roleMapper.findByRoleName("default");
                    DashboardUserDTO dashboardUserDTO = DashboardUserDTO.builder()
                            .userName(userName)
                            .password(ShaUtils.shaEncryption(password))
                            .role(1)
                            .roles(Lists.newArrayList(role.getId()))
                            .enabled(true)
                            .build();
                    createOrUpdate(dashboardUserDTO);
                    dashboardUserVO = DashboardUserTransfer.INSTANCE.transferDTO2VO(dashboardUserDTO);
                }
            }
            return dashboardUserVO;
        } catch (NameNotFoundException e) {
            return null;
        } catch (Exception e) {
            LOG.error("ldap verify error.", e);
            return null;
        }
    }
    
    private DashboardUserVO loginByDatabase(final String userName, final String password) {
        return findByQuery(userName, ShaUtils.shaEncryption(password));
    }
    
    /**
     * bind user and role id.
     *
     * @param userId  user id
     * @param roleIds role ids.
     */
    private void bindUserRole(final String userId, final List<String> roleIds) {
        if (CollectionUtils.isEmpty(roleIds) || StringUtils.isBlank(userId)) {
            return;
        }
        userRoleMapper.insertBatch(roleIds.stream()
                .map(roleId -> UserRoleDO.buildUserRoleDO(UserRoleDTO.builder()
                        .userId(userId)
                        .roleId(roleId)
                        .build()))
                .collect(Collectors.toList()));
    }
}
