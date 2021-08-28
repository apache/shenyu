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

package org.apache.shenyu.admin.service;

import org.apache.shenyu.admin.config.properties.JwtProperties;
import org.apache.shenyu.admin.config.properties.LdapProperties;
import org.apache.shenyu.admin.config.properties.SecretProperties;
import org.apache.shenyu.admin.mapper.DashboardUserMapper;
import org.apache.shenyu.admin.mapper.DataPermissionMapper;
import org.apache.shenyu.admin.mapper.RoleMapper;
import org.apache.shenyu.admin.mapper.UserRoleMapper;
import org.apache.shenyu.admin.model.dto.DashboardUserDTO;
import org.apache.shenyu.admin.model.dto.RoleDTO;
import org.apache.shenyu.admin.model.entity.DashboardUserDO;
import org.apache.shenyu.admin.model.entity.RoleDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.DashboardUserQuery;
import org.apache.shenyu.admin.model.vo.DashboardUserVO;
import org.apache.shenyu.admin.model.vo.LoginDashboardUserVO;
import org.apache.shenyu.admin.service.impl.DashboardUserServiceImpl;
import org.apache.shenyu.admin.utils.AesUtils;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.ldap.core.LdapTemplate;
import org.springframework.test.util.ReflectionTestUtils;

import java.sql.Timestamp;
import java.util.Arrays;
import java.util.Collections;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * test cases for DashboardUserService.
 */
@RunWith(MockitoJUnitRunner.class)
public final class DashboardUserServiceTest {

    public static final String TEST_ID = "testId";

    public static final String TEST_USER_NAME = "userName";

    public static final String TEST_PASSWORD = "password";

    @InjectMocks
    private DashboardUserServiceImpl dashboardUserService;

    @Mock
    private DashboardUserMapper dashboardUserMapper;

    @Mock
    private UserRoleMapper userRoleMapper;

    @Mock
    private RoleMapper roleMapper;

    @Mock
    private DataPermissionMapper dataPermissionMapper;

    @Mock
    private SecretProperties secretProperties;

    @Mock
    private JwtProperties jwtProperties;

    @Mock
    private LdapTemplate ldapTemplate;

    @Test
    public void testCreateOrUpdate() {
        DashboardUserDTO dashboardUserDTO = DashboardUserDTO.builder()
                .userName(TEST_USER_NAME).password(TEST_PASSWORD).roles(Collections.singletonList("1"))
                .build();
        given(dashboardUserMapper.insertSelective(any(DashboardUserDO.class))).willReturn(1);
        assertEquals(1, dashboardUserService.createOrUpdate(dashboardUserDTO));
        verify(dashboardUserMapper).insertSelective(any(DashboardUserDO.class));

        dashboardUserDTO.setId(TEST_ID);
        given(dashboardUserMapper.updateSelective(any(DashboardUserDO.class))).willReturn(2);
        assertEquals(2, dashboardUserService.createOrUpdate(dashboardUserDTO));
        verify(dashboardUserMapper).updateSelective(any(DashboardUserDO.class));
    }

    @Test
    public void testDelete() {
        given(dashboardUserMapper.delete(eq("1"))).willReturn(1);
        given(dashboardUserMapper.delete(eq("2"))).willReturn(1);
        assertEquals(2, dashboardUserService.delete(Arrays.asList("1", "2")));
    }

    @Test
    public void testFindById() {
        DashboardUserDO dashboardUserDO = createDashboardUserDO();
        given(dashboardUserMapper.selectById(eq(TEST_ID))).willReturn(dashboardUserDO);

        DashboardUserVO dashboardUserVO = dashboardUserService.findById(TEST_ID);
        assertEquals(TEST_ID, dashboardUserVO.getId());
        verify(dashboardUserMapper).selectById(eq(TEST_ID));
    }

    @Test
    public void testFindByQuery() {
        DashboardUserDO dashboardUserDO = createDashboardUserDO();
        given(dashboardUserMapper.findByQuery(eq(TEST_USER_NAME), eq(TEST_PASSWORD))).willReturn(dashboardUserDO);

        DashboardUserVO dashboardUserVO = dashboardUserService.findByQuery(TEST_USER_NAME, TEST_PASSWORD);
        assertEquals(TEST_ID, dashboardUserVO.getId());
        assertEquals(TEST_USER_NAME, dashboardUserVO.getUserName());
        assertEquals(TEST_PASSWORD, dashboardUserVO.getPassword());
        verify(dashboardUserMapper).findByQuery(eq(TEST_USER_NAME), eq(TEST_PASSWORD));
    }

    @Test
    public void testFindByUsername() {
        DashboardUserDO dashboardUserDO = createDashboardUserDO();
        given(dashboardUserMapper.selectByUserName(eq(TEST_USER_NAME))).willReturn(dashboardUserDO);

        DashboardUserVO dashboardUserVO = dashboardUserService.findByUserName(TEST_USER_NAME);
        assertEquals(TEST_ID, dashboardUserVO.getId());
        assertEquals(TEST_USER_NAME, dashboardUserVO.getUserName());
        assertEquals(TEST_PASSWORD, dashboardUserVO.getPassword());
        verify(dashboardUserMapper).selectByUserName(eq(TEST_USER_NAME));
    }

    @Test
    public void testListByPage() {
        DashboardUserQuery dashboardUserQuery = new DashboardUserQuery();
        dashboardUserQuery.setUserName(TEST_USER_NAME);
        PageParameter pageParameter = new PageParameter();
        dashboardUserQuery.setPageParameter(pageParameter);

        given(dashboardUserMapper.countByQuery(eq(dashboardUserQuery))).willReturn(1);
        DashboardUserDO dashboardUserDO = createDashboardUserDO();
        given(dashboardUserMapper.selectByQuery(eq(dashboardUserQuery))).willReturn(Collections.singletonList(dashboardUserDO));

        CommonPager<DashboardUserVO> commonPager = dashboardUserService.listByPage(dashboardUserQuery);
        assertThat(commonPager.getDataList()).isNotNull().isNotEmpty();
        assertEquals(1, commonPager.getDataList().size());
        assertEquals(TEST_ID, commonPager.getDataList().get(0).getId());
        verify(dashboardUserMapper).countByQuery(eq(dashboardUserQuery));
        verify(dashboardUserMapper).selectByQuery(eq(dashboardUserQuery));
    }

    @Test
    public void testLogin() {
        ReflectionTestUtils.setField(dashboardUserService, "secretProperties", secretProperties);
        ReflectionTestUtils.setField(dashboardUserService, "jwtProperties", jwtProperties);
        DashboardUserDO dashboardUserDO = createDashboardUserDO();
        String key = "2095132720951327";
        String iv = "6075877187097700";
        when(secretProperties.getKey()).thenReturn(key, key);
        when(secretProperties.getIv()).thenReturn(iv, iv);
        when(dashboardUserMapper.findByQuery(eq(TEST_USER_NAME), anyString())).thenReturn(dashboardUserDO);
        given(ldapTemplate.authenticate(anyString(), anyString(), anyString())).willReturn(true);
        given(roleMapper.findByRoleName("default")).willReturn(RoleDO.buildRoleDO(new RoleDTO("1", "test", null, null)));

        // test loginByLdap
        LdapProperties ldapProperties = new LdapProperties();
        ldapProperties.setBaseDn("test");
        ReflectionTestUtils.setField(dashboardUserService, "ldapProperties", ldapProperties);
        ReflectionTestUtils.setField(dashboardUserService, "ldapTemplate", ldapTemplate);
        LoginDashboardUserVO loginDashboardUserVO = dashboardUserService.login(TEST_USER_NAME, TEST_PASSWORD);
        assertEquals(TEST_USER_NAME, loginDashboardUserVO.getUserName());
        assertEquals(AesUtils.aesEncryption(TEST_PASSWORD, secretProperties.getKey(), secretProperties.getIv()), loginDashboardUserVO.getPassword());

        // test loginByDatabase
        ReflectionTestUtils.setField(dashboardUserService, "ldapTemplate", null);
        assertLoginSuccessful(dashboardUserDO, dashboardUserService.login(TEST_USER_NAME, TEST_PASSWORD));
        verify(dashboardUserMapper).findByQuery(eq(TEST_USER_NAME), anyString());
        assertLoginSuccessful(dashboardUserDO, dashboardUserService.login(TEST_USER_NAME, TEST_PASSWORD));
        verify(dashboardUserMapper, times(2)).findByQuery(eq(TEST_USER_NAME), anyString());
    }

    private DashboardUserDO createDashboardUserDO() {
        return DashboardUserDO.builder()
                .id(TEST_ID).userName(TEST_USER_NAME).password(TEST_PASSWORD)
                .dateCreated(new Timestamp(System.currentTimeMillis()))
                .dateUpdated(new Timestamp(System.currentTimeMillis()))
                .build();
    }

    private void assertLoginSuccessful(final DashboardUserDO dashboardUserDO, final DashboardUserVO dashboardUserVO) {
        assertEquals(dashboardUserDO.getId(), dashboardUserVO.getId());
        assertEquals(dashboardUserDO.getUserName(), dashboardUserVO.getUserName());
        assertEquals(dashboardUserDO.getPassword(), dashboardUserVO.getPassword());
    }
}
