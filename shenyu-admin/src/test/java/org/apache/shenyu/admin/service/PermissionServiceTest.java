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

import org.apache.shenyu.admin.mapper.DashboardUserMapper;
import org.apache.shenyu.admin.mapper.PermissionMapper;
import org.apache.shenyu.admin.mapper.ResourceMapper;
import org.apache.shenyu.admin.mapper.UserRoleMapper;
import org.apache.shenyu.admin.model.custom.UserInfo;
import org.apache.shenyu.admin.model.entity.DashboardUserDO;
import org.apache.shenyu.admin.model.entity.PermissionDO;
import org.apache.shenyu.admin.model.entity.ResourceDO;
import org.apache.shenyu.admin.model.entity.UserRoleDO;
import org.apache.shenyu.admin.model.vo.PermissionMenuVO;
import org.apache.shenyu.admin.service.impl.PermissionServiceImpl;
import org.apache.shenyu.admin.service.impl.ResourceServiceImpl;
import org.apache.shenyu.admin.spring.SpringBeanUtils;
import org.apache.shenyu.admin.utils.JwtUtils;
import org.apache.shiro.SecurityUtils;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.MockitoJUnitRunner;
import org.mockito.stubbing.Answer;
import org.springframework.context.ConfigurableApplicationContext;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.mockStatic;

/**
 * add test case for {@link PermissionServiceImpl}.
 */
@RunWith(MockitoJUnitRunner.class)
public final class PermissionServiceTest {

    @Mock
    private DashboardUserMapper mockDashboardUserMapper;

    @Mock
    private UserRoleMapper mockUserRoleMapper;

    @Mock
    private PermissionMapper mockPermissionMapper;

    @Mock
    private ResourceMapper mockResourceMapper;

    private PermissionServiceImpl permissionServiceImplUnderTest;

    @Mock
    private org.apache.shiro.mgt.SecurityManager securityManager;

    @Before
    public void setUp() throws Exception {
        SecurityUtils.setSecurityManager(securityManager);
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        final DashboardUserDO dashboardUserDO = DashboardUserDO.builder().id("1").userName("admin").role(1).enabled(true).build();
        final UserRoleDO userRoleDO = UserRoleDO.builder().userId("1").roleId("1346358560427216896")
                .id("1351007709096976384").dateCreated(new Timestamp(1610940313000L)).dateUpdated(new Timestamp(1610940313000L)).build();
        List<PermissionDO> permissionDOS = new ArrayList<>();
        permissionDOS.add(PermissionDO.builder().objectId("1346358560427216896").resourceId("1346775491550474240").id("1351007708572688384").build());
        permissionDOS.add(PermissionDO.builder().objectId("1346358560427216896").resourceId("1346776175553376256").id("1351007708585271296").build());
        permissionDOS.add(PermissionDO.builder().objectId("1346358560427216896").resourceId("1346777157943259136").id("1351007708593659904").build());
        permissionDOS.add(PermissionDO.builder().objectId("1346358560427216896").resourceId("1347053375029653504").id("1351007708593659914").build());
        final ResourceDO resourceDO1 = ResourceDO.builder().title("SHENYU.MENU.PLUGIN.LIST").name("plug").url("/plug").component("PluginList")
                .resourceType(0).sort(0).icon("dashboard").isLeaf(false).isRoute(0).status(1)
                .dateCreated(new Timestamp(1610940313000L))
                .dateUpdated(new Timestamp(1610940313000L))
                .id("1346775491550474240").build();
        final ResourceDO resourceDO2 = ResourceDO.builder().title("SHENYU.MENU.SYSTEM.MANAGMENT").name("system").url("/system").component("system")
                .resourceType(0).sort(1).icon("setting").isLeaf(false).isRoute(0).status(1)
                .dateCreated(new Timestamp(1610940313000L))
                .dateUpdated(new Timestamp(1610940313000L))
                .id("1346776175553376256").build();
        final ResourceDO resourceDO3 = ResourceDO.builder().parentId("1346776175553376256").title("SHENYU.MENU.SYSTEM.MANAGMENT.USER").name("manage").url("/system/manage").component("manage")
                .resourceType(1).sort(1).icon("").isLeaf(false).isRoute(0).status(1)
                .dateCreated(new Timestamp(1610940313000L))
                .dateUpdated(new Timestamp(1610940313000L))
                .id("1346777157943259136").build();
        final ResourceDO resourceDO4 = ResourceDO.builder().parentId("1347027526339538944").title("SHENYU.BUTTON.PLUGIN.SYNCHRONIZE")
                .resourceType(2).sort(4).isLeaf(true).isRoute(0).perms("plugin:sign:modify").status(1)
                .dateCreated(new Timestamp(1610940313000L))
                .dateUpdated(new Timestamp(1610940313000L))
                .id("1347053375029653504").build();
        when(mockDashboardUserMapper.selectByUserName("admin")).thenReturn(dashboardUserDO);
        when(mockUserRoleMapper.findByUserId("1")).thenReturn(Collections.singletonList(userRoleDO));
        when(mockPermissionMapper.findByObjectId("1346358560427216896")).thenReturn(permissionDOS);
        when(mockResourceMapper.selectById("1346775491550474240")).thenReturn(resourceDO1);
        when(mockResourceMapper.selectById("1346776175553376256")).thenReturn(resourceDO2);
        when(mockResourceMapper.selectById("1346777157943259136")).thenReturn(resourceDO3);
        when(mockResourceMapper.selectById("1347053375029653504")).thenReturn(resourceDO4);
        when(mockResourceMapper.selectAll()).thenReturn(Arrays.asList(resourceDO1, resourceDO2, resourceDO3, resourceDO4));
        ResourceService resourceService = new ResourceServiceImpl(mockResourceMapper, mockPermissionMapper);
        permissionServiceImplUnderTest = new PermissionServiceImpl(mockDashboardUserMapper, mockUserRoleMapper, mockPermissionMapper, mockResourceMapper, resourceService);
    }

    @Test
    public void testGetPermissionMenu() {
        try (MockedStatic<JwtUtils> mocked = mockStatic(JwtUtils.class)) {
            mocked.when(JwtUtils::getUserInfo)
                    .thenAnswer((Answer<UserInfo>) invocation -> UserInfo.builder().userId("1").userName("admin").build());
            final PermissionMenuVO expectedResult = new PermissionMenuVO(Arrays.asList(
                    new PermissionMenuVO.MenuInfo("1346776175553376256", "system", "/system", "system",
                            new PermissionMenuVO.Meta("setting", "SHENYU.MENU.SYSTEM.MANAGMENT"), Collections.singletonList(
                            new PermissionMenuVO.MenuInfo("1346777157943259136", "manage", "/system/manage", "manage",
                                    new PermissionMenuVO.Meta("", "SHENYU.MENU.SYSTEM.MANAGMENT.USER"), Collections.emptyList(), 1)
                    ), 1),
                    new PermissionMenuVO.MenuInfo("1346775491550474240", "plug", "/plug", "PluginList",
                            new PermissionMenuVO.Meta("dashboard", "SHENYU.MENU.PLUGIN.LIST"), Collections.emptyList(), 0)
            ),
                    Collections.singletonList(new PermissionMenuVO.AuthPerm("plugin:sign:modify", "SHENYU.BUTTON.PLUGIN.SYNCHRONIZE", null)),
                    Collections.singletonList(new PermissionMenuVO.AuthPerm("plugin:sign:modify", "SHENYU.BUTTON.PLUGIN.SYNCHRONIZE", null)));
            String token = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJhZG1pbiIsImlhdCI6MTYxMTc5MjEzOX0.eFORUk5kZawKLTsfRYojy-uaaDySo9kWtcfgxISS_3g";
            final PermissionMenuVO result = permissionServiceImplUnderTest.getPermissionMenu(token);
            assertThat(result, is(expectedResult));
        }
    }

    @Test
    public void testGetAuthPermByUserName() {
        final Set<String> result = permissionServiceImplUnderTest.getAuthPermByUserName("admin");
        assertThat(result.size(), is(1));
    }
}
