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

package org.dromara.soul.admin.service;

import org.dromara.soul.admin.config.properties.JwtProperties;
import org.dromara.soul.admin.model.entity.DashboardUserDO;
import org.dromara.soul.admin.model.entity.PermissionDO;
import org.dromara.soul.admin.model.entity.ResourceDO;
import org.dromara.soul.admin.model.entity.UserRoleDO;
import org.dromara.soul.admin.mapper.DashboardUserMapper;
import org.dromara.soul.admin.mapper.PermissionMapper;
import org.dromara.soul.admin.mapper.ResourceMapper;
import org.dromara.soul.admin.mapper.UserRoleMapper;
import org.dromara.soul.admin.service.impl.PermissionServiceImpl;
import org.dromara.soul.admin.service.impl.ResourceServiceImpl;
import org.dromara.soul.admin.spring.SpringBeanUtils;
import org.dromara.soul.admin.model.vo.PermissionMenuVO;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.context.ConfigurableApplicationContext;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * add test case for {@link PermissionServiceImpl}.
 *
 * @author HoldDie
 */
@RunWith(MockitoJUnitRunner.class)
public class PermissionServiceTest {

    @Mock
    private DashboardUserMapper mockDashboardUserMapper;

    @Mock
    private UserRoleMapper mockUserRoleMapper;

    @Mock
    private PermissionMapper mockPermissionMapper;

    @Mock
    private ResourceMapper mockResourceMapper;

    private ResourceService resourceService;

    private PermissionServiceImpl permissionServiceImplUnderTest;

    @Before
    public void setUp() throws Exception {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        JwtProperties jwtProperties = mock(JwtProperties.class);
        when(jwtProperties.getKey()).thenReturn("jwt-key");
        when(context.getBean(JwtProperties.class)).thenReturn(jwtProperties);
        SpringBeanUtils.getInstance().setCfgContext(context);
        final DashboardUserDO dashboardUserDO = DashboardUserDO.builder().id("1").userName("admin").role(1).enabled(true).build();
        final UserRoleDO userRoleDO = UserRoleDO.builder().userId("1").roleId("1346358560427216896")
                .id("1351007709096976384").dateCreated(new Timestamp(1610940313000L)).dateUpdated(new Timestamp(1610940313000L)).build();
        List<PermissionDO> permissionDOS = new ArrayList<>();
        permissionDOS.add(PermissionDO.builder().objectId("1346358560427216896").resourceId("1346775491550474240").id("1351007708572688384").build());
        permissionDOS.add(PermissionDO.builder().objectId("1346358560427216896").resourceId("1346776175553376256").id("1351007708585271296").build());
        permissionDOS.add(PermissionDO.builder().objectId("1346358560427216896").resourceId("1346777157943259136").id("1351007708593659904").build());
        permissionDOS.add(PermissionDO.builder().objectId("1346358560427216896").resourceId("1347053375029653504").id("1351007708593659914").build());
        final ResourceDO resourceDO1 = ResourceDO.builder().title("SOUL.MENU.PLUGIN.LIST").name("plug").url("/plug").component("PluginList")
                .resourceType(0).sort(0).icon("dashboard").isLeaf(false).isRoute(0).status(1)
                .dateCreated(new Timestamp(1610940313000L))
                .dateUpdated(new Timestamp(1610940313000L))
                .id("1346775491550474240").build();
        final ResourceDO resourceDO2 = ResourceDO.builder().title("SOUL.MENU.SYSTEM.MANAGMENT").name("system").url("/system").component("system")
                .resourceType(0).sort(1).icon("setting").isLeaf(false).isRoute(0).status(1)
                .dateCreated(new Timestamp(1610940313000L))
                .dateUpdated(new Timestamp(1610940313000L))
                .id("1346776175553376256").build();
        final ResourceDO resourceDO3 = ResourceDO.builder().parentId("1346776175553376256").title("SOUL.MENU.SYSTEM.MANAGMENT.USER").name("manage").url("/system/manage").component("manage")
                .resourceType(1).sort(1).icon("").isLeaf(false).isRoute(0).status(1)
                .dateCreated(new Timestamp(1610940313000L))
                .dateUpdated(new Timestamp(1610940313000L))
                .id("1346777157943259136").build();
        final ResourceDO resourceDO4 = ResourceDO.builder().parentId("1347027526339538944").title("SOUL.BUTTON.PLUGIN.SYNCHRONIZE")
                .resourceType(2).sort(4).isLeaf(true).isRoute(0).perms("plugin:sign:modify").status(1)
                .dateCreated(new Timestamp(1610940313000L))
                .dateUpdated(new Timestamp(1610940313000L))
                .id("1347053375029653504").build();
        when(mockDashboardUserMapper.selectByUserName("admin")).thenReturn(dashboardUserDO);
        when(mockUserRoleMapper.findByUserId("1")).thenReturn(Arrays.asList(userRoleDO));
        when(mockPermissionMapper.findByObjectId("1346358560427216896")).thenReturn(permissionDOS);
        when(mockResourceMapper.selectById("1346775491550474240")).thenReturn(resourceDO1);
        when(mockResourceMapper.selectById("1346776175553376256")).thenReturn(resourceDO2);
        when(mockResourceMapper.selectById("1346777157943259136")).thenReturn(resourceDO3);
        when(mockResourceMapper.selectById("1347053375029653504")).thenReturn(resourceDO4);
        when(mockResourceMapper.selectAll()).thenReturn(Arrays.asList(resourceDO1, resourceDO2, resourceDO3, resourceDO4));
        resourceService = new ResourceServiceImpl(mockResourceMapper, mockPermissionMapper);
        permissionServiceImplUnderTest = new PermissionServiceImpl(mockDashboardUserMapper, mockUserRoleMapper, mockPermissionMapper, mockResourceMapper, resourceService);
    }

    @Test
    public void testGetPermissionMenu() {
        final PermissionMenuVO expectedResult = new PermissionMenuVO(Arrays.asList(
                new PermissionMenuVO.MenuInfo("1346776175553376256", "system", "/system", "system",
                        new PermissionMenuVO.Meta("setting", "SOUL.MENU.SYSTEM.MANAGMENT"), Arrays.asList(
                        new PermissionMenuVO.MenuInfo("1346777157943259136", "manage", "/system/manage", "manage",
                                new PermissionMenuVO.Meta("", "SOUL.MENU.SYSTEM.MANAGMENT.USER"), Arrays.asList(), 1)
                ), 1),
                new PermissionMenuVO.MenuInfo("1346775491550474240", "plug", "/plug", "PluginList",
                        new PermissionMenuVO.Meta("dashboard", "SOUL.MENU.PLUGIN.LIST"), Arrays.asList(), 0)
        ),
                Arrays.asList(new PermissionMenuVO.AuthPerm("plugin:sign:modify", "SOUL.BUTTON.PLUGIN.SYNCHRONIZE", null)),
                Arrays.asList(new PermissionMenuVO.AuthPerm("plugin:sign:modify", "SOUL.BUTTON.PLUGIN.SYNCHRONIZE", null)));
        String token = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJhZG1pbiIsImlhdCI6MTYxMTc5MjEzOX0.eFORUk5kZawKLTsfRYojy-uaaDySo9kWtcfgxISS_3g";
        final PermissionMenuVO result = permissionServiceImplUnderTest.getPermissionMenu(token);
        assertThat(result, is(expectedResult));
    }

    @Test
    public void testGetAuthPermByUserName() {
        final Set<String> result = permissionServiceImplUnderTest.getAuthPermByUserName("admin");
        assertThat(result.size(), is(1));
    }
}
