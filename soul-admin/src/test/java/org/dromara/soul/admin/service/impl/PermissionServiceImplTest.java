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

import org.dromara.soul.admin.entity.DashboardUserDO;
import org.dromara.soul.admin.entity.PermissionDO;
import org.dromara.soul.admin.entity.ResourceDO;
import org.dromara.soul.admin.entity.UserRoleDO;
import org.dromara.soul.admin.mapper.DashboardUserMapper;
import org.dromara.soul.admin.mapper.PermissionMapper;
import org.dromara.soul.admin.mapper.ResourceMapper;
import org.dromara.soul.admin.mapper.UserRoleMapper;
import org.dromara.soul.admin.vo.PermissionMenuVO;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

/**
 * add test case for {@link PermissionServiceImpl}.
 *
 * @author HoldDie
 * @version 1.0
 */
@RunWith(MockitoJUnitRunner.class)
public class PermissionServiceImplTest {

    @Mock
    private DashboardUserMapper mockDashboardUserMapper;
    @Mock
    private UserRoleMapper mockUserRoleMapper;
    @Mock
    private PermissionMapper mockPermissionMapper;
    @Mock
    private ResourceMapper mockResourceMapper;

    private PermissionServiceImpl permissionServiceImplUnderTest;

    @Before
    public void setUp() throws Exception {
        permissionServiceImplUnderTest = new PermissionServiceImpl(mockDashboardUserMapper, mockUserRoleMapper, mockPermissionMapper, mockResourceMapper);
    }

    @Test
    public void testGetPermissionMenu() {
//        final PermissionMenuVO expectedResult = new PermissionMenuVO(Arrays.asList(new PermissionMenuVO.MenuInfo("id", "name", "url", "component", new PermissionMenuVO.Meta("icon", "title"), Arrays.asList(), 0)), Arrays.asList(new PermissionMenuVO.AuthPerm("perms", "description")), Arrays.asList(new PermissionMenuVO.AuthPerm("perms", "description")));
//        when(mockUserRoleMapper.findByUserId("userId")).thenReturn(Arrays.asList(new UserRoleDO()));
//        when(mockDashboardUserMapper.selectByUserName("userName")).thenReturn(new DashboardUserDO());
//        when(mockPermissionMapper.findByObjectId("objectId")).thenReturn(Arrays.asList(new PermissionDO()));
//        when(mockResourceMapper.selectById("id")).thenReturn(new ResourceDO());
//        when(mockResourceMapper.selectAll()).thenReturn(Arrays.asList(new ResourceDO()));
//        final PermissionMenuVO result = permissionServiceImplUnderTest.getPermissionMenu("token");
//        assertEquals(expectedResult, result);
    }

    @Test
    public void testGetAuthPermByUserName() {
        DashboardUserDO dashboardUserDO = DashboardUserDO.builder().id("1")
                .userName("admin").role(1).enabled(true).build();
        UserRoleDO userRoleDO = UserRoleDO.builder().userId("1").roleId("1346358560427216896")
                .id("1351007709096976384")
                .dateCreated(new Timestamp(1610940313000L))
                .dateUpdated(new Timestamp(1610940313000L))
                .build();
        List<PermissionDO> permissionDOS = new ArrayList<PermissionDO>() {{
            add(PermissionDO.builder().objectId("1346358560427216896")
                    .resourceId("1346775491550474240").id("1351007708572688384").build());
            add(PermissionDO.builder().objectId("1346358560427216896")
                    .resourceId("1346776175553376256").id("1351007708585271296").build());
            add(PermissionDO.builder().objectId("1346358560427216896")
                    .resourceId("1346777157943259136").id("1351007708593659904").build());
        }};
        ResourceDO resourceDO1 = ResourceDO.builder().title("SOUL.MENU.PLUGIN.LIST").name("plug").url("/plug").component("PluginList")
                .resourceType(0).sort(0).icon("dashboard").isLeaf(false).isRoute(0).status(1)
                .dateCreated(new Timestamp(1610940313000L))
                .dateUpdated(new Timestamp(1610940313000L))
                .id("1346775491550474240").build();
        ResourceDO resourceDO2 = ResourceDO.builder().title("SOUL.MENU.SYSTEM.MANAGMENT").name("system").url("/system").component("system")
                .resourceType(0).sort(1).icon("setting").isLeaf(false).isRoute(0).status(1)
                .dateCreated(new Timestamp(1610940313000L))
                .dateUpdated(new Timestamp(1610940313000L))
                .id("1346776175553376256").build();
        ResourceDO resourceDO3 = ResourceDO.builder().parentId("1346776175553376256").title("SOUL.MENU.SYSTEM.MANAGMENT.USER").name("manage").url("/system/manage").component("manage")
                .resourceType(1).sort(1).icon("").isLeaf(false).isRoute(0).status(1)
                .dateCreated(new Timestamp(1610940313000L))
                .dateUpdated(new Timestamp(1610940313000L))
                .id("1346777157943259136").build();
        when(mockDashboardUserMapper.selectByUserName("admin")).thenReturn(dashboardUserDO);
        when(mockUserRoleMapper.findByUserId("1")).thenReturn(Arrays.asList(userRoleDO));
        when(mockPermissionMapper.findByObjectId("1346358560427216896")).thenReturn(permissionDOS);
        when(mockResourceMapper.selectById("1346775491550474240")).thenReturn(resourceDO1);
        when(mockResourceMapper.selectById("1346776175553376256")).thenReturn(resourceDO2);
        when(mockResourceMapper.selectById("1346777157943259136")).thenReturn(resourceDO3);
        final Set<String> result = permissionServiceImplUnderTest.getAuthPermByUserName("admin");
        assertThat(result.size(), is(0));
    }
}
