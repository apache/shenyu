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

package org.apache.shenyu.admin.controller;

import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.PermissionMenuVO;
import org.apache.shenyu.admin.service.PermissionService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Arrays;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.when;

/**
 * add test case for {@link PermissionController}.
 */
@ExtendWith(MockitoExtension.class)
public class PermissionControllerTest {

    @Mock
    private PermissionService mockPermissionService;

    private PermissionController permissionController;

    @BeforeEach
    public void setUp() throws Exception {
        permissionController = new PermissionController(mockPermissionService);
    }

    @Test
    public void testGetUserPermissionByToken() {
        final PermissionMenuVO permissionMenuVO = new PermissionMenuVO(
                Arrays.asList(new PermissionMenuVO.MenuInfo("id", "name", "url", "component",
                        new PermissionMenuVO.Meta("icon", "title"), Arrays.asList(), 0)),
                Arrays.asList(new PermissionMenuVO.AuthPerm("perms1", "description1", "icon")),
                Arrays.asList(new PermissionMenuVO.AuthPerm("perms2", "description2", "icon")));
        when(mockPermissionService.getPermissionMenu("token")).thenReturn(permissionMenuVO);
        final ShenyuAdminResult result = permissionController.getUserPermissionByToken("token");
        assertThat(result.getCode(), is(CommonErrorCode.SUCCESSFUL));
        assertThat(result.getMessage(), is(ShenyuResultMessage.MENU_SUCCESS));
        assertThat(result.getData(), is(permissionMenuVO));
    }

    @Test
    public void testGetUserPermissionByTokenNull() {
        when(mockPermissionService.getPermissionMenu("token")).thenReturn(null);
        final ShenyuAdminResult result = permissionController.getUserPermissionByToken("token");
        assertThat(result.getCode(), is(CommonErrorCode.ERROR));
        assertThat(result.getMessage(), is(ShenyuResultMessage.MENU_FAILED));
    }
}
