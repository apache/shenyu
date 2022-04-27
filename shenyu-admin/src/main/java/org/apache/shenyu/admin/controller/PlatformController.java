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
import org.apache.shenyu.admin.model.vo.LoginDashboardUserVO;
import org.apache.shenyu.admin.service.DashboardUserService;
import org.apache.shenyu.admin.service.EnumService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Optional;

/**
 * this is platform controller.
 */
@RestController
@RequestMapping("/platform")
public class PlatformController {

    private final DashboardUserService dashboardUserService;

    private final EnumService enumService;

    public PlatformController(final DashboardUserService dashboardUserService, final EnumService enumService) {
        this.dashboardUserService = dashboardUserService;
        this.enumService = enumService;
    }

    /**
     * login dashboard user.
     *
     * @param userName user name
     * @param password user password
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/login")
    public ShenyuAdminResult loginDashboardUser(final String userName, final String password) {
        LoginDashboardUserVO loginVO = dashboardUserService.login(userName, password);
        return Optional.ofNullable(loginVO)
                .map(loginStatus -> {
                    if (loginStatus.getEnabled()) {
                        return ShenyuAdminResult.success(ShenyuResultMessage.PLATFORM_LOGIN_SUCCESS, loginVO);
                    }
                    return ShenyuAdminResult.error(ShenyuResultMessage.LOGIN_USER_DISABLE_ERROR);
                }).orElse(ShenyuAdminResult.error(ShenyuResultMessage.PLATFORM_LOGIN_ERROR));
    }

    /**
     * query enums.
     *
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/enum")
    public ShenyuAdminResult queryEnums() {
        return ShenyuAdminResult.success(enumService.list());
    }
}
