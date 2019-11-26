/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.admin.controller;

import org.dromara.soul.admin.result.SoulAdminResult;
import org.dromara.soul.admin.service.DashboardUserService;
import org.dromara.soul.admin.service.EnumService;
import org.dromara.soul.admin.vo.DashboardUserVO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * this is platform controller.
 *
 * @author jiangxiaofeng(Nicholas)
 */
@RestController
@RequestMapping("/platform")
public class PlatformController {

    private final DashboardUserService dashboardUserService;

    private final EnumService enumService;

    @Autowired(required = false)
    public PlatformController(final DashboardUserService dashboardUserService, final EnumService enumService) {
        this.dashboardUserService = dashboardUserService;
        this.enumService = enumService;
    }

    /**
     * login dashboard user.
     *
     * @param userName user name
     * @param password user password
     * @return {@linkplain SoulAdminResult}
     */
    @GetMapping("/login")
    public SoulAdminResult loginDashboardUser(final String userName, final String password) {
        DashboardUserVO dashboardUserVO = dashboardUserService.findByQuery(userName, password);
        return SoulAdminResult.success("login dashboard user success", dashboardUserVO);
    }

    /**
     * query enums.
     *
     * @return {@linkplain SoulAdminResult}
     */
    @GetMapping("/enum")
    public SoulAdminResult queryEnums() {
        return SoulAdminResult.success(enumService.list());
    }
}
