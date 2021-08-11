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
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.Optional;

/**
 * this is permission controller.
 */
@RestController
@RequestMapping("/permission")
public class PermissionController {

    private final PermissionService permissionService;

    public PermissionController(final PermissionService permissionService) {
        this.permissionService = permissionService;
    }

    /**
     * get menu by token.
     *
     * @param token login success ack token
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/getUserPermissionByToken")
    public ShenyuAdminResult getUserPermissionByToken(@RequestParam(name = "token") final String token) {
        PermissionMenuVO permissionMenuVO = permissionService.getPermissionMenu(token);
        return Optional.ofNullable(permissionMenuVO)
                .map(item -> ShenyuAdminResult.success(ShenyuResultMessage.MENU_SUCCESS, item))
                .orElseGet(() -> ShenyuAdminResult.error(ShenyuResultMessage.MENU_FAILED));
    }
}
