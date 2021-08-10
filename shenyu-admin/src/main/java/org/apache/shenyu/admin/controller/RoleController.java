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

import org.apache.shenyu.admin.model.dto.RoleDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.RoleQuery;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.RoleEditVO;
import org.apache.shenyu.admin.model.vo.RoleVO;
import org.apache.shenyu.admin.service.RoleService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import java.util.List;
import java.util.Optional;

/**
 * this is role controller.
 */
@Validated
@RestController
@RequestMapping("/role")
public class RoleController {

    private static final String SUPER = "super";

    private final RoleService roleService;

    public RoleController(final RoleService roleService) {
        this.roleService = roleService;
    }

    /**
     * get all roles.
     *
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/getAllRoles")
    public ShenyuAdminResult selectAll() {
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, roleService.selectAll());
    }

    /**
     * query role.
     *
     * @param roleName    role name
     * @param currentPage current page
     * @param pageSize    page size
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("")
    public ShenyuAdminResult queryRole(final String roleName, final Integer currentPage, final Integer pageSize) {
        CommonPager<RoleVO> commonPager = roleService.listByPage(new RoleQuery(roleName, new PageParameter(currentPage, pageSize)));
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, commonPager);
    }

    /**
     * detail role and permission info.
     *
     * @param id role id
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/{id}")
    public ShenyuAdminResult detailRole(@PathVariable("id") final String id) {
        RoleEditVO roleEditVO = roleService.findById(id);
        return Optional.ofNullable(roleEditVO)
                .map(item -> ShenyuAdminResult.success(ShenyuResultMessage.DETAIL_SUCCESS, item))
                .orElse(ShenyuAdminResult.error(ShenyuResultMessage.DETAIL_FAILED));
    }

    /**
     * create role.
     *
     * @param roleDTO role
     * @return {@linkplain ShenyuAdminResult}
     */
    @PostMapping("")
    public ShenyuAdminResult createRole(@Valid @RequestBody final RoleDTO roleDTO) {
        if (SUPER.equals(roleDTO.getRoleName())) {
            return ShenyuAdminResult.error(ShenyuResultMessage.ROLE_CREATE_ERROR);
        }
        return ShenyuAdminResult.success(ShenyuResultMessage.CREATE_SUCCESS, roleService.createOrUpdate(roleDTO));
    }

    /**
     * update role and permission info.
     *
     * @param id      primary key.
     * @param roleDTO role and permission info
     * @return {@linkplain ShenyuAdminResult}
     */
    @PutMapping("/{id}")
    public ShenyuAdminResult updateRole(@PathVariable("id") final String id, @Valid @RequestBody final RoleDTO roleDTO) {
        roleDTO.setId(id);
        return ShenyuAdminResult.success(ShenyuResultMessage.UPDATE_SUCCESS, roleService.createOrUpdate(roleDTO));
    }

    /**
     * delete role info.
     *
     * @param ids primary keys.
     * @return {@linkplain ShenyuAdminResult}
     */
    @DeleteMapping("/batch")
    public ShenyuAdminResult deleteRole(@RequestBody @NotEmpty final List<@NotBlank String> ids) {
        return ShenyuAdminResult.success(ShenyuResultMessage.DELETE_SUCCESS, roleService.delete(ids));
    }
}
