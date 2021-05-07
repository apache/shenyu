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

import org.apache.shenyu.admin.service.RoleService;
import org.apache.shenyu.admin.utils.SoulResultMessage;
import org.apache.shenyu.admin.model.dto.RoleDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.RoleQuery;
import org.apache.shenyu.admin.model.result.SoulAdminResult;
import org.apache.shenyu.admin.model.vo.RoleEditVO;
import org.apache.shenyu.admin.model.vo.RoleVO;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * this is role controller.
 *
 * @author nuo-promise
 */
@RestController
@RequestMapping("/role")
public class RoleController {

    private final RoleService roleService;

    public RoleController(final RoleService roleService) {
        this.roleService = roleService;
    }

    /**
     * get all roles.
     *
     * @return {@linkplain SoulAdminResult}
     */
    @GetMapping("/getAllRoles")
    public SoulAdminResult selectAll() {
        return SoulAdminResult.success(SoulResultMessage.QUERY_SUCCESS, roleService.selectAll());
    }

    /**
     * query role.
     *
     * @param roleName role name
     * @param currentPage current page
     * @param pageSize page size
     * @return {@linkplain SoulAdminResult}
     */
    @GetMapping("")
    public SoulAdminResult queryRole(final String roleName, final Integer currentPage, final Integer pageSize) {
        CommonPager<RoleVO> commonPager = roleService.listByPage(new RoleQuery(roleName, new PageParameter(currentPage, pageSize)));
        return SoulAdminResult.success(SoulResultMessage.QUERY_SUCCESS, commonPager);
    }

    /**
     * detail role and permission info.
     * @param id role id
     * @return {@linkplain SoulAdminResult}
     */
    @GetMapping("/{id}")
    public SoulAdminResult detailRole(@PathVariable("id") final String id) {
        RoleEditVO roleEditVO = roleService.findById(id);
        return Optional.ofNullable(roleEditVO).map(item -> SoulAdminResult.success(SoulResultMessage.DETAIL_SUCCESS, item)).orElse(SoulAdminResult.error(SoulResultMessage.DETAIL_FAILED));
    }

    /**
     * create role.
     *
     * @param roleDTO role
     * @return {@linkplain SoulAdminResult}
     */
    @PostMapping("")
    public SoulAdminResult createRole(@RequestBody final RoleDTO roleDTO) {
        return Optional.ofNullable(roleDTO).map(item -> {
            if (roleDTO.getRoleName().equals("super")) {
                return SoulAdminResult.error(SoulResultMessage.ROLE_CREATE_ERROR);
            }
            Integer createCount = roleService.createOrUpdate(item);
            return SoulAdminResult.success(SoulResultMessage.CREATE_SUCCESS, createCount);
        }).orElse(SoulAdminResult.error(SoulResultMessage.CREATE_FAILED));
    }

    /**
     * update role and permission info.
     *
     * @param id primary key.
     * @param roleDTO role and permission info
     * @return {@linkplain SoulAdminResult}
     */
    @PutMapping("/{id}")
    public SoulAdminResult updateRole(@PathVariable("id") final String id, @RequestBody final RoleDTO roleDTO) {
        Objects.requireNonNull(roleDTO);
        roleDTO.setId(id);
        return SoulAdminResult.success(SoulResultMessage.UPDATE_SUCCESS, roleService.createOrUpdate(roleDTO));
    }

    /**
     * delete role info.
     *
     * @param ids primary keys.
     * @return {@linkplain SoulAdminResult}
     */
    @DeleteMapping("/batch")
    public SoulAdminResult deleteRole(@RequestBody final List<String> ids) {
        return SoulAdminResult.success(SoulResultMessage.DELETE_SUCCESS, roleService.delete(ids));
    }
}
