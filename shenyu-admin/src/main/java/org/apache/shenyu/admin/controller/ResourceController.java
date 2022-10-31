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

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.admin.mapper.ResourceMapper;
import org.apache.shenyu.admin.model.dto.CreateResourceDTO;
import org.apache.shenyu.admin.model.dto.ResourceDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.ResourceQuery;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.PermissionMenuVO.MenuInfo;
import org.apache.shenyu.admin.model.vo.ResourceVO;
import org.apache.shenyu.admin.service.ResourceService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.admin.validation.annotation.Existed;
import org.apache.shiro.authz.annotation.Logical;
import org.apache.shiro.authz.annotation.RequiresPermissions;

import java.util.List;
import java.util.Optional;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * this is resource controller.
 */
@Validated
@RestController
@RequestMapping("/resource")
public class ResourceController {
    
    private final ResourceService resourceService;
    
    public ResourceController(final ResourceService resourceService) {
        this.resourceService = resourceService;
    }
    
    /**
     * query resource.
     *
     * @param title       resource title
     * @param currentPage current Page
     * @param pageSize    page size
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("")
    @RequiresPermissions("system:resource:list")
    public ShenyuAdminResult queryResource(final String title,
                                           @RequestParam @NotNull final Integer currentPage,
                                           @RequestParam @NotNull final Integer pageSize) {
        CommonPager<ResourceVO> commonPager = resourceService.listByPage(new ResourceQuery(title, new PageParameter(currentPage, pageSize)));
        if (CollectionUtils.isNotEmpty(commonPager.getDataList())) {
            return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, commonPager);
        }
        return ShenyuAdminResult.error(ShenyuResultMessage.QUERY_FAILED);
    }
    
    /**
     * get menu tree.
     *
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/menu")
    @RequiresPermissions("system:manager:configureDataPermission")
    public ShenyuAdminResult getMenuTree() {
        List<MenuInfo> menuInfoList = resourceService.getMenuTree();
        if (CollectionUtils.isNotEmpty(menuInfoList)) {
            return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, menuInfoList);
        }
        return ShenyuAdminResult.error(ShenyuResultMessage.QUERY_FAILED);
    }
    
    /**
     * get button by parentId.
     *
     * @param id resource id
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/button")
    @RequiresPermissions(value = {"system:resource:addButton", "system:resource:list"}, logical = Logical.OR)
    public ShenyuAdminResult getButton(final String id) {
        List<ResourceVO> resourceVOList = resourceService.findByParentId(id);
        if (CollectionUtils.isNotEmpty(resourceVOList)) {
            return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, resourceVOList);
        }
        return ShenyuAdminResult.error(ShenyuResultMessage.QUERY_FAILED);
    }
    
    /**
     * detail resource info.
     *
     * @param id role id
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/{id}")
    @RequiresPermissions(value = {"system:resource:list", "system:resource:editMenu", "system:resource:list"}, logical = Logical.OR)
    public ShenyuAdminResult detailResource(@PathVariable("id") final String id) {
        return Optional.ofNullable(resourceService.findById(id))
                .map(item -> ShenyuAdminResult.success(ShenyuResultMessage.DETAIL_SUCCESS, item))
                .orElse(ShenyuAdminResult.error(ShenyuResultMessage.DETAIL_FAILED));
    }
    
    /**
     * create resource.
     *
     * @param createResourceDTO resource dto
     * @return {@linkplain ShenyuAdminResult}
     */
    @PostMapping("")
    @RequiresPermissions(value = {"system:resource:addMenu", "system:resource:addButton"}, logical = Logical.OR)
    public ShenyuAdminResult createResource(@Valid @RequestBody final CreateResourceDTO createResourceDTO) {
        return ShenyuAdminResult.success(ShenyuResultMessage.CREATE_SUCCESS, resourceService.create(createResourceDTO));
    }
    
    /**
     * update resource.
     *
     * @param id          primary key.
     * @param resourceDTO resource info
     * @return {@linkplain ShenyuAdminResult}
     */
    @PutMapping("/{id}")
    @RequiresPermissions(value = {"system:resource:editMenu", "system:resource:editButton"}, logical = Logical.OR)
    public ShenyuAdminResult updateResource(@PathVariable("id") @Valid
                                            @Existed(provider = ResourceMapper.class,
                                                    message = "resource not existed") final String id,
                                            @RequestBody final ResourceDTO resourceDTO) {
        resourceDTO.setId(id);
        return ShenyuAdminResult.success(ShenyuResultMessage.UPDATE_SUCCESS, resourceService.update(resourceDTO));
    }
    
    /**
     * delete resource info.
     *
     * @param ids primary keys.
     * @return {@linkplain ShenyuAdminResult}
     */
    @DeleteMapping("/batch")
    @RequiresPermissions(value = {"system:resource:deleteMenu", "system:resource:deleteButton"}, logical = Logical.OR)
    public ShenyuAdminResult deleteResource(@RequestBody @NotEmpty final List<@NotBlank String> ids) {
        return ShenyuAdminResult.success(ShenyuResultMessage.DELETE_SUCCESS, resourceService.delete(ids));
    }
}
