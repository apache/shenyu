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

import java.util.List;

import org.apache.shenyu.admin.mapper.PluginHandleMapper;
import org.apache.shenyu.admin.model.dto.PluginHandleDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.PluginHandleQuery;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.PluginHandleVO;
import org.apache.shenyu.admin.service.PluginHandleService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.admin.validation.annotation.Existed;
import org.apache.shiro.authz.annotation.RequiresPermissions;
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

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

/**
 * this is a plugin handle controller.
 */
@Validated
@RestController
@RequestMapping("/plugin-handle")
public class PluginHandleController {
    
    private final PluginHandleService pluginHandleService;
    
    public PluginHandleController(final PluginHandleService pluginHandleService) {
        this.pluginHandleService = pluginHandleService;
    }
    
    /**
     * query plugin handle by plugin id.
     *
     * @param pluginId    plugin id.
     * @param field       plugin field.
     * @param currentPage current page.
     * @param pageSize    page size
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("")
    @RequiresPermissions("system:pluginHandler:list")
    public ShenyuAdminResult queryPluginHandles(final String pluginId, final String field,
                                                @RequestParam @NotNull final Integer currentPage,
                                                @RequestParam @NotNull final Integer pageSize) {
        CommonPager<PluginHandleVO> commonPager = pluginHandleService.listByPage(new PluginHandleQuery(pluginId, field, null, new PageParameter(currentPage, pageSize)));
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, commonPager);
    }
    
    /**
     * query plugin handle by plugin id.
     *
     * @param pluginId plugin id.
     * @param type     type 1:selector,2:rule
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/all/{pluginId}/{type}")
    public ShenyuAdminResult queryAllPluginHandlesByPluginId(@PathVariable("pluginId") final String pluginId, @PathVariable("type") final Integer type) {
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, pluginHandleService.list(pluginId, type));
    }
    
    /**
     * get plugin handle detail.
     *
     * @param id plugin handle id.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/{id}")
    @RequiresPermissions("system:pluginHandler:edit")
    public ShenyuAdminResult detailRule(@PathVariable("id") @Valid
                                        @Existed(provider = PluginHandleMapper.class,
                                                message = "rule not exited") final String id) {
        PluginHandleVO pluginHandleVO = pluginHandleService.findById(id);
        return ShenyuAdminResult.success(ShenyuResultMessage.DETAIL_SUCCESS, pluginHandleVO);
    }
    
    /**
     * create plugin handle.
     *
     * @param pluginHandleDTO {@link PluginHandleDTO}
     * @return {@link ShenyuAdminResult}
     */
    @PostMapping("")
    @RequiresPermissions("system:pluginHandler:add")
    public ShenyuAdminResult createPluginHandle(@Valid @RequestBody final PluginHandleDTO pluginHandleDTO) {
        Integer createCount = pluginHandleService.createOrUpdate(pluginHandleDTO);
        return ShenyuAdminResult.success(ShenyuResultMessage.CREATE_SUCCESS, createCount);
    }
    
    /**
     * update plugin handle by id.
     *
     * @param id              plugin handle id
     * @param pluginHandleDTO {@linkplain PluginHandleDTO}
     * @return {@linkplain ShenyuAdminResult}
     */
    @PutMapping("/{id}")
    @RequiresPermissions("system:pluginHandler:edit")
    public ShenyuAdminResult updatePluginHandle(@PathVariable("id") @Valid
                                                @Existed(provider = PluginHandleMapper.class,
                                                        message = "rule not exited") final String id,
                                                @Valid @RequestBody final PluginHandleDTO pluginHandleDTO) {
        pluginHandleDTO.setId(id);
        return ShenyuAdminResult.success(ShenyuResultMessage.UPDATE_SUCCESS, pluginHandleService.createOrUpdate(pluginHandleDTO));
    }
    
    /**
     * batch delete some plugin handles by some id list.
     *
     * @param ids plugin handle id list.
     * @return {@linkplain ShenyuAdminResult}
     */
    @DeleteMapping("/batch")
    @RequiresPermissions("system:pluginHandler:delete")
    public ShenyuAdminResult deletePluginHandles(@RequestBody @NotEmpty final List<@NotBlank String> ids) {
        return ShenyuAdminResult.success(ShenyuResultMessage.DELETE_SUCCESS, pluginHandleService.deletePluginHandles(ids));
    }
}
