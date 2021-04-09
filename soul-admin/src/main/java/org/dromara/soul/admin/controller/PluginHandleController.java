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

package org.dromara.soul.admin.controller;

import org.dromara.soul.admin.model.dto.PluginHandleDTO;
import org.dromara.soul.admin.model.page.CommonPager;
import org.dromara.soul.admin.model.page.PageParameter;
import org.dromara.soul.admin.model.query.PluginHandleQuery;
import org.dromara.soul.admin.model.result.SoulAdminResult;
import org.dromara.soul.admin.service.PluginHandleService;
import org.dromara.soul.admin.model.vo.PluginHandleVO;
import org.dromara.soul.admin.utils.SoulResultMessage;
import org.springframework.beans.factory.annotation.Autowired;
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

/**
 * this is a plugin handle controller.
 * @author liangziqiang.
 */
@RestController
@RequestMapping("/plugin-handle")
public class PluginHandleController {
    private final PluginHandleService pluginHandleService;

    @Autowired(required = false)
    public PluginHandleController(final PluginHandleService pluginHandleService) {
        this.pluginHandleService = pluginHandleService;
    }

    /**
     * query plugin handle by plugin id.
     * @param pluginId  plugin id.
     * @param currentPage  current page.
     * @param pageSize page size
     * @return {@linkplain SoulAdminResult}
     */
    @GetMapping("")
    public SoulAdminResult queryPluginHandles(final String pluginId, final Integer currentPage, final Integer pageSize) {
        CommonPager<PluginHandleVO> commonPager = pluginHandleService.listByPage(new PluginHandleQuery(pluginId, null, new PageParameter(currentPage, pageSize)));
        return SoulAdminResult.success(SoulResultMessage.QUERY_SUCCESS, commonPager);
    }

    /**
     * query plugin handle by plugin id.
     * @param pluginId  plugin id.
     * @param type type 1:selector,2:rule
     * @return {@linkplain SoulAdminResult}
     */
    @GetMapping("/all/{pluginId}/{type}")
    public SoulAdminResult queryAllPluginHandlesByPluginId(@PathVariable("pluginId") final String pluginId, @PathVariable("type") final Integer type) {
        List<PluginHandleVO> pluginHandleVOS = pluginHandleService.list(pluginId, type);
        return SoulAdminResult.success(SoulResultMessage.QUERY_SUCCESS, pluginHandleVOS);
    }

    /**
     * get plugin handle detail.
     *
     * @param id plugin handle id.
     * @return {@linkplain SoulAdminResult}
     */
    @GetMapping("/{id}")
    public SoulAdminResult detailRule(@PathVariable("id") final String id) {
        PluginHandleVO pluginHandleVO = pluginHandleService.findById(id);
        return SoulAdminResult.success(SoulResultMessage.DETAIL_SUCCESS, pluginHandleVO);
    }

    /**
     * create plugin handle.
     * @param pluginHandleDTO {@link PluginHandleDTO}
     * @return {@link SoulAdminResult}
     */
    @PostMapping("")
    public SoulAdminResult createPluginHandle(@RequestBody final PluginHandleDTO pluginHandleDTO) {
        Integer createCount = pluginHandleService.createOrUpdate(pluginHandleDTO);
        return SoulAdminResult.success(SoulResultMessage.CREATE_SUCCESS, createCount);
    }

    /**
     * update plugin handle by id.
     * @param id plugin handle id
     * @param pluginHandleDTO {@linkplain PluginHandleDTO}
     * @return {@linkplain SoulAdminResult}
     */
    @PutMapping("/{id}")
    public SoulAdminResult updatePluginHandle(@PathVariable("id") final String id, @RequestBody final PluginHandleDTO pluginHandleDTO) {
        Objects.requireNonNull(pluginHandleDTO);
        pluginHandleDTO.setId(id);
        Integer updateCount = pluginHandleService.createOrUpdate(pluginHandleDTO);
        return SoulAdminResult.success(SoulResultMessage.UPDATE_SUCCESS, updateCount);
    }

    /**
     * batch delete some plugin handles by some id list.
     * @param ids plugin handle id list.
     * @return {@linkplain SoulAdminResult}
     */
    @DeleteMapping("/batch")
    public SoulAdminResult deletePluginHandles(@RequestBody final List<String> ids) {
        Integer deleteCount = pluginHandleService.deletePluginHandles(ids);
        return SoulAdminResult.success(SoulResultMessage.DELETE_SUCCESS, deleteCount);
    }
}
