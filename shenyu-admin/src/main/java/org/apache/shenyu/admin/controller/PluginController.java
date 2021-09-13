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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.dto.BatchCommonDTO;
import org.apache.shenyu.admin.model.dto.PluginDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.PluginQuery;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.PluginVO;
import org.apache.shenyu.admin.service.PluginService;
import org.apache.shenyu.admin.service.SyncDataService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
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

/**
 * this is plugin controller.
 */
@Validated
@RestController
@RequestMapping("/plugin")
public class PluginController {

    private final PluginService pluginService;

    private final SyncDataService syncDataService;

    public PluginController(final PluginService pluginService, final SyncDataService syncDataService) {
        this.pluginService = pluginService;
        this.syncDataService = syncDataService;
    }

    /**
     * query plugins.
     *
     * @param name        plugin name.
     * @param enabled     plugin enabled.
     * @param currentPage current page.
     * @param pageSize    page size.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("")
    public ShenyuAdminResult queryPlugins(final String name, final Integer enabled, final Integer currentPage, final Integer pageSize) {
        CommonPager<PluginVO> commonPager = pluginService.listByPage(new PluginQuery(name, enabled, new PageParameter(currentPage, pageSize)));
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, commonPager);
    }

    /**
     * query All plugins.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/all")
    public ShenyuAdminResult queryAllPlugins() {
        List<PluginData> pluginDataList = pluginService.listAll();
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, pluginDataList);
    }

    /**
     * detail plugin.
     *
     * @param id plugin id.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/{id}")
    public ShenyuAdminResult detailPlugin(@PathVariable("id") final String id) {
        PluginVO pluginVO = pluginService.findById(id);
        return ShenyuAdminResult.success(ShenyuResultMessage.DETAIL_SUCCESS, pluginVO);
    }

    /**
     * create plugin.
     *
     * @param pluginDTO plugin.
     * @return {@linkplain ShenyuAdminResult}
     */
    @PostMapping("")
    public ShenyuAdminResult createPlugin(@Valid @RequestBody final PluginDTO pluginDTO) {
        String result = pluginService.createOrUpdate(pluginDTO);
        if (StringUtils.isNoneBlank(result)) {
            return ShenyuAdminResult.error(result);
        }
        return ShenyuAdminResult.success(ShenyuResultMessage.CREATE_SUCCESS);
    }

    /**
     * update plugin.
     *
     * @param id        primary key.
     * @param pluginDTO plugin.
     * @return {@linkplain ShenyuAdminResult}
     */
    @PutMapping("/{id}")
    public ShenyuAdminResult updatePlugin(@PathVariable("id") final String id, @Valid @RequestBody final PluginDTO pluginDTO) {
        pluginDTO.setId(id);
        final String result = pluginService.createOrUpdate(pluginDTO);
        if (StringUtils.isNoneBlank(result)) {
            return ShenyuAdminResult.error(result);
        }
        return ShenyuAdminResult.success(ShenyuResultMessage.UPDATE_SUCCESS);
    }

    /**
     * delete plugins.
     *
     * @param ids primary key.
     * @return {@linkplain ShenyuAdminResult}
     */
    @DeleteMapping("/batch")
    public ShenyuAdminResult deletePlugins(@RequestBody @NotEmpty final List<@NotBlank String> ids) {
        final String result = pluginService.delete(ids);
        if (StringUtils.isNoneBlank(result)) {
            return ShenyuAdminResult.error(result);
        }
        return ShenyuAdminResult.success(ShenyuResultMessage.DELETE_SUCCESS);
    }

    /**
     * Enable plugins.
     *
     * @param batchCommonDTO the batch common dto
     * @return the mono
     */
    @PostMapping("/enabled")
    public ShenyuAdminResult enabled(@Valid @RequestBody final BatchCommonDTO batchCommonDTO) {
        final String result = pluginService.enabled(batchCommonDTO.getIds(), batchCommonDTO.getEnabled());
        if (StringUtils.isNoneBlank(result)) {
            return ShenyuAdminResult.error(result);
        }
        return ShenyuAdminResult.success(ShenyuResultMessage.ENABLE_SUCCESS);
    }

    /**
     * sync plugins.
     *
     * @return {@linkplain ShenyuAdminResult}
     */
    @PostMapping("/syncPluginAll")
    public ShenyuAdminResult syncPluginAll() {
        boolean success = syncDataService.syncAll(DataEventTypeEnum.REFRESH);
        if (success) {
            return ShenyuAdminResult.success(ShenyuResultMessage.SYNC_SUCCESS);
        } else {
            return ShenyuAdminResult.error(ShenyuResultMessage.SYNC_FAIL);
        }
    }

    /**
     * Sync plugin data.
     *
     * @param id the id
     * @return the mono
     */
    @PutMapping("/syncPluginData/{id}")
    public ShenyuAdminResult syncPluginData(@PathVariable("id") final String id) {
        boolean success = syncDataService.syncPluginData(id);
        if (success) {
            return ShenyuAdminResult.success(ShenyuResultMessage.SYNC_SUCCESS);
        } else {
            return ShenyuAdminResult.error(ShenyuResultMessage.SYNC_FAIL);
        }
    }
}
