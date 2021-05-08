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
import org.apache.shenyu.admin.service.PluginService;
import org.apache.shenyu.admin.service.SyncDataService;
import org.apache.shenyu.admin.utils.SoulResultMessage;
import org.apache.shenyu.admin.model.dto.BatchCommonDTO;
import org.apache.shenyu.admin.model.dto.PluginDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.PluginQuery;
import org.apache.shenyu.admin.model.result.SoulAdminResult;
import org.apache.shenyu.admin.model.vo.PluginVO;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
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
 * this is plugin controller.
 *
 * @author jiangxiaofeng(Nicholas)
 */
@RestController
@RequestMapping("/plugin")
public class PluginController {

    private final PluginService pluginService;

    private final SyncDataService syncDataService;

    /**
     * Instantiates a new Plugin controller.
     *
     * @param pluginService   the plugin service
     * @param syncDataService the sync data service
     */
    @Autowired(required = false)
    public PluginController(final PluginService pluginService,
                            final SyncDataService syncDataService) {
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
     * @return {@linkplain SoulAdminResult}
     */
    @GetMapping("")
    public SoulAdminResult queryPlugins(final String name, final Integer enabled, final Integer currentPage, final Integer pageSize) {
        CommonPager<PluginVO> commonPager = pluginService.listByPage(new PluginQuery(name, enabled, new PageParameter(currentPage, pageSize)));
        return SoulAdminResult.success(SoulResultMessage.QUERY_SUCCESS, commonPager);
    }

    /**
     * query All plugins.
     * @return {@linkplain SoulAdminResult}
     */
    @GetMapping("/all")
    public SoulAdminResult queryAllPlugins() {
        List<PluginData> pluginDataList = pluginService.listAll();
        return SoulAdminResult.success(SoulResultMessage.QUERY_SUCCESS, pluginDataList);
    }

    /**
     * detail plugin.
     *
     * @param id plugin id.
     * @return {@linkplain SoulAdminResult}
     */
    @GetMapping("/{id}")
    public SoulAdminResult detailPlugin(@PathVariable("id") final String id) {
        PluginVO pluginVO = pluginService.findById(id);
        return SoulAdminResult.success(SoulResultMessage.DETAIL_SUCCESS, pluginVO);
    }

    /**
     * create plugin.
     *
     * @param pluginDTO plugin.
     * @return {@linkplain SoulAdminResult}
     */
    @PostMapping("")
    public SoulAdminResult createPlugin(@RequestBody final PluginDTO pluginDTO) {
        String result = pluginService.createOrUpdate(pluginDTO);
        if (StringUtils.isNoneBlank(result)) {
            return SoulAdminResult.error(result);
        }
        return SoulAdminResult.success(SoulResultMessage.CREATE_SUCCESS);
    }

    /**
     * update plugin.
     *
     * @param id        primary key.
     * @param pluginDTO plugin.
     * @return {@linkplain SoulAdminResult}
     */
    @PutMapping("/{id}")
    public SoulAdminResult updatePlugin(@PathVariable("id") final String id, @RequestBody final PluginDTO pluginDTO) {
        Objects.requireNonNull(pluginDTO);
        pluginDTO.setId(id);
        final String result = pluginService.createOrUpdate(pluginDTO);
        if (StringUtils.isNoneBlank(result)) {
            return SoulAdminResult.error(result);
        }
        return SoulAdminResult.success(SoulResultMessage.UPDATE_SUCCESS);
    }

    /**
     * delete plugins.
     *
     * @param ids primary key.
     * @return {@linkplain SoulAdminResult}
     */
    @DeleteMapping("/batch")
    public SoulAdminResult deletePlugins(@RequestBody final List<String> ids) {
        final String result = pluginService.delete(ids);
        if (StringUtils.isNoneBlank(result)) {
            return SoulAdminResult.error(result);
        }
        return SoulAdminResult.success(SoulResultMessage.DELETE_SUCCESS);
    }

    /**
     * Enable plugins.
     *
     * @param batchCommonDTO the batch common dto
     * @return the mono
     */
    @PostMapping("/enabled")
    public SoulAdminResult enabled(@RequestBody final BatchCommonDTO batchCommonDTO) {
        final String result = pluginService.enabled(batchCommonDTO.getIds(), batchCommonDTO.getEnabled());
        if (StringUtils.isNoneBlank(result)) {
            return SoulAdminResult.error(result);
        }
        return SoulAdminResult.success(SoulResultMessage.ENABLE_SUCCESS);
    }


    /**
     * sync plugins.
     *
     * @return {@linkplain SoulAdminResult}
     */
    @PostMapping("/syncPluginAll")
    public SoulAdminResult syncPluginAll() {
        boolean success = syncDataService.syncAll(DataEventTypeEnum.REFRESH);
        if (success) {
            return SoulAdminResult.success(SoulResultMessage.SYNC_SUCCESS);
        } else {
            return SoulAdminResult.error(SoulResultMessage.SYNC_FAIL);
        }
    }

    /**
     * Sync plugin data.
     *
     * @param id the id
     * @return the mono
     */
    @PutMapping("/syncPluginData/{id}")
    public SoulAdminResult syncPluginData(@PathVariable("id") final String id) {
        boolean success = syncDataService.syncPluginData(id);
        if (success) {
            return SoulAdminResult.success(SoulResultMessage.SYNC_SUCCESS);
        } else {
            return SoulAdminResult.error(SoulResultMessage.SYNC_FAIL);
        }
    }
}
