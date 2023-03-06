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
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.model.dto.BatchCommonDTO;
import org.apache.shenyu.admin.model.dto.PluginDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.PluginQuery;
import org.apache.shenyu.admin.model.query.PluginQueryCondition;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.PluginVO;
import org.apache.shenyu.admin.service.PageService;
import org.apache.shenyu.admin.service.PluginService;
import org.apache.shenyu.admin.service.SyncDataService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.admin.validation.annotation.Existed;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shiro.authz.annotation.RequiresPermissions;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestBody;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.util.List;

/**
 * this is plugin controller.
 */
@Validated
@RestController
@RequestMapping("/plugin")
public class PluginController implements PagedController<PluginQueryCondition, PluginVO> {
    
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
    public ShenyuAdminResult queryPlugins(final String name, final Integer enabled,
                                          @NotNull final Integer currentPage,
                                          @NotNull final Integer pageSize) {
        CommonPager<PluginVO> commonPager = pluginService.listByPage(new PluginQuery(name, enabled, new PageParameter(currentPage, pageSize)));
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, commonPager);
    }
    
    /**
     * query All plugins.
     *
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
    @RequiresPermissions("system:plugin:edit")
    public ShenyuAdminResult detailPlugin(@PathVariable("id")
                                          @Existed(message = "plugin is not existed",
                                                  provider = PluginMapper.class) final String id) {
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
    @RequiresPermissions("system:plugin:add")
    public ShenyuAdminResult createPlugin(@Valid @ModelAttribute final PluginDTO pluginDTO) {
        return ShenyuAdminResult.success(pluginService.createOrUpdate(pluginDTO));
    }

    
    /**
     * update plugin.
     *
     * @param id        primary key.
     * @param pluginDTO plugin.
     * @return {@linkplain ShenyuAdminResult}
     */
    @PutMapping("/{id}")
    @RequiresPermissions("system:plugin:edit")
    public ShenyuAdminResult updatePlugin(@PathVariable("id")
                                          @Existed(message = "plugin is not existed",
                                                  provider = PluginMapper.class) final String id,
                                          @Valid @ModelAttribute final PluginDTO pluginDTO) {
        pluginDTO.setId(id);
        return createPlugin(pluginDTO);
    }

    /**
     * create plugin resource.
     * @param id primary key
     * @param pluginDTO plugin
     * @return {@linkplain ShenyuAdminResult}
     */
    @PutMapping("/createPluginResource/{id}")
    @RequiresPermissions("system:plugin:resource")
    public ShenyuAdminResult createPluginResource(@PathVariable("id")
                                                  @Existed(message = "plugin is not existed",
                                                          provider = PluginMapper.class) final String id,
                                                  @Valid @RequestBody final PluginDTO pluginDTO) {
        pluginDTO.setId(id);
        return ShenyuAdminResult.success(pluginService.createPluginResource(pluginDTO));
    }

    /**
     * delete plugins.
     *
     * @param ids primary key.
     * @return {@linkplain ShenyuAdminResult}
     */
    @DeleteMapping("/batch")
    @RequiresPermissions("system:plugin:delete")
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
    @RequiresPermissions("system:plugin:disable")
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
    @RequiresPermissions("system:plugin:modify")
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
    public ShenyuAdminResult syncPluginData(@PathVariable("id")
                                            @Existed(message = "plugin is not existed",
                                                    provider = PluginMapper.class) final String id) {
        return ShenyuAdminResult.success(syncDataService.syncPluginData(id) ? ShenyuResultMessage.SYNC_SUCCESS : ShenyuResultMessage.SYNC_FAIL);
    }
    
    /**
     * active plugin snapshot.
     *
     * @return list
     */
    @GetMapping("/snapshot/active")
    public ShenyuAdminResult activePluginSnapshot() {
        return ShenyuAdminResult.success(pluginService.activePluginSnapshot());
    }
    
    @Override
    public PageService<PluginQueryCondition, PluginVO> pageService() {
        return pluginService;
    }
}
