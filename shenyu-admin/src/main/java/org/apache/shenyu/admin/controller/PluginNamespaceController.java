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
import org.apache.shenyu.admin.aspect.annotation.RestApi;
import org.apache.shenyu.admin.mapper.NamespaceMapper;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.PluginNsRelMapper;
import org.apache.shenyu.admin.model.dto.BatchCommonDTO;
import org.apache.shenyu.admin.model.dto.BatchNamespaceCommonDTO;
import org.apache.shenyu.admin.model.dto.PluginNamespaceDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.PluginNamespaceQuery;
import org.apache.shenyu.admin.model.query.PluginNamespaceQueryCondition;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.PluginNamespaceVO;
import org.apache.shenyu.admin.service.PageService;
import org.apache.shenyu.admin.service.PluginNamespaceService;
import org.apache.shenyu.admin.service.SyncDataService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.admin.validation.annotation.Existed;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shiro.authz.annotation.RequiresPermissions;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.util.List;

/**
 * this is plugin controller.
 */
@RestApi("/pluginNamespace")
public class PluginNamespaceController implements PagedController<PluginNamespaceQueryCondition, PluginNamespaceVO> {

    private final PluginNamespaceService pluginNamespaceService;

    private final SyncDataService syncDataService;

    public PluginNamespaceController(final PluginNamespaceService pluginNamespaceService, final SyncDataService syncDataService) {
        this.pluginNamespaceService = pluginNamespaceService;
        this.syncDataService = syncDataService;
    }

    /**
     * query plugins.
     *
     * @param name        plugin name.
     * @param enabled     plugin enabled.
     * @param namespaceId namespace id.
     * @param currentPage current page.
     * @param pageSize    page size.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("")
    public ShenyuAdminResult queryPlugins(final String name, final Integer enabled,
                                          @Existed(message = "namespace is not existed",
                                                  provider = NamespaceMapper.class) final String namespaceId,
                                          @NotNull final Integer currentPage,
                                          @NotNull final Integer pageSize) {
        CommonPager<PluginNamespaceVO> commonPager = pluginNamespaceService.listByPage(new PluginNamespaceQuery(name, enabled, new PageParameter(currentPage, pageSize), namespaceId));
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, commonPager);
    }

    /**
     * query All plugins.
     *
     * @param namespaceId namespace id.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/all/{namespaceId}")
    public ShenyuAdminResult queryAllPlugins(
            @PathVariable("namespaceId")
            @Existed(message = "namespace is not existed", provider = NamespaceMapper.class) final String namespaceId) {
        List<PluginData> pluginDataList = pluginNamespaceService.listAll(namespaceId);
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, pluginDataList);
    }

    /**
     * detail plugin.
     *
     * @param namespaceId namespace id.
     * @param id          id.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/id={id}&namespaceId={namespaceId}")
    @RequiresPermissions("system:plugin:edit")
    public ShenyuAdminResult detailPlugin(
            @PathVariable("namespaceId")
            @Existed(message = "namespace is not existed", provider = NamespaceMapper.class) final String namespaceId,
            @PathVariable("id")
            @Existed(message = "id is not existed",
                    provider = PluginNsRelMapper.class) final String id) {
        PluginNamespaceVO pluginNamespaceVO = pluginNamespaceService.findById(id, namespaceId);
        return ShenyuAdminResult.success(ShenyuResultMessage.DETAIL_SUCCESS, pluginNamespaceVO);
    }

//    /**
//     * create plugin.
//     *
//     * @param pluginDTO plugin.
//     * @return {@linkplain ShenyuAdminResult}
//     */
//    @PostMapping("")
//    @RequiresPermissions("system:plugin:add")
//    public ShenyuAdminResult createPlugin(@Valid @ModelAttribute final PluginDTO pluginDTO) {
//        return ShenyuAdminResult.success(pluginService.createOrUpdate(pluginDTO));
//    }


    /**
     * update plugin.
     *
     * @param namespaceId        namespace id.
     * @param pluginId           primary key.
     * @param pluginNamespaceDTO plugin namespace.
     * @return {@linkplain ShenyuAdminResult}
     */
    @PutMapping("/pluginId={pluginId}&namespaceId={namespaceId}")
    @RequiresPermissions("system:plugin:edit")
    public ShenyuAdminResult updatePlugin(
            @PathVariable("namespaceId")
            @Existed(message = "namespace is not existed", provider = NamespaceMapper.class) final String namespaceId,
            @PathVariable("pluginId")
            @Existed(message = "PluginMapper is not existed",
                    provider = PluginMapper.class) final String pluginId,
            @Valid @ModelAttribute final PluginNamespaceDTO pluginNamespaceDTO) {
        pluginNamespaceDTO.setPluginId(pluginId);
        pluginNamespaceDTO.setNamespaceId(namespaceId);
        return ShenyuAdminResult.success(pluginNamespaceService.update(pluginNamespaceDTO));
    }


    /**
     * delete plugins.
     *
     * @param batchNamespaceCommonDTO the batch namespace common dto
     * @return {@linkplain ShenyuAdminResult}
     */
    @DeleteMapping("/batch")
    @RequiresPermissions("system:plugin:delete")
    public ShenyuAdminResult deletePlugins(@Valid @RequestBody final BatchNamespaceCommonDTO batchNamespaceCommonDTO) {
        final String result = pluginNamespaceService.delete(batchNamespaceCommonDTO.getIds(), batchNamespaceCommonDTO.getNamespaceId());
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
        if (batchCommonDTO.getEnabled() == null) {
            return ShenyuAdminResult.error(ShenyuResultMessage.ENABLED_NOT_NULL);
        }
        final String result = pluginNamespaceService.enabled(batchCommonDTO.getIds(), batchCommonDTO.getEnabled(), batchCommonDTO.getNamespaceId());
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
     * @param namespaceId namespace id.
     * @param id          the id
     * @return the mono
     */
    @PutMapping("/syncPluginData/id={id}&namespaceId={namespaceId}")
    public ShenyuAdminResult syncPluginData(@PathVariable("namespaceId")
                                            @Existed(message = "namespace is not existed", provider = NamespaceMapper.class) final String namespaceId,
                                            @PathVariable("id")
                                            @Existed(message = "plugin is not existed",
                                                    provider = PluginMapper.class) final String id) {
        return ShenyuAdminResult.success(syncDataService.syncPluginData(id, namespaceId) ? ShenyuResultMessage.SYNC_SUCCESS : ShenyuResultMessage.SYNC_FAIL);
    }

//    /**
//     * active plugin snapshot.
//     *
//     * @return list
//     */
//    @GetMapping("/snapshot/active")
//    public ShenyuAdminResult activePluginSnapshot() {
//        return ShenyuAdminResult.success(pluginService.activePluginSnapshot());
//    }

    @Override
    public PageService<PluginNamespaceQueryCondition, PluginNamespaceVO> pageService() {
        return pluginNamespaceService;
    }
}
