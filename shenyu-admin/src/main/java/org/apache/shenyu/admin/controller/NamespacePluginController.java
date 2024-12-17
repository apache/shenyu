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

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotNull;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.aspect.annotation.RestApi;
import org.apache.shenyu.admin.mapper.NamespaceMapper;
import org.apache.shenyu.admin.mapper.NamespacePluginRelMapper;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.model.dto.BatchCommonDTO;
import org.apache.shenyu.admin.model.dto.BatchNamespaceCommonDTO;
import org.apache.shenyu.admin.model.dto.NamespacePluginDTO;
import org.apache.shenyu.admin.model.dto.NamespaceSyncDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.NamespacePluginQuery;
import org.apache.shenyu.admin.model.query.NamespacePluginQueryCondition;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.NamespacePluginVO;
import org.apache.shenyu.admin.service.NamespacePluginService;
import org.apache.shenyu.admin.service.PageService;
import org.apache.shenyu.admin.service.SyncDataService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.admin.validation.annotation.Existed;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shiro.authz.annotation.RequiresPermissions;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;

/**
 * this is namespace plugin controller.
 */
@RestApi("/namespace-plugin")
public class NamespacePluginController implements PagedController<NamespacePluginQueryCondition, NamespacePluginVO> {

    private final NamespacePluginService namespacePluginService;

    private final SyncDataService syncDataService;

    public NamespacePluginController(final NamespacePluginService namespacePluginService, final SyncDataService syncDataService) {
        this.namespacePluginService = namespacePluginService;
        this.syncDataService = syncDataService;
    }

    /**
     * query plugins of namespace.
     *
     * @param name        plugin name.
     * @param enabled     plugin enabled.
     * @param namespaceId namespace id.
     * @param currentPage current page.
     * @param pageSize    page size.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping
    public ShenyuAdminResult queryPlugins(@RequestParam(name = "name", required = false) final String name,
                                          @RequestParam(name = "enabled", required = false) final Integer enabled,
                                          @Existed(message = "namespace is not existed",
                                                  provider = NamespaceMapper.class)
                                          @RequestParam(name = "namespaceId") final String namespaceId,
                                          @NotNull @RequestParam(name = "currentPage") final Integer currentPage,
                                          @NotNull @RequestParam(name = "pageSize") final Integer pageSize) {
        CommonPager<NamespacePluginVO> commonPager = namespacePluginService.listByPage(new NamespacePluginQuery(name, enabled, new PageParameter(currentPage, pageSize), namespaceId));
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, commonPager);
    }

    /**
     * query all plugins of namespace.
     *
     * @param namespaceId  namespace id.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/all/{namespaceId}")
    public ShenyuAdminResult queryAllNamespacePlugins(@Existed(message = "namespace is not existed", provider = NamespaceMapper.class)
                                             @PathVariable("namespaceId") final String namespaceId) {
        List<PluginData> pluginDataList = namespacePluginService.listAll(namespaceId);
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, pluginDataList);
    }

    /**
     * detail plugin of namespace.
     *
     * @param id namespace plugin relation id.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/{id}")
    @RequiresPermissions("system:plugin:edit")
    public ShenyuAdminResult detailNamespacePlugin(@Existed(message = "namespace plugin relation is not exist", provider = NamespacePluginRelMapper.class)
                                                   @PathVariable("id") final String id) {
        NamespacePluginVO namespacePluginVO = namespacePluginService.findById(id);
        return ShenyuAdminResult.success(ShenyuResultMessage.DETAIL_SUCCESS, namespacePluginVO);
    }

    /**
     * update plugin of namespace.
     *
     * @param id                namespace plugin relation id.
     * @param namespacePluginDTO plugin namespace.
     * @return {@linkplain ShenyuAdminResult}
     */
    @PutMapping("/{id}")
    @RequiresPermissions("system:plugin:edit")
    public ShenyuAdminResult updatePlugin(@Existed(message = "namespace plugin relation is not exist", provider = NamespacePluginRelMapper.class)
                                          @PathVariable("id") final String id,
                                          @Valid @RequestBody final NamespacePluginDTO namespacePluginDTO) {
        namespacePluginDTO.setId(id);
        return ShenyuAdminResult.success(namespacePluginService.update(namespacePluginDTO));
    }

    /**
     * add plugin of namespace.
     *
     * @param namespaceId        namespaceId.
     * @param pluginId           pluginId.
     * @return {@linkplain ShenyuAdminResult}
     */
    @PostMapping("/{namespaceId}/{pluginId}")
    @RequiresPermissions("system:plugin:edit")
    public ShenyuAdminResult generateNamespacePlugin(@Existed(message = "namespace is not exist", provider = NamespaceMapper.class)
                                          @PathVariable("namespaceId") final String namespaceId,
                                       @Existed(message = "plugin is not exist", provider = PluginMapper.class)
                                       @PathVariable("pluginId") final String pluginId) {
        return ShenyuAdminResult.success(namespacePluginService.create(namespaceId, pluginId));
    }



    /**
     * delete plugins of namespace.
     *
     * @param batchNamespaceCommonDTO the batch namespace common dto
     * @return {@linkplain ShenyuAdminResult}
     */
    @DeleteMapping("/batch")
    @RequiresPermissions("system:plugin:delete")
    public ShenyuAdminResult deleteNamespacePlugin(@Valid @RequestBody final BatchNamespaceCommonDTO batchNamespaceCommonDTO) {
        final String result = namespacePluginService.delete(batchNamespaceCommonDTO.getIds());
        if (StringUtils.isNoneBlank(result)) {
            return ShenyuAdminResult.error(result);
        }
        return ShenyuAdminResult.success(ShenyuResultMessage.DELETE_SUCCESS);
    }

    /**
     * Enable plugins of namespace.
     *
     * @param batchCommonDTO the batch common dto
     * @return the mono
     */
    @PostMapping("/enabled")
    @RequiresPermissions("system:plugin:disable")
    public ShenyuAdminResult enabled(@Valid @RequestBody final BatchCommonDTO batchCommonDTO) {
        final String result = namespacePluginService.enabled(batchCommonDTO.getIds(), batchCommonDTO.getEnabled());
        if (StringUtils.isNoneBlank(result)) {
            return ShenyuAdminResult.error(result);
        }
        return ShenyuAdminResult.success(ShenyuResultMessage.ENABLE_SUCCESS);
    }

    /**
     * Enable plugins of namespace.
     *
     * @param batchCommonDTO the batch common dto
     * @return the mono
     */
    @PostMapping("/enabledByNamespace")
    @RequiresPermissions("system:plugin:disable")
    public ShenyuAdminResult enabledByNamespace(@Valid @RequestBody final BatchCommonDTO batchCommonDTO) {
        final String result = namespacePluginService.enabled(batchCommonDTO.getNamespaceId(), batchCommonDTO.getIds(), batchCommonDTO.getEnabled());
        if (StringUtils.isNoneBlank(result)) {
            return ShenyuAdminResult.error(result);
        }
        return ShenyuAdminResult.success(ShenyuResultMessage.ENABLE_SUCCESS);
    }

    /**
     * sync plugins of namespace.
     *
     * @param namespaceSyncDTO the namespaceSync dto
     * @return {@linkplain ShenyuAdminResult}
     */
    @PostMapping("/syncPluginAll")
    @RequiresPermissions("system:plugin:modify")
    public ShenyuAdminResult syncPluginAll(@Valid @RequestBody final NamespaceSyncDTO namespaceSyncDTO) {
        boolean success = syncDataService.syncAllByNamespaceId(DataEventTypeEnum.REFRESH, namespaceSyncDTO.getNamespaceId());
        if (success) {
            return ShenyuAdminResult.success(ShenyuResultMessage.SYNC_SUCCESS);
        } else {
            return ShenyuAdminResult.error(ShenyuResultMessage.SYNC_FAIL);
        }
    }

    /**
     * sync plugin data.
     *
     * @param id          the id
     * @return {@linkplain ShenyuAdminResult}
     */
    @PutMapping("/syncPluginData")
    public ShenyuAdminResult syncPluginData(@RequestParam("id") final String id) {
        return ShenyuAdminResult.success(syncDataService.syncPluginData(id) ? ShenyuResultMessage.SYNC_SUCCESS : ShenyuResultMessage.SYNC_FAIL);
    }

    /**
     * active plugin snapshot of namespace.
     *
     * @param namespaceId the namespace id
     * @return list
     */
    @GetMapping("/snapshot/active")
    public ShenyuAdminResult activePluginSnapshot(final String namespaceId) {
        return ShenyuAdminResult.success(namespacePluginService.activePluginSnapshot(namespaceId));
    }
    
    /**
     * query plugins by namespace.
     *
     * @param namespace namespace.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/listByNamespace")
    public ShenyuAdminResult queryPluginsByNamespace(final String namespace) {
        List<PluginData> pluginDataList = namespacePluginService.listByNamespace(namespace);
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, pluginDataList);
    }

    @Override
    public PageService<NamespacePluginQueryCondition, NamespacePluginVO> pageService() {
        return namespacePluginService;
    }
}
