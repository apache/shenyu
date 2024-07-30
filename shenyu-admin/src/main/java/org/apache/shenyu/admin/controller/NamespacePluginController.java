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
import org.apache.shenyu.admin.mapper.NamespacePluginRelMapper;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.model.dto.BatchCommonDTO;
import org.apache.shenyu.admin.model.dto.BatchNamespaceCommonDTO;
import org.apache.shenyu.admin.model.dto.NamespacePluginDTO;
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
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.util.List;

/**
 * this is namespace plugin controller.
 */
@RestApi("/namespacePlugin")
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
    @GetMapping("")
    public ShenyuAdminResult queryPlugins(final String name, final Integer enabled,
                                          @Existed(message = "namespace is not existed",
                                                  provider = NamespaceMapper.class) final String namespaceId,
                                          @NotNull final Integer currentPage,
                                          @NotNull final Integer pageSize) {
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
    public ShenyuAdminResult queryAllPlugins(@PathVariable("namespaceId")
                                             @Existed(message = "namespace is not existed", provider = NamespaceMapper.class) final String namespaceId) {
        List<PluginData> pluginDataList = namespacePluginService.listAll(namespaceId);
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, pluginDataList);
    }

    /**
     * detail plugin of namespace.
     *
     * @param namespaceId namespace id.
     * @param id          id.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/id={id}&namespaceId={namespaceId}")
    @RequiresPermissions("system:plugin:edit")
    public ShenyuAdminResult detailPlugin(@PathVariable("namespaceId")
                                          @Existed(message = "namespace is not existed", provider = NamespaceMapper.class) final String namespaceId,
                                          @PathVariable("id")
                                          @Existed(message = "id is not existed", provider = NamespacePluginRelMapper.class) final String id) {
        NamespacePluginVO namespacePluginVO = namespacePluginService.findById(id, namespaceId);
        return ShenyuAdminResult.success(ShenyuResultMessage.DETAIL_SUCCESS, namespacePluginVO);
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
     * update plugin of namespace.
     *
     * @param namespaceId        namespace id.
     * @param pluginId           primary key.
     * @param namespacePluginDTO plugin namespace.
     * @return {@linkplain ShenyuAdminResult}
     */
    @PutMapping("/pluginId={pluginId}&namespaceId={namespaceId}")
    @RequiresPermissions("system:plugin:edit")
    public ShenyuAdminResult updatePlugin(@PathVariable("namespaceId")
                                          @Existed(message = "namespace is not existed", provider = NamespaceMapper.class) final String namespaceId,
                                          @PathVariable("pluginId")
                                          @Existed(message = "PluginMapper is not existed", provider = PluginMapper.class) final String pluginId,
                                          @Valid @ModelAttribute final NamespacePluginDTO namespacePluginDTO) {
        namespacePluginDTO.setPluginId(pluginId);
        namespacePluginDTO.setNamespaceId(namespaceId);
        return ShenyuAdminResult.success(namespacePluginService.update(namespacePluginDTO));
    }


    /**
     * delete plugins of namespace.
     *
     * @param batchNamespaceCommonDTO the batch namespace common dto
     * @return {@linkplain ShenyuAdminResult}
     */
    @DeleteMapping("/batch")
    @RequiresPermissions("system:plugin:delete")
    public ShenyuAdminResult deletePlugins(@Valid @RequestBody final BatchNamespaceCommonDTO batchNamespaceCommonDTO) {
        final String result = namespacePluginService.delete(batchNamespaceCommonDTO.getIds(), batchNamespaceCommonDTO.getNamespaceId());
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
        final String result = namespacePluginService.enabled(batchCommonDTO.getIds(), batchCommonDTO.getEnabled(), batchCommonDTO.getNamespaceId());
        if (StringUtils.isNoneBlank(result)) {
            return ShenyuAdminResult.error(result);
        }
        return ShenyuAdminResult.success(ShenyuResultMessage.ENABLE_SUCCESS);
    }

    /**
     * sync plugins of namespace.
     *
     * @return {@linkplain ShenyuAdminResult}
     */
    @PostMapping("/syncPluginAll")
    @RequiresPermissions("system:plugin:modify")
    public ShenyuAdminResult syncPluginAll() {
        //todo:[Namespace] Synchronize based on namespaceId
        boolean success = syncDataService.syncAll(DataEventTypeEnum.REFRESH);
        if (success) {
            return ShenyuAdminResult.success(ShenyuResultMessage.SYNC_SUCCESS);
        } else {
            return ShenyuAdminResult.error(ShenyuResultMessage.SYNC_FAIL);
        }
    }

    /**
     * Sync plugin data of namespace.
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

    /**
     * active plugin snapshot of namespace.
     *
     * @return list
     */
    @GetMapping("/snapshot/active")
    public ShenyuAdminResult activePluginSnapshot() {
        return ShenyuAdminResult.success(namespacePluginService.activePluginSnapshot());
    }

    @Override
    public PageService<NamespacePluginQueryCondition, NamespacePluginVO> pageService() {
        return namespacePluginService;
    }
}
