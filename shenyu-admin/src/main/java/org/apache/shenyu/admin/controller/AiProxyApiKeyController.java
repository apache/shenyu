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
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.aspect.annotation.RestApi;
import org.apache.shenyu.admin.model.dto.BatchCommonDTO;
import org.apache.shenyu.admin.model.dto.ProxyApiKeyDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.ProxyApiKeyQuery;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.ProxyApiKeyVO;
import org.apache.shenyu.admin.service.AiProxyApiKeyService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.admin.service.PageService;
import org.apache.shiro.authz.annotation.RequiresPermissions;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

/** AiProxyApiKeyController. */
@RestApi("/selector/{selectorId}/ai-proxy-apikey")
public class AiProxyApiKeyController implements PagedController<ProxyApiKeyQuery, ProxyApiKeyVO> {

    private final AiProxyApiKeyService aiProxyApiKeyService;

    public AiProxyApiKeyController(final AiProxyApiKeyService aiProxyApiKeyService) {
        this.aiProxyApiKeyService = aiProxyApiKeyService;
    }

    /**
     * Create or bind real->proxy api key mapping.
     *
     * @param dto dto
     * @return result
     */
    @PostMapping
    @RequiresPermissions("system:aiProxyApiKey:add")
    public ShenyuAdminResult create(@Valid @RequestBody final ProxyApiKeyDTO dto) {
        int rows = aiProxyApiKeyService.create(dto);
        return rows > 0
                ? ShenyuAdminResult.success(ShenyuResultMessage.CREATE_SUCCESS, dto)
                : ShenyuAdminResult.error(ShenyuResultMessage.PARAMETER_ERROR);
    }

    /**
     * List mappings by page.
     *
     * @param namespaceId namespace id
     * @param currentPage current page
     * @param pageSize page size
     * @param proxyApiKey optional filter
     * @return list pager
     */
    @GetMapping
    @RequiresPermissions("system:aiProxyApiKey:list")
    public ShenyuAdminResult list(
            @RequestParam("namespaceId") @NotBlank final String namespaceId,
            @RequestParam("currentPage") @NotNull final Integer currentPage,
            @RequestParam("pageSize") @NotNull final Integer pageSize,
            @RequestParam(value = "proxyApiKey", required = false) final String proxyApiKey) {
        ProxyApiKeyQuery query = new ProxyApiKeyQuery();
        query.setNamespaceId(namespaceId);
        query.setProxyApiKey(proxyApiKey);
        query.setPageParameter(new PageParameter(currentPage, pageSize));
        CommonPager<ProxyApiKeyVO> pager = aiProxyApiKeyService.listByPage(query);
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, pager);
    }

    /**
     * Update mapping description or enabled flag.
     *
     * @param id id
     * @param dto dto
     * @return result
     */
    @PutMapping("/{id}")
    @RequiresPermissions("system:aiProxyApiKey:edit")
    public ShenyuAdminResult update(
            @PathVariable("id") final String id, @RequestBody final ProxyApiKeyDTO dto) {
        dto.setId(id);
        int rows = aiProxyApiKeyService.update(dto);
        return rows > 0
                ? ShenyuAdminResult.success(ShenyuResultMessage.UPDATE_SUCCESS)
                : ShenyuAdminResult.error(ShenyuResultMessage.PARAMETER_ERROR);
    }

    /**
     * Batch delete.
     *
     * @param batchCommonDTO ids container
     * @return result
     */
    @PostMapping("/batchDelete")
    @RequiresPermissions("system:aiProxyApiKey:delete")
    public ShenyuAdminResult batchDelete(@Valid @RequestBody final BatchCommonDTO batchCommonDTO) {
        int rows = aiProxyApiKeyService.delete(batchCommonDTO.getIds());
        return rows > 0
                ? ShenyuAdminResult.success(ShenyuResultMessage.DELETE_SUCCESS)
                : ShenyuAdminResult.error(ShenyuResultMessage.DELETE_FAIL);
    }

    /**
     * Batch enable/disable.
     *
     * @param batchCommonDTO ids and enabled
     * @return result
     */
    @PostMapping("/batchEnabled")
    @RequiresPermissions("system:aiProxyApiKey:disable")
    public ShenyuAdminResult batchEnabled(@Valid @RequestBody final BatchCommonDTO batchCommonDTO) {
        final String result =
                aiProxyApiKeyService.enabled(batchCommonDTO.getIds(), batchCommonDTO.getEnabled());
        if (StringUtils.isNoneBlank(result)) {
            return ShenyuAdminResult.error(result);
        }
        return ShenyuAdminResult.success(ShenyuResultMessage.ENABLE_SUCCESS);
    }

    @Override
    public PageService<ProxyApiKeyQuery, ProxyApiKeyVO> pageService() {
        return aiProxyApiKeyService;
    }
}
 