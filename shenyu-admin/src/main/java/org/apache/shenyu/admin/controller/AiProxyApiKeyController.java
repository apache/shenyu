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
import org.apache.commons.collections4.CollectionUtils;
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

import org.apache.shenyu.admin.model.dto.BatchIdsDTO;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.common.constant.AdminConstants;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/** AiProxyApiKeyController. */
@RestApi("/selector/{selectorId}/ai-proxy-apikey")
public class AiProxyApiKeyController implements PagedController<ProxyApiKeyQuery, ProxyApiKeyVO> {

    private final AiProxyApiKeyService aiProxyApiKeyService;

    private final SelectorMapper selectorMapper;

    public AiProxyApiKeyController(final AiProxyApiKeyService aiProxyApiKeyService,
            final SelectorMapper selectorMapper) {
        this.aiProxyApiKeyService = aiProxyApiKeyService;
        this.selectorMapper = selectorMapper;
    }

    /**
     * Create or bind real->proxy api key mapping.
     *
     * @param selectorId selector id
     * @param dto        dto
     * @return result
     */
    @PostMapping
    @RequiresPermissions("system:aiProxyApiKey:add")
    public ShenyuAdminResult create(@PathVariable("selectorId") final String selectorId,
            @Valid @RequestBody final ProxyApiKeyDTO dto) {
        // derive namespaceId from selector to avoid mismatch
        final SelectorDO selector = selectorMapper.selectById(selectorId);
        if (Objects.nonNull(selector)) {
            dto.setNamespaceId(selector.getNamespaceId());
        }
        int rows = aiProxyApiKeyService.create(dto, selectorId);
        if (rows > 0) {
            final ProxyApiKeyVO vo = aiProxyApiKeyService.findById(dto.getId());
            return ShenyuAdminResult.success(ShenyuResultMessage.CREATE_SUCCESS, vo);
        }
        return ShenyuAdminResult.error(ShenyuResultMessage.PARAMETER_ERROR);
    }

    /**
     * List mappings by page.
     *
     * @param selectorId  selector id
     * @param namespaceId namespace id
     * @param currentPage current page
     * @param pageSize    page size
     * @param proxyApiKey optional filter
     * @return list pager
     */
    @GetMapping
    @RequiresPermissions("system:aiProxyApiKey:list")
    public ShenyuAdminResult list(
            @PathVariable("selectorId") final String selectorId,
            @RequestParam(value = "namespaceId", required = false) final String namespaceId,
            @RequestParam("currentPage") @NotNull final Integer currentPage,
            @RequestParam("pageSize") @NotNull final Integer pageSize,
            @RequestParam(value = "proxyApiKey", required = false) final String proxyApiKey) {
        // derive namespaceId from selectorId to ensure filtering is bound to selector
        final SelectorDO selector = selectorMapper.selectById(selectorId);
        if (Objects.isNull(selector)) {
            return ShenyuAdminResult.error(AdminConstants.ID_NOT_EXIST);
        }
        ProxyApiKeyQuery query = new ProxyApiKeyQuery();
        // only filter by selectorId to ensure strict scoping
        query.setSelectorId(selectorId);
        query.setNamespaceId(selector.getNamespaceId());
        query.setProxyApiKey(proxyApiKey);
        query.setPageParameter(new PageParameter(currentPage, pageSize));
        CommonPager<ProxyApiKeyVO> pager = aiProxyApiKeyService.listByPage(query);
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, pager);
    }

    /**
     * Update mapping description or enabled flag.
     *
     * @param selectorId selector id
     * @param id         id
     * @param dto        dto
     * @return result
     */
    @PutMapping("/{id}")
    @RequiresPermissions("system:aiProxyApiKey:edit")
    public ShenyuAdminResult update(
            @PathVariable("selectorId") final String selectorId,
            @PathVariable("id") final String id, @RequestBody final ProxyApiKeyDTO dto) {
        final ProxyApiKeyVO exist = aiProxyApiKeyService.findById(id);
        if (Objects.isNull(exist)) {
            return ShenyuAdminResult.error(AdminConstants.ID_NOT_EXIST);
        }
        if (!selectorId.equals(exist.getSelectorId())) {
            return ShenyuAdminResult.error(AdminConstants.PROXY_SELECTOR_ID_IS_NOT_EXIST);
        }
        dto.setId(id);
        int rows = aiProxyApiKeyService.update(dto);
        return rows > 0
                ? ShenyuAdminResult.success(ShenyuResultMessage.UPDATE_SUCCESS)
                : ShenyuAdminResult.error(ShenyuResultMessage.PARAMETER_ERROR);
    }

    /**
     * Batch delete.
     *
     * @param selectorId selector id
     * @param request    ids container
     * @return result
     */
    @PostMapping("/batchDelete")
    @RequiresPermissions("system:aiProxyApiKey:delete")
    public ShenyuAdminResult batchDelete(@PathVariable("selectorId") final String selectorId,
            @Valid @RequestBody final BatchIdsDTO request) {
        final List<String> allIds = request.getIds();
        final List<String> validIds = new ArrayList<>();
        if (CollectionUtils.isNotEmpty(allIds)) {
            List<ProxyApiKeyVO> exists = aiProxyApiKeyService.findByIds(allIds);
            for (ProxyApiKeyVO exist : exists) {
                if (Objects.nonNull(exist) && selectorId.equals(exist.getSelectorId())) {
                    validIds.add(exist.getId());
                }
            }
        }
        if (validIds.isEmpty()) {
            return ShenyuAdminResult.error(AdminConstants.PROXY_SELECTOR_ID_IS_NOT_EXIST);
        }
        int rows = aiProxyApiKeyService.delete(validIds);
        return rows > 0
                ? ShenyuAdminResult.success(ShenyuResultMessage.DELETE_SUCCESS)
                : ShenyuAdminResult.error(ShenyuResultMessage.DELETE_FAIL);
    }

    /**
     * Batch enable/disable.
     *
     * @param selectorId     selector id
     * @param batchCommonDTO ids and enabled
     * @return result
     */
    @PostMapping("/batchEnabled")
    @RequiresPermissions("system:aiProxyApiKey:disable")
    public ShenyuAdminResult batchEnabled(@PathVariable("selectorId") final String selectorId,
            @Valid @RequestBody final BatchCommonDTO batchCommonDTO) {
        final List<String> allIds = batchCommonDTO.getIds();
        final List<String> validIds = new ArrayList<>();
        if (CollectionUtils.isNotEmpty(allIds)) {
            List<ProxyApiKeyVO> exists = aiProxyApiKeyService.findByIds(allIds);
            for (ProxyApiKeyVO exist : exists) {
                if (Objects.nonNull(exist) && selectorId.equals(exist.getSelectorId())) {
                    validIds.add(exist.getId());
                }
            }
        }
        if (validIds.isEmpty()) {
            return ShenyuAdminResult.error(AdminConstants.PROXY_SELECTOR_ID_IS_NOT_EXIST);
        }
        final String result = aiProxyApiKeyService.enabled(validIds, batchCommonDTO.getEnabled());
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
