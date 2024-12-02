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
import org.apache.shenyu.admin.aspect.annotation.RestApi;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.BatchCommonDTO;
import org.apache.shenyu.admin.model.dto.BatchNamespaceCommonDTO;
import org.apache.shenyu.admin.model.dto.SelectorDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageCondition;
import org.apache.shenyu.admin.model.query.SelectorQueryCondition;
import org.apache.shenyu.admin.model.result.AdminResult;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.SelectorVO;
import org.apache.shenyu.admin.service.PageService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.admin.validation.annotation.Existed;
import org.apache.shenyu.common.utils.ListUtil;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

/**
 * this is selector controller.
 */
@RestApi("/selector")
public class SelectorController implements PagedController<SelectorQueryCondition, SelectorVO> {

    private final SelectorService selectorService;

    public SelectorController(final SelectorService selectorService) {
        this.selectorService = selectorService;
    }

    /**
     * query Selectors.
     *
     * @param pluginId    plugin id.
     * @param name        selector name.
     * @param currentPage current page.
     * @param pageSize    page size.
     * @param namespaceId namespaceId.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("")
    public AdminResult<CommonPager<SelectorVO>> querySelectors(final String pluginId, final String name,
                                                               @RequestParam @NotNull final Integer currentPage,
                                                               @RequestParam @NotNull final Integer pageSize,
                                                               @RequestParam(value = "namespaceId", required = false) final String namespaceId
    ) {
        final SelectorQueryCondition condition = new SelectorQueryCondition();
        condition.setUserId(SessionUtil.visitor().getUserId());
        condition.setPlugin(ListUtil.of(pluginId));
        condition.setKeyword(name);
        if (namespaceId != null) {
            condition.setNamespaceId(namespaceId);
        }
        return searchAdaptor(new PageCondition<>(currentPage, pageSize, condition));
    }

    /**
     * detail selector.
     *
     * @param id selector id.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/{id}")
    public ShenyuAdminResult detailSelector(@PathVariable("id") @Valid
                                            @Existed(provider = SelectorMapper.class, message = "selector is not existed") final String id) {
        SelectorVO selectorVO = selectorService.findById(id);
        return ShenyuAdminResult.success(ShenyuResultMessage.DETAIL_SUCCESS, selectorVO);
    }

    /**
     * create selector.
     *
     * @param selectorDTO selector.
     * @return {@linkplain ShenyuAdminResult}
     */
    @PostMapping("")
    public ShenyuAdminResult createSelector(@Valid @RequestBody final SelectorDTO selectorDTO) {
        selectorService.createOrUpdate(selectorDTO);
        return ShenyuAdminResult.success(ShenyuResultMessage.CREATE_SUCCESS, selectorDTO.getId());
    }

    /**
     * update Selector.
     *
     * @param id          primary key.
     * @param selectorDTO selector.
     * @return {@linkplain ShenyuAdminResult}
     */
    @PutMapping("/{id}")
    public ShenyuAdminResult updateSelector(@PathVariable("id") @Valid
                                            @Existed(provider = SelectorMapper.class, message = "selector is not existed") final String id,
                                            @Valid @RequestBody final SelectorDTO selectorDTO) {
        selectorDTO.setId(id);
        Integer updateCount = selectorService.createOrUpdate(selectorDTO);
        return ShenyuAdminResult.success(ShenyuResultMessage.UPDATE_SUCCESS, updateCount);
    }

    /**
     * Batch enabled selector.
     *
     * @param batchCommonDTO the batch common dto
     * @return the shenyu result
     */
    @PostMapping("/batchEnabled")
    public ShenyuAdminResult batchEnabled(@Valid @RequestBody final BatchCommonDTO batchCommonDTO) {
        if (!selectorService.enabledByIdsAndNamespaceId(batchCommonDTO.getIds(), batchCommonDTO.getEnabled(), batchCommonDTO.getNamespaceId())) {
            return ShenyuAdminResult.error(ShenyuResultMessage.NOT_FOUND_EXCEPTION);
        }
        return ShenyuAdminResult.success(ShenyuResultMessage.ENABLE_SUCCESS);
    }

    /**
     * delete Selectors.
     *
     * @param batchNamespaceCommonDTO batchNamespaceCommonDTO.
     * @return {@linkplain ShenyuAdminResult}
     */
    @DeleteMapping("/batch")
    public ShenyuAdminResult deleteSelector(@Valid @RequestBody final BatchNamespaceCommonDTO batchNamespaceCommonDTO) {
        Integer deleteCount = selectorService.deleteByNamespaceId(batchNamespaceCommonDTO.getIds(), batchNamespaceCommonDTO.getNamespaceId());
        return ShenyuAdminResult.success(ShenyuResultMessage.DELETE_SUCCESS, deleteCount);
    }

    @Override
    public PageService<SelectorQueryCondition, SelectorVO> pageService() {
        return selectorService;
    }
}
