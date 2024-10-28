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
import org.apache.shenyu.admin.mapper.NamespaceMapper;
import org.apache.shenyu.admin.mapper.RuleMapper;
import org.apache.shenyu.admin.model.dto.BatchCommonDTO;
import org.apache.shenyu.admin.model.dto.BatchNamespaceCommonDTO;
import org.apache.shenyu.admin.model.dto.RuleDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageCondition;
import org.apache.shenyu.admin.model.query.RuleQueryCondition;
import org.apache.shenyu.admin.model.result.AdminResult;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.RuleVO;
import org.apache.shenyu.admin.service.PageService;
import org.apache.shenyu.admin.service.RuleService;
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
 * this is rule controller.
 */
@RestApi("/rule")
public class RuleController implements PagedController<RuleQueryCondition, RuleVO> {

    private final RuleService ruleService;

    public RuleController(final RuleService ruleService) {
        this.ruleService = ruleService;
    }

    /**
     * query rules.
     *
     * @param selectorId  selector id.
     * @param name        rule name.
     * @param currentPage current page.
     * @param pageSize    page size.
     * @param namespaceId namespaceId.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("")
    public AdminResult<CommonPager<RuleVO>> queryRules(final String selectorId, final String name,
                                                       @RequestParam @NotNull final Integer currentPage,
                                                       @RequestParam @NotNull final Integer pageSize,
                                                       @Valid @Existed(message = "namespaceId is not existed",
                                                               provider = NamespaceMapper.class) final String namespaceId) {
        final RuleQueryCondition condition = new RuleQueryCondition();
        condition.setUserId(SessionUtil.visitor().getUserId());
        condition.setSelectors(ListUtil.of(selectorId));
        condition.setKeyword(name);
        condition.setNamespaceId(namespaceId);
        return searchAdaptor(new PageCondition<>(currentPage, pageSize, condition));
    }

    /**
     * detail rule.
     *
     * @param id rule id.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/{id}")
    public ShenyuAdminResult detailRule(@Valid @PathVariable("id")
                                        @Existed(provider = RuleMapper.class, message = "rule is not existed") final String id) {
        RuleVO ruleVO = ruleService.findById(id);
        return ShenyuAdminResult.success(ShenyuResultMessage.DETAIL_SUCCESS, ruleVO);
    }

    /**
     * create rule.
     *
     * @param ruleDTO rule.
     * @return {@linkplain ShenyuAdminResult}
     */
    @PostMapping("")
    public ShenyuAdminResult createRule(@Valid @RequestBody final RuleDTO ruleDTO) {
        Integer createCount = ruleService.createOrUpdate(ruleDTO);
        return ShenyuAdminResult.success(ShenyuResultMessage.CREATE_SUCCESS, createCount);
    }

    /**
     * update rule.
     *
     * @param id      primary key.
     * @param ruleDTO rule.
     * @return {@linkplain ShenyuAdminResult}
     */
    @PutMapping("/{id}")
    public ShenyuAdminResult updateRule(@PathVariable("id") @Valid
                                        @Existed(provider = RuleMapper.class,
                                                message = "rule is not existed") final String id,
                                        @Valid @RequestBody final RuleDTO ruleDTO) {
        ruleDTO.setId(id);
        Integer updateCount = ruleService.createOrUpdate(ruleDTO);
        return ShenyuAdminResult.success(ShenyuResultMessage.UPDATE_SUCCESS, updateCount);
    }

    /**
     * Batch enabled rule.
     *
     * @param batchCommonDTO the batch common dto
     * @return the shenyu result
     */
    @PostMapping("/batchEnabled")
    public ShenyuAdminResult batchEnabled(@Valid @RequestBody final BatchCommonDTO batchCommonDTO) {
        if (!ruleService.enabledByIdsAndNamespaceId(batchCommonDTO.getIds(), batchCommonDTO.getEnabled(), batchCommonDTO.getNamespaceId())) {
            return ShenyuAdminResult.error(ShenyuResultMessage.NOT_FOUND_EXCEPTION);
        }
        return ShenyuAdminResult.success(ShenyuResultMessage.ENABLE_SUCCESS);
    }

    /**
     * delete rules.
     *
     * @param batchNamespaceCommonDTO batchNamespaceCommonDTO.
     * @return {@linkplain ShenyuAdminResult}
     */
    @DeleteMapping("/batch")
    public ShenyuAdminResult deleteRules(@Valid @RequestBody final BatchNamespaceCommonDTO batchNamespaceCommonDTO) {
        Integer deleteCount = ruleService.deleteByIdsAndNamespaceId(batchNamespaceCommonDTO.getIds(), batchNamespaceCommonDTO.getNamespaceId());
        return ShenyuAdminResult.success(ShenyuResultMessage.DELETE_SUCCESS, deleteCount);
    }

    @Override
    public PageService<RuleQueryCondition, RuleVO> pageService() {
        return ruleService;
    }
}
