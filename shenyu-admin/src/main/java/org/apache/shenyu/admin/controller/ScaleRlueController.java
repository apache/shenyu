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
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import org.apache.shenyu.admin.aspect.annotation.RestApi;
import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.mapper.ScaleRuleMapper;
import org.apache.shenyu.admin.model.dto.ScaleRuleDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.ScaleRuleQuery;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.ScaleRuleVO;
import org.apache.shenyu.admin.service.ScaleRuleService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.admin.validation.annotation.Existed;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;
import java.util.Optional;

/**
 * this is scale rule controller.
 */
@RestApi("/scale/rule")
public class ScaleRlueController {

    private final ScaleRuleService scaleRuleService;

    private final ScaleRuleMapper scaleRuleMapper;

    public ScaleRlueController(final ScaleRuleService scaleRuleService, final ScaleRuleMapper scaleRuleMapper) {
        this.scaleRuleService = scaleRuleService;
        this.scaleRuleMapper = scaleRuleMapper;
    }

    /**
     * get all rules.
     *
     * @return ShenyuAdminResult
     */
    @GetMapping("/getAllRules")
    public ShenyuAdminResult selectAll() {
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, scaleRuleService.selectAll());
    }

    /**
     * query scale rule.
     *
     * @param metricName  metricName
     * @param type  type
     * @param status  status
     * @param currentPage currentPage
     * @param pageSize    pageSize
     * @return org.apache.shenyu.admin.model.result.ShenyuAdminResult
     */
    @GetMapping
    public ShenyuAdminResult queryRule(final String metricName,
                                       final Integer type,
                                       final Integer status,
                                       @RequestParam @NotNull final Integer currentPage,
                                       @RequestParam @NotNull final Integer pageSize) {
        CommonPager<ScaleRuleVO> commonPager =
                scaleRuleService.listByPage(new ScaleRuleQuery(metricName, type, status, new PageParameter(currentPage, pageSize)));
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, commonPager);
    }

    /**
     * detail scale rule.
     *
     * @param id id
     * @return ShenyuAdminResult
     */
    @GetMapping("/{id}")
    public ShenyuAdminResult detailRule(@PathVariable("id")
                                        @Valid
                                        @Existed(provider = ScaleRuleMapper.class,
                                                message = "scale role is not existed") final String id) {
        ScaleRuleVO scaleRuleVO = scaleRuleService.findById(id);
        return Optional.ofNullable(scaleRuleVO)
                .map(item -> ShenyuAdminResult.success(ShenyuResultMessage.DETAIL_SUCCESS, item))
                .orElse(ShenyuAdminResult.error(ShenyuResultMessage.DETAIL_FAILED));
    }

    /**
     * create scale rule.
     *
     * @param scaleRuleDTO scaleRuleDTO
     * @return ShenyuAdminResult
     */
    @PostMapping
    public ShenyuAdminResult createRule(@Valid @RequestBody final ScaleRuleDTO scaleRuleDTO) {
        return ShenyuAdminResult.success(ShenyuResultMessage.CREATE_SUCCESS, scaleRuleService.createOrUpdate(scaleRuleDTO));
    }

    /**
     * update rule.
     *
     * @param scaleRuleDTO scaleRuleDTO
     * @return ShenyuAdminResult
     */
    @PutMapping
    public ShenyuAdminResult updateRule(@Valid @RequestBody final ScaleRuleDTO scaleRuleDTO) {
        if (!scaleRuleMapper.existed(scaleRuleDTO.getId())) {
            throw new ShenyuAdminException("scale rule is not existed");
        }
        return ShenyuAdminResult.success(ShenyuResultMessage.UPDATE_SUCCESS, scaleRuleService.createOrUpdate(scaleRuleDTO));
    }

    /**
     * delete rules.
     *
     * @param ids ids
     * @return ShenyuAdminResult
     */
    @DeleteMapping("/batch")
    public ShenyuAdminResult deleteRules(@RequestBody @NotEmpty final List<@NotBlank String> ids) {
        return ShenyuAdminResult.success(ShenyuResultMessage.DELETE_SUCCESS, scaleRuleService.delete(ids));
    }
}
