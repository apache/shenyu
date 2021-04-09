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

package org.dromara.soul.admin.controller;

import org.dromara.soul.admin.model.dto.RuleDTO;
import org.dromara.soul.admin.model.page.CommonPager;
import org.dromara.soul.admin.model.page.PageParameter;
import org.dromara.soul.admin.model.query.RuleQuery;
import org.dromara.soul.admin.model.result.SoulAdminResult;
import org.dromara.soul.admin.service.RuleService;
import org.dromara.soul.admin.utils.SoulResultMessage;
import org.dromara.soul.admin.model.vo.RuleVO;
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
 * this is rule controller.
 *
 * @author jiangxiaofeng(Nicholas)
 */
@RestController
@RequestMapping("/rule")
public class RuleController {

    private final RuleService ruleService;

    @Autowired(required = false)
    public RuleController(final RuleService ruleService) {
        this.ruleService = ruleService;
    }

    /**
     * query rules.
     *
     * @param selectorId  selector id.
     * @param currentPage current page.
     * @param pageSize    page size.
     * @return {@linkplain SoulAdminResult}
     */
    @GetMapping("")
    public SoulAdminResult queryRules(final String selectorId, final Integer currentPage, final Integer pageSize) {
        CommonPager<RuleVO> commonPager = ruleService.listByPage(new RuleQuery(selectorId, new PageParameter(currentPage, pageSize)));
        return SoulAdminResult.success(SoulResultMessage.QUERY_SUCCESS, commonPager);
    }

    /**
     * detail rule.
     *
     * @param id rule id.
     * @return {@linkplain SoulAdminResult}
     */
    @GetMapping("/{id}")
    public SoulAdminResult detailRule(@PathVariable("id") final String id) {
        RuleVO ruleVO = ruleService.findById(id);
        return SoulAdminResult.success(SoulResultMessage.DETAIL_SUCCESS, ruleVO);
    }

    /**
     * create rule.
     *
     * @param ruleDTO rule.
     * @return {@linkplain SoulAdminResult}
     */
    @PostMapping("")
    public SoulAdminResult createRule(@RequestBody final RuleDTO ruleDTO) {
        Integer createCount = ruleService.createOrUpdate(ruleDTO);
        return SoulAdminResult.success(SoulResultMessage.CREATE_SUCCESS, createCount);
    }

    /**
     * update rule.
     *
     * @param id      primary key.
     * @param ruleDTO rule.
     * @return {@linkplain SoulAdminResult}
     */
    @PutMapping("/{id}")
    public SoulAdminResult updateRule(@PathVariable("id") final String id, @RequestBody final RuleDTO ruleDTO) {
        Objects.requireNonNull(ruleDTO);
        ruleDTO.setId(id);
        Integer updateCount = ruleService.createOrUpdate(ruleDTO);
        return SoulAdminResult.success(SoulResultMessage.UPDATE_SUCCESS, updateCount);
    }

    /**
     * delete rules.
     *
     * @param ids primary key.
     * @return {@linkplain SoulAdminResult}
     */
    @DeleteMapping("/batch")
    public SoulAdminResult deleteRules(@RequestBody final List<String> ids) {
        Integer deleteCount = ruleService.delete(ids);
        return SoulAdminResult.success(SoulResultMessage.DELETE_SUCCESS, deleteCount);
    }
}
