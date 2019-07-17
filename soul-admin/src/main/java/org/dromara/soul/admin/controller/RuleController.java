/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.admin.controller;

import org.dromara.soul.admin.dto.RuleDTO;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.page.PageParameter;
import org.dromara.soul.admin.query.RuleQuery;
import org.dromara.soul.admin.service.RuleService;
import org.dromara.soul.admin.vo.RuleVO;
import org.dromara.soul.common.result.SoulResult;
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
     * @return {@linkplain SoulResult}
     */
    @GetMapping("")
    public SoulResult queryRules(final String selectorId, final Integer currentPage, final Integer pageSize) {
        try {
            CommonPager<RuleVO> commonPager = ruleService.listByPage(new RuleQuery(selectorId, new PageParameter(currentPage, pageSize)));
            return SoulResult.success("query rules success", commonPager);
        } catch (Exception e) {
            return SoulResult.error("query rules exception");
        }
    }

    /**
     * detail rule.
     *
     * @param id rule id.
     * @return {@linkplain SoulResult}
     */
    @GetMapping("/{id}")
    public SoulResult detailRule(@PathVariable("id") final String id) {
        try {
            RuleVO ruleVO = ruleService.findById(id);
            return SoulResult.success("detail rule success", ruleVO);
        } catch (Exception e) {
            return SoulResult.error("detail rule exception");
        }
    }

    /**
     * create rule.
     *
     * @param ruleDTO rule.
     * @return {@linkplain SoulResult}
     */
    @PostMapping("")
    public SoulResult createRule(@RequestBody final RuleDTO ruleDTO) {
        try {
            Integer createCount = ruleService.createOrUpdate(ruleDTO);
            return SoulResult.success("create rule success", createCount);
        } catch (Exception e) {
            return SoulResult.error("create rule exception");
        }
    }

    /**
     * update rule.
     *
     * @param id      primary key.
     * @param ruleDTO rule.
     * @return {@linkplain SoulResult}
     */
    @PutMapping("/{id}")
    public SoulResult updateRule(@PathVariable("id") final String id, @RequestBody final RuleDTO ruleDTO) {
        try {
            Objects.requireNonNull(ruleDTO);
            ruleDTO.setId(id);
            Integer updateCount = ruleService.createOrUpdate(ruleDTO);
            return SoulResult.success("update rule success", updateCount);
        } catch (Exception e) {
            return SoulResult.error("update rule exception");
        }
    }

    /**
     * delete rules.
     *
     * @param ids primary key.
     * @return {@linkplain SoulResult}
     */
    @DeleteMapping("/batch")
    public SoulResult deleteRules(@RequestBody final List<String> ids) {
        try {
            Integer deleteCount = ruleService.delete(ids);
            return SoulResult.success("delete rule success", deleteCount);
        } catch (Exception e) {
            return SoulResult.error("delete rule exception");
        }
    }
}
