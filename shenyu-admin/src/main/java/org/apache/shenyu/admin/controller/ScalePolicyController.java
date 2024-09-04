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
import org.apache.shenyu.admin.aspect.annotation.RestApi;
import org.apache.shenyu.admin.mapper.ScalePolicyMapper;
import org.apache.shenyu.admin.model.dto.ScalePolicyDTO;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.ScalePolicyVO;
import org.apache.shenyu.admin.service.ScalePolicyService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.admin.validation.annotation.Existed;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;

import java.util.Optional;

/**
 * this is ScalePolicy controller.
 */
@RestApi("/scale/policy")
public class ScalePolicyController {

    private final ScalePolicyService scalePolicyService;

    public ScalePolicyController(final ScalePolicyService scalePolicyService) {
        this.scalePolicyService = scalePolicyService;
    }

    /**
     * get all policies.
     *
     * @return ShenyuAdminResult
     */
    @GetMapping("/getAllPolicies")
    public ShenyuAdminResult selectAll() {
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, scalePolicyService.selectAll());
    }

    /**
     * detail scale policy.
     *
     * @param id primary key
     * @return ShenyuAdminResult
     */
    @GetMapping("/{id}")
    public ShenyuAdminResult detailPolicy(@PathVariable("id")
                                          @Valid
                                          @Existed(provider = ScalePolicyMapper.class,
                                                  message = "scale policy is not existed") final String id) {
        ScalePolicyVO scalePolicyVO = scalePolicyService.findById(id);
        return Optional.ofNullable(scalePolicyVO)
                .map(item -> ShenyuAdminResult.success(ShenyuResultMessage.DETAIL_SUCCESS, item))
                .orElse(ShenyuAdminResult.error(ShenyuResultMessage.DETAIL_FAILED));
    }

    /**
     * update scale policy.
     *
     * @param scalePolicyDTO scale policy info
     * @return ShenyuAdminResult
     */
    @PutMapping
    public ShenyuAdminResult updateScalePolicy(@Valid @RequestBody final ScalePolicyDTO scalePolicyDTO) {
        return ShenyuAdminResult.success(ShenyuResultMessage.UPDATE_SUCCESS, scalePolicyService.update(scalePolicyDTO));
    }
}
