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

import org.apache.shenyu.admin.model.dto.AlertTemplateDTO;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.AlertTemplateVO;
import org.apache.shenyu.admin.model.entity.AlertTemplateDO;
import org.apache.shenyu.admin.service.AlertTemplateService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import java.util.List;

/**
 * AlertTemplate Controller.
 */
@Validated
@RestController
@RequestMapping("/alertTemplate")
public class AlertTemplateController {

    @Autowired
    private AlertTemplateService alertTemplateService;

    /**
     * Add alert template.
     * @param alertTemplateDTO alertTemplateDTO
     * @return row int
     */
    @PostMapping("addTemplate")
    public ShenyuAdminResult addTemplate(@Valid @RequestBody final AlertTemplateDTO alertTemplateDTO) {
        int row = alertTemplateService.addTemplate(alertTemplateDTO);
        return ShenyuAdminResult.success(ShenyuResultMessage.CREATE_SUCCESS, row);
    }

    /**
     * Delete alert template.
     * @param ids ids
     * @return row int
     */
    @PostMapping("deleteTemplate")
    public ShenyuAdminResult deleteTemplate(@RequestBody @NotEmpty final List<@NotBlank String> ids) {
        int row = alertTemplateService.deleteTemplate(ids);
        return ShenyuAdminResult.success(ShenyuResultMessage.DELETE_SUCCESS, row);
    }

    /**
     * Update alert template.
     * @param alertTemplateDTO alertTemplateDTO
     * @return row int
     */
    @PostMapping("updateTemplate")
    public ShenyuAdminResult updateTemplate(@Valid @RequestBody final AlertTemplateDTO alertTemplateDTO) {
        int row = alertTemplateService.updateTemplate(alertTemplateDTO);
        return ShenyuAdminResult.success(ShenyuResultMessage.UPDATE_SUCCESS, row);
    }

    /**
     * Get all template.
     * @return all {@link AlertTemplateVO}
     */
    @GetMapping("getAll")
    public ShenyuAdminResult getAll() {
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, alertTemplateService.getAll());
    }

    /**
     * Template detail.
     * @param id id
     * @return {@link AlertTemplateDO}
     */
    @GetMapping("detail/{id}")
    public ShenyuAdminResult detail(@PathVariable("id") final Long id) {
        return ShenyuAdminResult.success(ShenyuResultMessage.DETAIL_SUCCESS, alertTemplateService.detail(id));
    }
}
