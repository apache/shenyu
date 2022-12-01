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

import org.apache.shenyu.admin.model.dto.FieldDTO;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.FieldQuery;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.service.FieldService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.util.List;

@Validated
@RestController
@RequestMapping("/field")
public class FieldController {
    private final FieldService fieldService;

    public FieldController(final FieldService fieldService) {
        this.fieldService = fieldService;
    }

    /**
     * create or update mockRequestRecord.
     *
     * @param fieldDTO fieldDTO.
     * @return {@linkplain ShenyuAdminResult}
     */
    @PostMapping("/insertOrUpdate")
    public ShenyuAdminResult createOrUpdate(@Valid @RequestBody final FieldDTO fieldDTO) {
        return ShenyuAdminResult.success(ShenyuResultMessage.SUCCESS, fieldService.createOrUpdate(fieldDTO));
    }

    /**
     * batch delete.
     *
     * @param ids ids
     * @return {@linkplain ShenyuAdminResult}
     */
    @DeleteMapping("/batchDelete")
    public ShenyuAdminResult batchDelete(@RequestBody @NotEmpty final List<@NotBlank String> ids) {
        Integer deleteCount = fieldService.deleteBatch(ids);
        return ShenyuAdminResult.success(ShenyuResultMessage.DELETE_SUCCESS, deleteCount);
    }

    /**
     * findPageByQuery.
     *
     * @param name        name
     * @param fieldDesc   fieldDesc
     * @param currentPage currentPage
     * @param pageSize    pageSize
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/findPageByQuery")
    public ShenyuAdminResult listByPage(final String name, final String fieldDesc,
                                        @RequestParam @NotNull(message = "currentPage not null") final Integer currentPage,
                                        @RequestParam @NotNull(message = "pageSize not null") final Integer pageSize) {
        PageParameter pageParameter = new PageParameter(currentPage, pageSize);
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, this.fieldService.listByPage(new FieldQuery(name, fieldDesc, pageParameter)));
    }

}
