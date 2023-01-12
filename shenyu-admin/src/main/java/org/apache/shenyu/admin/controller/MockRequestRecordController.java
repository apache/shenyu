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

import org.apache.shenyu.admin.mapper.MockRequestRecordMapper;
import org.apache.shenyu.admin.model.dto.MockRequestRecordDTO;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.MockRequestRecordQuery;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.service.MockRequestRecordService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.admin.validation.annotation.Existed;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
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

/**
 * AlertTemplate MockRequestRecordController.
 */
@Validated
@RestController
@RequestMapping("/mock")
public class MockRequestRecordController {

    private final MockRequestRecordService mockRequestRecordService;

    public MockRequestRecordController(final MockRequestRecordService mockRequestRecordService) {
        this.mockRequestRecordService = mockRequestRecordService;
    }

    /**
     * create or update mockRequestRecord.
     *
     * @param mockRequestRecordDTO mockRequestRecordDTO.
     * @return {@linkplain ShenyuAdminResult}
     */
    @PostMapping("/insertOrUpdate")
    public ShenyuAdminResult createOrUpdate(@Valid @RequestBody final MockRequestRecordDTO mockRequestRecordDTO) {
        return ShenyuAdminResult.success(ShenyuResultMessage.SUCCESS, mockRequestRecordService.createOrUpdate(mockRequestRecordDTO));
    }

    /**
     * batch delete.
     * @param ids ids
     * @return {@linkplain ShenyuAdminResult}
     */
    @DeleteMapping("/batchDelete")
    public ShenyuAdminResult batchDelete(@RequestBody @NotEmpty final List<@NotBlank String> ids) {
        Integer deleteCount = mockRequestRecordService.batchDelete(ids);
        return ShenyuAdminResult.success(ShenyuResultMessage.DELETE_SUCCESS, deleteCount);
    }

    /**
     * delete.
     * @param id id
     * @return {@linkplain ShenyuAdminResult}
     */
    @DeleteMapping("/{id}")
    public ShenyuAdminResult delete(@PathVariable @Valid @Existed(provider = MockRequestRecordMapper.class,
            message = " is not existed") final String id) {
        return ShenyuAdminResult.success(ShenyuResultMessage.DELETE_SUCCESS, mockRequestRecordService.delete(id));
    }

    /**
     * findPageByQuery.
     * @param apiId apiId
     * @param host host
     * @param url url
     * @param pathVariable pathVariable
     * @param header header
     * @param currentPage currentPage
     * @param pageSize pageSize
     * @return ShenyuAdminResult
     */
    @GetMapping("/findPageByQuery")
    public ShenyuAdminResult listByPage(final String apiId, final String host, final String url,
                                        final String pathVariable, final String header,
                                        @RequestParam @NotNull(message = "currentPage not null") final Integer currentPage,
                                        @RequestParam @NotNull(message = "pageSize not null") final Integer pageSize) {
        PageParameter pageParameter = new PageParameter(currentPage, pageSize);
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, this.mockRequestRecordService.listByPage(new MockRequestRecordQuery(apiId, host, url,
                pathVariable, header, pageParameter)));
    }

    /**
     * get Mock Request.
     * @param apiId apiId
     * @return ShenyuAdminResult
     */
    @GetMapping("/{apiId}")
    public ShenyuAdminResult get(@PathVariable final String apiId) {
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, this.mockRequestRecordService.queryByApiId(apiId));
    }
}
