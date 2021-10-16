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

import org.apache.shenyu.admin.model.dto.BatchCommonDTO;
import org.apache.shenyu.admin.model.dto.ShenyuDictDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.ShenyuDictQuery;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.ShenyuDictVO;
import org.apache.shenyu.admin.service.ShenyuDictService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import java.util.List;
import java.util.Objects;

/**
 * this is a shenyu dict controller.
 */
@Validated
@RestController
@RequestMapping("/shenyu-dict")
public class ShenyuDictController {

    private final ShenyuDictService shenyuDictService;

    public ShenyuDictController(final ShenyuDictService shenyuDictService) {
        this.shenyuDictService = shenyuDictService;
    }

    /**
     * query shenyu dicts.
     *
     * @param type        dict type.
     * @param dictCode    dict code.
     * @param dictName    dict name.
     * @param currentPage current page.
     * @param pageSize    page size
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("")
    public ShenyuAdminResult queryDicts(final String type, final String dictCode, final String dictName, final Integer currentPage, final Integer pageSize) {
        CommonPager<ShenyuDictVO> commonPager = shenyuDictService.listByPage(new ShenyuDictQuery(type, dictCode, dictName, new PageParameter(currentPage, pageSize)));
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, commonPager);
    }

    /**
     * query shenyu dicts by dict type.
     *
     * @param type dict type.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/all/{type}")
    public ShenyuAdminResult findByType(@PathVariable("type") final String type) {
        List<ShenyuDictVO> shenyuDictVOS = shenyuDictService.list(type);
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, shenyuDictVOS);
    }

    /**
     * detail dict.
     *
     * @param id dict id.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/{id}")
    public ShenyuAdminResult detail(@PathVariable("id") final String id) {
        ShenyuDictVO shenyuDictVO = shenyuDictService.findById(id);
        return ShenyuAdminResult.success(ShenyuResultMessage.DETAIL_SUCCESS, shenyuDictVO);
    }

    /**
     * create shenyu dict.
     *
     * @param shenyuDictDTO {@link ShenyuDictDTO}
     * @return {@link ShenyuAdminResult}
     */
    @PostMapping("")
    public ShenyuAdminResult createShenyuDict(@Valid @RequestBody final ShenyuDictDTO shenyuDictDTO) {
        Integer createCount = shenyuDictService.createOrUpdate(shenyuDictDTO);
        return ShenyuAdminResult.success(ShenyuResultMessage.CREATE_SUCCESS, createCount);
    }

    /**
     * update shenyu dict by id.
     *
     * @param id shenyu dict id
     * @param shenyuDictDTO {@linkplain ShenyuDictDTO}
     * @return {@linkplain ShenyuAdminResult}
     */
    @PutMapping("/{id}")
    public ShenyuAdminResult updateShenyuDict(@PathVariable("id") final String id, @Valid @RequestBody final ShenyuDictDTO shenyuDictDTO) {
        Objects.requireNonNull(shenyuDictDTO);
        shenyuDictDTO.setId(id);
        Integer updateCount = shenyuDictService.createOrUpdate(shenyuDictDTO);
        return ShenyuAdminResult.success(ShenyuResultMessage.UPDATE_SUCCESS, updateCount);
    }

    /**
     * batch delete some shenyu dicts by some id list.
     *
     * @param ids shenyu dict id list.
     * @return {@linkplain ShenyuAdminResult}
     */
    @DeleteMapping("/batch")
    public ShenyuAdminResult deleteShenyuDicts(@RequestBody @NotEmpty final List<@NotBlank String> ids) {
        Integer deleteCount = shenyuDictService.deleteShenyuDicts(ids);
        return ShenyuAdminResult.success(ShenyuResultMessage.DELETE_SUCCESS, deleteCount);
    }

    /**
     * Batch enabled shenyu dict result.
     *
     * @param batchCommonDTO the batch common dto
     * @return the shenyu result
     */
    @PostMapping("/batchEnabled")
    public ShenyuAdminResult batchEnabled(@Valid @RequestBody final BatchCommonDTO batchCommonDTO) {
        final Integer result = shenyuDictService.enabled(batchCommonDTO.getIds(), batchCommonDTO.getEnabled());
        return ShenyuAdminResult.success("batch enable success", result);
    }
}
