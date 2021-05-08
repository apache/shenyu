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

import org.apache.shenyu.admin.service.SoulDictService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.admin.model.dto.BatchCommonDTO;
import org.apache.shenyu.admin.model.dto.SoulDictDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.SoulDictQuery;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.SoulDictVO;
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
 * this is a shenyu dict controller.
 */
@RestController
@RequestMapping("/soul-dict")
public class ShenyuDictController {
    private final SoulDictService soulDictService;

    @Autowired(required = false)
    public ShenyuDictController(final SoulDictService soulDictService) {
        this.soulDictService = soulDictService;
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
        CommonPager<SoulDictVO> commonPager = soulDictService.listByPage(new SoulDictQuery(type, dictCode, dictName, new PageParameter(currentPage, pageSize)));
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
        List<SoulDictVO> soulDictVOS = soulDictService.list(type);
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, soulDictVOS);
    }

    /**
     * detail dict.
     *
     * @param id dict id.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/{id}")
    public ShenyuAdminResult detail(@PathVariable("id") final String id) {
        SoulDictVO soulDictVO = soulDictService.findById(id);
        return ShenyuAdminResult.success(ShenyuResultMessage.DETAIL_SUCCESS, soulDictVO);
    }

    /**
     * create shenyu dict.
     *
     * @param soulDictDTO {@link SoulDictDTO}
     * @return {@link ShenyuAdminResult}
     */
    @PostMapping("")
    public ShenyuAdminResult createSoulDict(@RequestBody final SoulDictDTO soulDictDTO) {
        Integer createCount = soulDictService.createOrUpdate(soulDictDTO);
        return ShenyuAdminResult.success(ShenyuResultMessage.CREATE_SUCCESS, createCount);
    }

    /**
     * update shenyu dict by id.
     *
     * @param id          shenyu dict id
     * @param soulDictDTO {@linkplain SoulDictDTO}
     * @return {@linkplain ShenyuAdminResult}
     */
    @PutMapping("/{id}")
    public ShenyuAdminResult updateSoulDict(@PathVariable("id") final String id, @RequestBody final SoulDictDTO soulDictDTO) {
        Objects.requireNonNull(soulDictDTO);
        soulDictDTO.setId(id);
        Integer updateCount = soulDictService.createOrUpdate(soulDictDTO);
        return ShenyuAdminResult.success(ShenyuResultMessage.UPDATE_SUCCESS, updateCount);
    }

    /**
     * batch delete some shenyu dicts by some id list.
     *
     * @param ids shenyu dict id list.
     * @return {@linkplain ShenyuAdminResult}
     */
    @DeleteMapping("/batch")
    public ShenyuAdminResult deleteSoulDicts(@RequestBody final List<String> ids) {
        Integer deleteCount = soulDictService.deleteSoulDicts(ids);
        return ShenyuAdminResult.success(ShenyuResultMessage.DELETE_SUCCESS, deleteCount);
    }

    /**
     * Batch enabled shenyu dict result.
     *
     * @param batchCommonDTO the batch common dto
     * @return the shenyu result
     */
    @PostMapping("/batchEnabled")
    public ShenyuAdminResult batchEnabled(@RequestBody final BatchCommonDTO batchCommonDTO) {
        final Integer result = soulDictService.enabled(batchCommonDTO.getIds(), batchCommonDTO.getEnabled());
        return ShenyuAdminResult.success("batch enable success", result);
    }
}
