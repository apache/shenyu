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

import org.dromara.soul.admin.model.dto.BatchCommonDTO;
import org.dromara.soul.admin.model.dto.SoulDictDTO;
import org.dromara.soul.admin.model.page.CommonPager;
import org.dromara.soul.admin.model.page.PageParameter;
import org.dromara.soul.admin.model.query.SoulDictQuery;
import org.dromara.soul.admin.model.result.SoulAdminResult;
import org.dromara.soul.admin.service.SoulDictService;
import org.dromara.soul.admin.utils.SoulResultMessage;
import org.dromara.soul.admin.model.vo.SoulDictVO;
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
 * this is a soul dict controller.
 *
 * @author dengliming
 */
@RestController
@RequestMapping("/soul-dict")
public class SoulDictController {
    private final SoulDictService soulDictService;

    @Autowired(required = false)
    public SoulDictController(final SoulDictService soulDictService) {
        this.soulDictService = soulDictService;
    }

    /**
     * query soul dicts.
     *
     * @param type        dict type.
     * @param dictCode    dict code.
     * @param dictName    dict name.
     * @param currentPage current page.
     * @param pageSize    page size
     * @return {@linkplain SoulAdminResult}
     */
    @GetMapping("")
    public SoulAdminResult queryDicts(final String type, final String dictCode, final String dictName, final Integer currentPage, final Integer pageSize) {
        CommonPager<SoulDictVO> commonPager = soulDictService.listByPage(new SoulDictQuery(type, dictCode, dictName, new PageParameter(currentPage, pageSize)));
        return SoulAdminResult.success(SoulResultMessage.QUERY_SUCCESS, commonPager);
    }

    /**
     * query soul dicts by dict type.
     *
     * @param type dict type.
     * @return {@linkplain SoulAdminResult}
     */
    @GetMapping("/all/{type}")
    public SoulAdminResult findByType(@PathVariable("type") final String type) {
        List<SoulDictVO> soulDictVOS = soulDictService.list(type);
        return SoulAdminResult.success(SoulResultMessage.QUERY_SUCCESS, soulDictVOS);
    }

    /**
     * detail dict.
     *
     * @param id dict id.
     * @return {@linkplain SoulAdminResult}
     */
    @GetMapping("/{id}")
    public SoulAdminResult detail(@PathVariable("id") final String id) {
        SoulDictVO soulDictVO = soulDictService.findById(id);
        return SoulAdminResult.success(SoulResultMessage.DETAIL_SUCCESS, soulDictVO);
    }

    /**
     * create soul dict.
     *
     * @param soulDictDTO {@link SoulDictDTO}
     * @return {@link SoulAdminResult}
     */
    @PostMapping("")
    public SoulAdminResult createSoulDict(@RequestBody final SoulDictDTO soulDictDTO) {
        Integer createCount = soulDictService.createOrUpdate(soulDictDTO);
        return SoulAdminResult.success(SoulResultMessage.CREATE_SUCCESS, createCount);
    }

    /**
     * update soul dict by id.
     *
     * @param id          soul dict id
     * @param soulDictDTO {@linkplain SoulDictDTO}
     * @return {@linkplain SoulAdminResult}
     */
    @PutMapping("/{id}")
    public SoulAdminResult updateSoulDict(@PathVariable("id") final String id, @RequestBody final SoulDictDTO soulDictDTO) {
        Objects.requireNonNull(soulDictDTO);
        soulDictDTO.setId(id);
        Integer updateCount = soulDictService.createOrUpdate(soulDictDTO);
        return SoulAdminResult.success(SoulResultMessage.UPDATE_SUCCESS, updateCount);
    }

    /**
     * batch delete some soul dicts by some id list.
     *
     * @param ids soul dict id list.
     * @return {@linkplain SoulAdminResult}
     */
    @DeleteMapping("/batch")
    public SoulAdminResult deleteSoulDicts(@RequestBody final List<String> ids) {
        Integer deleteCount = soulDictService.deleteSoulDicts(ids);
        return SoulAdminResult.success(SoulResultMessage.DELETE_SUCCESS, deleteCount);
    }

    /**
     * Batch enabled soul dict result.
     *
     * @param batchCommonDTO the batch common dto
     * @return the soul result
     */
    @PostMapping("/batchEnabled")
    public SoulAdminResult batchEnabled(@RequestBody final BatchCommonDTO batchCommonDTO) {
        final Integer result = soulDictService.enabled(batchCommonDTO.getIds(), batchCommonDTO.getEnabled());
        return SoulAdminResult.success("batch enable success", result);
    }
}
