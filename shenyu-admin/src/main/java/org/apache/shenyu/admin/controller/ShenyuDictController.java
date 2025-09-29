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

import org.apache.shenyu.admin.aspect.annotation.RestApi;
import org.apache.shenyu.admin.mapper.ShenyuDictMapper;
import org.apache.shenyu.admin.model.dto.BatchCommonDTO;
import org.apache.shenyu.admin.model.dto.ShenyuDictDTO;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.ShenyuDictQuery;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.service.ShenyuDictService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.admin.validation.annotation.Existed;
import org.apache.shiro.authz.annotation.Logical;
import org.apache.shiro.authz.annotation.RequiresPermissions;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import java.util.List;

/**
 * this is a shenyu dict controller.
 */
@RestApi("/shenyu-dict")
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
    @GetMapping
    @RequiresPermissions("system:dict:list")
    public ShenyuAdminResult queryDicts(final String type, final String dictCode, final String dictName,
                                        @RequestParam @NotNull final Integer currentPage,
                                        @RequestParam @NotNull final Integer pageSize) {
        final ShenyuDictQuery query = new ShenyuDictQuery(type, dictCode, dictName, new PageParameter(currentPage, pageSize));
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, shenyuDictService.listByPage(query));
    }
    
    /**
     * query shenyu dicts by dict type.
     *
     * @param type dict type.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/all/{type}")
    public ShenyuAdminResult findByType(@PathVariable("type") final String type) {
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, shenyuDictService.list(type));
    }
    
    /**
     * detail dict.
     *
     * @param id dict id.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/{id}")
    @RequiresPermissions("system:dict:edit")
    public ShenyuAdminResult detail(@PathVariable("id") @Valid
                                    @Existed(provider = ShenyuDictMapper.class,
                                            message = "dict is not existed") final String id) {
        return ShenyuAdminResult.success(ShenyuResultMessage.DETAIL_SUCCESS, shenyuDictService.findById(id));
    }
    
    /**
     * create shenyu dict.
     *
     * @param shenyuDictDTO {@link ShenyuDictDTO}
     * @return {@link ShenyuAdminResult}
     */
    @PostMapping
    @RequiresPermissions(value = {"system:dict:add", "system:dict:edit"}, logical = Logical.OR)
    public ShenyuAdminResult createShenyuDict(@Valid @RequestBody final ShenyuDictDTO shenyuDictDTO) {
        return ShenyuAdminResult.success(ShenyuResultMessage.CREATE_SUCCESS, shenyuDictService.createOrUpdate(shenyuDictDTO));
    }
    
    /**
     * update shenyu dict by id.
     *
     * @param id            shenyu dict id
     * @param shenyuDictDTO {@linkplain ShenyuDictDTO}
     * @return {@linkplain ShenyuAdminResult}
     */
    @PutMapping("/{id}")
    @RequiresPermissions("system:dict:edit")
    public ShenyuAdminResult updateShenyuDict(@PathVariable("id") @Valid
                                              @Existed(provider = ShenyuDictMapper.class,
                                                      message = "dict is not existed") final String id,
                                              @Valid @NotNull @RequestBody final ShenyuDictDTO shenyuDictDTO) {
        shenyuDictDTO.setId(id);
        return ShenyuAdminResult.success(ShenyuResultMessage.UPDATE_SUCCESS, shenyuDictService.createOrUpdate(shenyuDictDTO));
    }
    
    /**
     * batch delete some shenyu dicts by some id list.
     *
     * @param ids shenyu dict id list.
     * @return {@linkplain ShenyuAdminResult}
     */
    @DeleteMapping("/batch")
    @RequiresPermissions("system:dict:delete")
    public ShenyuAdminResult deleteShenyuDicts(@RequestBody @NotEmpty final List<@NotBlank String> ids) {
        return ShenyuAdminResult.success(ShenyuResultMessage.DELETE_SUCCESS, shenyuDictService.deleteShenyuDicts(ids));
    }
    
    /**
     * Batch enabled shenyu dict result.
     *
     * @param batchCommonDTO the batch common dto
     * @return the shenyu result
     */
    @PostMapping("/batchEnabled")
    @RequiresPermissions("system:dict:disable")
    public ShenyuAdminResult batchEnabled(@Valid @RequestBody final BatchCommonDTO batchCommonDTO) {
        final Integer result = shenyuDictService.enabled(batchCommonDTO.getIds(), batchCommonDTO.getEnabled());
        return ShenyuAdminResult.success("batch enable success", result);
    }
}
