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

import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.dto.BatchCommonDTO;
import org.apache.shenyu.admin.model.dto.MetaDataDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.MetaDataQuery;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.MetaDataVO;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shiro.authz.annotation.Logical;
import org.apache.shiro.authz.annotation.RequiresPermissions;
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

/**
 * The type Meta data controller.
 */
@Validated
@RestController
@RequestMapping("/meta-data")
public class MetaDataController {

    private final MetaDataService metaDataService;

    public MetaDataController(final MetaDataService metaDataService) {
        this.metaDataService = metaDataService;
    }

    /**
     * Query metadata list.
     *
     * @param path     the path
     * @param currentPage the current page
     * @param pageSize    the page size
     * @return the shenyu result
     */
    @GetMapping("/queryList")
    @RequiresPermissions("system:meta:list")
    public ShenyuAdminResult queryList(final String path,
                                       @RequestParam @NotNull(message = "currentPage not null") final Integer currentPage,
                                       @RequestParam @NotNull(message = "pageSize not null") final Integer pageSize) {
        CommonPager<MetaDataVO> commonPager = metaDataService.listByPage(new MetaDataQuery(path, new PageParameter(currentPage, pageSize)));
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, commonPager);
    }

    /**
     * Find all metadata.
     *
     * @return the shenyu result
     */
    @GetMapping("/findAll")
    @RequiresPermissions("system:meta:list")
    public ShenyuAdminResult findAll() {
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, metaDataService.findAll());
    }

    /**
     * Find all group of metadata.
     *
     * @return the shenyu result
     */
    @GetMapping("/findAllGroup")
    @RequiresPermissions("system:meta:list")
    public ShenyuAdminResult findAllGroup() {
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, metaDataService.findAllGroup());
    }

    /**
     * Get detail of metadata.
     *
     * @param id the id
     * @return the shenyu result
     */
    @GetMapping("/{id}")
    @RequiresPermissions("system:meta:edit")
    public ShenyuAdminResult detail(@PathVariable("id") final String id) {
        return ShenyuAdminResult.success(ShenyuResultMessage.DETAIL_SUCCESS, metaDataService.findById(id));
    }

    /**
     * Create or update metadata.
     *
     * @param metaDataDTO the meta data dto
     * @return the shenyu result
     */
    @PostMapping("/createOrUpdate")
    @RequiresPermissions(value = {"system:meta:add", "system:meta:edit"}, logical = Logical.OR)
    public ShenyuAdminResult createOrUpdate(@Valid @RequestBody final MetaDataDTO metaDataDTO) {
        return ShenyuAdminResult.success(metaDataService.createOrUpdate(metaDataDTO));
    }

    /**
     * Batch deleted metadata.
     *
     * @param ids the ids
     * @return the shenyu result
     */
    @PostMapping("/batchDeleted")
    @RequiresPermissions("system:meta:delete")
    public ShenyuAdminResult batchDeleted(@RequestBody @NotEmpty final List<@NotBlank String> ids) {
        Integer deleteCount = metaDataService.delete(ids);
        return ShenyuAdminResult.success(ShenyuResultMessage.DELETE_SUCCESS, deleteCount);
    }
    
    /**
     * Batch deleted metadata.
     *
     * @param ids the ids
     * @return the shenyu result
     */
    @DeleteMapping("/batchDeleted")
    @RequiresPermissions("system:meta:delete")
    public ShenyuAdminResult batchDelete(@RequestBody @NotEmpty final List<@NotBlank String> ids) {
        return batchDeleted(ids);
    }

    /**
     * Batch enabled metadata.
     *
     * @param batchCommonDTO the batch common dto
     * @return the shenyu result
     */
    @PostMapping("/batchEnabled")
    @RequiresPermissions("system:meta:disable")
    public ShenyuAdminResult batchEnabled(@Valid @RequestBody final BatchCommonDTO batchCommonDTO) {
        final String result = metaDataService.enabled(batchCommonDTO.getIds(), batchCommonDTO.getEnabled());
        if (StringUtils.isNoneBlank(result)) {
            return ShenyuAdminResult.error(result);
        }
        return ShenyuAdminResult.success(ShenyuResultMessage.ENABLE_SUCCESS);
    }

    /**
     * Sync metadata.
     *
     * @return the shenyu result
     */
    @PostMapping("/syncData")
    @RequiresPermissions("system:meta:modify")
    public ShenyuAdminResult syncData() {
        metaDataService.syncData();
        return ShenyuAdminResult.success();
    }
}
