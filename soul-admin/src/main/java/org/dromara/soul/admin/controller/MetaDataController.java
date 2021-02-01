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

import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.admin.dto.BatchCommonDTO;
import org.dromara.soul.admin.dto.MetaDataDTO;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.page.PageParameter;
import org.dromara.soul.admin.query.MetaDataQuery;
import org.dromara.soul.admin.result.SoulAdminResult;
import org.dromara.soul.admin.service.MetaDataService;
import org.dromara.soul.admin.utils.SoulResultMessage;
import org.dromara.soul.admin.vo.MetaDataVO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * The type Meta data controller.
 *
 * @author xiaoyu
 */
@RestController
@RequestMapping("/meta-data")
public class MetaDataController {

    private final MetaDataService metaDataService;

    /**
     * Instantiates a new Meta data controller.
     *
     * @param metaDataService the meta data service
     */
    @Autowired(required = false)
    public MetaDataController(final MetaDataService metaDataService) {
        this.metaDataService = metaDataService;
    }

    /**
     * Query metadata list.
     *
     * @param appName     the app name
     * @param currentPage the current page
     * @param pageSize    the page size
     * @return the soul result
     */
    @GetMapping("/queryList")
    public SoulAdminResult queryList(final String appName, final Integer currentPage, final Integer pageSize) {
        CommonPager<MetaDataVO> commonPager = metaDataService.listByPage(new MetaDataQuery(appName, new PageParameter(currentPage, pageSize)));
        return SoulAdminResult.success(SoulResultMessage.QUERY_SUCCESS, commonPager);
    }

    /**
     * Find all metadata.
     *
     * @return the soul result
     */
    @GetMapping("/findAll")
    public SoulAdminResult findAll() {
        return SoulAdminResult.success(SoulResultMessage.QUERY_SUCCESS, metaDataService.findAll());
    }

    /**
     * Find all group of metadata.
     *
     * @return the soul result
     */
    @GetMapping("/findAllGroup")
    public SoulAdminResult findAllGroup() {
        return SoulAdminResult.success(SoulResultMessage.QUERY_SUCCESS, metaDataService.findAllGroup());
    }

    /**
     * Get detail of metadata.
     *
     * @param id the id
     * @return the soul result
     */
    @GetMapping("/{id}")
    public SoulAdminResult editor(@PathVariable("id") final String id) {
        MetaDataVO metaDataVO = metaDataService.findById(id);
        return SoulAdminResult.success(SoulResultMessage.DETAIL_SUCCESS, metaDataVO);
    }

    /**
     * Create or update metadata.
     *
     * @param metaDataDTO the meta data dto
     * @return the soul result
     */
    @PostMapping("/createOrUpdate")
    public SoulAdminResult createOrUpdate(@RequestBody final MetaDataDTO metaDataDTO) {
        String result = metaDataService.createOrUpdate(metaDataDTO);
        if (StringUtils.isNoneBlank(result)) {
            return SoulAdminResult.error(result);
        }
        return SoulAdminResult.success(SoulResultMessage.CREATE_SUCCESS);
    }

    /**
     * Batch deleted metadata.
     *
     * @param ids the ids
     * @return the soul result
     */
    @PostMapping("/batchDeleted")
    public SoulAdminResult batchDeleted(@RequestBody final List<String> ids) {
        Integer deleteCount = metaDataService.delete(ids);
        return SoulAdminResult.success(SoulResultMessage.DELETE_SUCCESS, deleteCount);
    }

    /**
     * Batch enabled metadata.
     *
     * @param batchCommonDTO the batch common dto
     * @return the soul result
     */
    @PostMapping("/batchEnabled")
    public SoulAdminResult batchEnabled(@RequestBody final BatchCommonDTO batchCommonDTO) {
        final String result = metaDataService.enabled(batchCommonDTO.getIds(), batchCommonDTO.getEnabled());
        if (StringUtils.isNoneBlank(result)) {
            return SoulAdminResult.error(result);
        }
        return SoulAdminResult.success(SoulResultMessage.ENABLE_SUCCESS);
    }
    
    /**
     * Sync metadata.
     *
     * @return the soul result
     */
    @PostMapping("/syncData")
    public SoulAdminResult syncData() {
        metaDataService.syncData();
        return SoulAdminResult.success();
    }
}
