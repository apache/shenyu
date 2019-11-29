/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
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
     * Query list soul result.
     *
     * @param appName     the app name
     * @param currentPage the current page
     * @param pageSize    the page size
     * @return the soul result
     */
    @GetMapping("/queryList")
    public SoulAdminResult queryList(final String appName, final Integer currentPage, final Integer pageSize) {
        CommonPager<MetaDataVO> commonPager = metaDataService.listByPage(new MetaDataQuery(appName, new PageParameter(currentPage, pageSize)));
        return SoulAdminResult.success("query  success", commonPager);
    }

    /**
     * Find all soul result.
     *
     * @return the soul result
     */
    @GetMapping("/findAll")
    public SoulAdminResult findAll() {
        return SoulAdminResult.success("query success", metaDataService.findAll());
    }

    /**
     * Find all group soul result.
     *
     * @return the soul result
     */
    @GetMapping("/findAllGroup")
    public SoulAdminResult findAllGroup() {
        return SoulAdminResult.success("query success", metaDataService.findAllGroup());
    }


    /**
     * Detail app auth soul result.
     *
     * @param id the id
     * @return the soul result
     */
    @GetMapping("/{id}")
    public SoulAdminResult editor(@PathVariable("id") final String id) {
        MetaDataVO metaDataVO = metaDataService.findById(id);
        return SoulAdminResult.success("detail success", metaDataVO);
    }

    /**
     * Create or update soul result.
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
        return SoulAdminResult.success("create success");
    }


    /**
     * Register string.
     *
     * @param metaDataDTO the meta data dto
     * @return the string
     */
    @PostMapping("/register")
    public String register(@RequestBody final MetaDataDTO metaDataDTO) {
        return metaDataService.register(metaDataDTO);
    }

    /**
     * Batch deleted soul result.
     *
     * @param ids the ids
     * @return the soul result
     */
    @PostMapping("/batchDeleted")
    public SoulAdminResult batchDeleted(@RequestBody final List<String> ids) {
        Integer deleteCount = metaDataService.delete(ids);
        return SoulAdminResult.success("delete  success", deleteCount);
    }


    /**
     * Batch enabled soul result.
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
        return SoulAdminResult.success("enable success");
    }


    /**
     * Sync data soul result.
     *
     * @return the soul result
     */
    @PostMapping("/syncData")
    public SoulAdminResult syncData() {
        metaDataService.syncData();
        return SoulAdminResult.success();
    }
}
