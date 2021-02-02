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

import org.apache.commons.collections4.CollectionUtils;
import org.dromara.soul.admin.dto.ResourceDTO;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.page.PageParameter;
import org.dromara.soul.admin.query.ResourceQuery;
import org.dromara.soul.admin.result.SoulAdminResult;
import org.dromara.soul.admin.service.ResourceService;
import org.dromara.soul.admin.utils.SoulResultMessage;
import org.dromara.soul.admin.vo.ResourceVO;
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
import java.util.Optional;

/**
 * this is resource controller.
 *
 * @author nuo-promise
 */
@RestController
@RequestMapping("/resource")
public class ResourceController {

    private final ResourceService resourceService;

    public ResourceController(final ResourceService resourceService) {
        this.resourceService = resourceService;
    }

    /**
     * query resource.
     *
     * @param title resource title
     * @param currentPage current Page
     * @param pageSize page size
     * @return {@linkplain SoulAdminResult}
     */
    @GetMapping("")
    public SoulAdminResult queryResource(final String title, final Integer currentPage, final Integer pageSize) {
        CommonPager<ResourceVO> commonPager = resourceService.listByPage(new ResourceQuery(title, new PageParameter(currentPage, pageSize)));
        if (CollectionUtils.isNotEmpty(commonPager.getDataList())) {
            return SoulAdminResult.success(SoulResultMessage.QUERY_SUCCESS, commonPager);
        } else {
            return SoulAdminResult.error(SoulResultMessage.QUERY_FAILED);
        }
    }

    /**
     * detail resource info.
     *
     * @param id role id
     * @return {@linkplain SoulAdminResult}
     */
    @GetMapping("/{id}")
    public SoulAdminResult detailResource(@PathVariable("id") final String id) {
        return Optional.ofNullable(resourceService.findById(id))
                .map(item -> SoulAdminResult.success(SoulResultMessage.DETAIL_SUCCESS, item)).orElse(SoulAdminResult.error(SoulResultMessage.DETAIL_FAILED));
    }

    /**
     * create resource.
     *
     * @param resourceDTO resource dto
     * @return {@linkplain SoulAdminResult}
     */
    @PostMapping("")
    public SoulAdminResult createResource(@RequestBody final ResourceDTO resourceDTO) {
        return Optional.ofNullable(resourceDTO)
                .map(item -> SoulAdminResult.success(SoulResultMessage.CREATE_SUCCESS, resourceService.createOrUpdate(item))).orElse(SoulAdminResult.error(SoulResultMessage.CREATE_FAILED));
    }

    /**
     * update resource.
     *
     * @param id primary key.
     * @param resourceDTO resource info
     * @return {@linkplain SoulAdminResult}
     */
    @PutMapping("/{id}")
    public SoulAdminResult updateResource(@PathVariable("id") final String id, @RequestBody final ResourceDTO resourceDTO) {
        Objects.requireNonNull(resourceDTO);
        resourceDTO.setId(id);
        return SoulAdminResult.success(SoulResultMessage.UPDATE_SUCCESS, resourceService.createOrUpdate(resourceDTO));
    }

    /**
     * delete resource info.
     *
     * @param ids primary keys.
     * @return {@linkplain SoulAdminResult}
     */
    @DeleteMapping("/batch")
    public SoulAdminResult deleteResource(@RequestBody final List<String> ids) {
        return SoulAdminResult.success(SoulResultMessage.DELETE_SUCCESS, resourceService.delete(ids));
    }
}
