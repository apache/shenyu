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
import org.apache.shenyu.admin.mapper.NamespaceMapper;
import org.apache.shenyu.admin.model.dto.NamespaceDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.NamespaceQuery;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.NamespaceVO;
import org.apache.shenyu.admin.service.NamespaceService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.admin.validation.annotation.Existed;
import org.apache.shiro.authz.annotation.RequiresPermissions;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import java.util.List;

@RestApi("/namespace")
public class NamespaceController {

    private final NamespaceService namespaceService;

    public NamespaceController(final NamespaceService namespaceService) {
        this.namespaceService = namespaceService;
    }

    /**
     * Find namespace page by query.
     *
     * @param namespaceId the namespace id
     * @param name        name
     * @param currentPage current page of list
     * @param pageSize    page size of query
     * @return the shenyu result
     */
    @GetMapping("/findPageByQuery")
    @RequiresPermissions("system:namespace:list")
    public ShenyuAdminResult findPageByQuery(final String namespaceId, final String name,
                                             @RequestParam @NotNull(message = "currentPage not null") final Integer currentPage,
                                             @RequestParam @NotNull(message = "pageSize not null") final Integer pageSize) {
        NamespaceQuery query = new NamespaceQuery();
        query.setNamespaceId(namespaceId);
        query.setName(name);
        query.setPageParameter(new PageParameter(currentPage, pageSize));
        CommonPager<NamespaceVO> commonPager = namespaceService.listByPage(query);
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, commonPager);
    }

    /**
     * Find namespace list.
     *
     * @param name name
     * @return the shenyu result
     */
    @GetMapping("/list")
    @RequiresPermissions("system:namespace:list")
    public ShenyuAdminResult list(final String name) {
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, namespaceService.list(name));
    }


    /**
     * Insert or update namespace.
     *
     * @param namespaceDTO {@link NamespaceDTO}
     * @return {@linkplain ShenyuAdminResult}
     */
    @PostMapping("/insertOrUpdate")
    public ShenyuAdminResult createOrUpdate(@Valid @RequestBody final NamespaceDTO namespaceDTO) {
        return ShenyuAdminResult.success(ShenyuResultMessage.SUCCESS, namespaceService.createOrUpdate(namespaceDTO));
    }

    /**
     * delete by namespaceId.
     *
     * @param ids ids
     * @return {@linkplain ShenyuAdminResult}
     */
    @DeleteMapping("/batch")
    public ShenyuAdminResult delete(@RequestBody final List<@NotBlank String> ids) {
        return ShenyuAdminResult.success(ShenyuResultMessage.SUCCESS, namespaceService.delete(ids));
    }

    /**
     * detail namespace.
     *
     * @param namespaceId namespace id.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/{id}")
    @RequiresPermissions("system:namespace:edit")
    public ShenyuAdminResult detailPlugin(@PathVariable("id")
                                          @Existed(message = "namespaceId is not existed",
                                                  provider = NamespaceMapper.class) final String namespaceId) {
        NamespaceVO namespaceVO = namespaceService.findByNamespaceId(namespaceId);
        return ShenyuAdminResult.success(ShenyuResultMessage.DETAIL_SUCCESS, namespaceVO);
    }
}
