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

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import org.apache.shenyu.admin.aspect.annotation.RestApi;
import org.apache.shenyu.admin.mapper.RegistryMapper;
import org.apache.shenyu.admin.model.dto.RegistryDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.RegistryQuery;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.RegistryVO;
import org.apache.shenyu.admin.service.impl.RegistryServiceImpl;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.admin.validation.annotation.Existed;
import org.apache.shiro.authz.annotation.Logical;
import org.apache.shiro.authz.annotation.RequiresPermissions;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;

@RestApi("/registry")
public class RegistryController {

    private final RegistryServiceImpl registryService;

    public RegistryController(final RegistryServiceImpl registryService) {
        this.registryService = registryService;
    }

    /**
     * Find registry page by query.
     *
     * @param registryId    the registry registryId
     * @param address       the registry address
     * @param namespace     the registry namespace
     * @param currentPage   current page of list
     * @param pageSize      page size of query
     * @return the shenyu result
     */
    @GetMapping("/findPageByQuery")
    @RequiresPermissions("system:registry:list")
    public ShenyuAdminResult findPageByQuery(final String registryId, final String address, final String namespace,
                                             @RequestParam @NotNull(message = "currentPage not null") final Integer currentPage,
                                             @RequestParam @NotNull(message = "pageSize not null") final Integer pageSize) {
        RegistryQuery query = new RegistryQuery();
        query.setRegistryId(registryId);
        query.setAddress(address);
        query.setNamespace(namespace);
        query.setPageParameter(new PageParameter(currentPage, pageSize));
        CommonPager<RegistryVO> commonPager = registryService.listByPage(query);
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, commonPager);
    }

    /**
     * Find registry list.
     *
     * @return the shenyu result
     */
    @GetMapping("/list")
    @RequiresPermissions("system:registry:list")
    public ShenyuAdminResult list() {
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, registryService.listAll());
    }


    /**
     * Insert or update registry.
     *
     * @param registryDTO {@link RegistryDTO}
     * @return {@linkplain ShenyuAdminResult}
     */
    @PostMapping("/insertOrUpdate")
    @RequiresPermissions(value = {"system:registry:add", "system:registry:edit"}, logical = Logical.OR)
    public ShenyuAdminResult createOrUpdate(@Valid @RequestBody final RegistryDTO registryDTO) {
        return ShenyuAdminResult.success(ShenyuResultMessage.SUCCESS, registryService.createOrUpdate(registryDTO));
    }

    /**
     * delete by id.
     *
     * @param ids ids
     * @return {@linkplain ShenyuAdminResult}
     */
    @DeleteMapping("/batch")
    @RequiresPermissions("system:registry:delete")
    public ShenyuAdminResult delete(@RequestBody final List<@NotBlank String> ids) {
        return ShenyuAdminResult.success(ShenyuResultMessage.SUCCESS, registryService.delete(ids));
    }

    /**
     * detail registry.
     *
     * @param id registry id.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/{id}")
    @RequiresPermissions("system:registry:edit")
    public ShenyuAdminResult detailPlugin(@PathVariable("id")
                                          @Existed(message = "id is not existed",
                                                  provider = RegistryMapper.class) final String id) {
        RegistryVO registryVO = registryService.findById(id);
        return ShenyuAdminResult.success(ShenyuResultMessage.DETAIL_SUCCESS, registryVO);
    }
}
