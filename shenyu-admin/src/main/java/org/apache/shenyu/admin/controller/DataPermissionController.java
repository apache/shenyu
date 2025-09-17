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
import org.apache.shenyu.admin.model.dto.DataPermissionDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.RuleQuery;
import org.apache.shenyu.admin.model.query.SelectorQuery;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.DataPermissionPageVO;
import org.apache.shenyu.admin.service.DataPermissionService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.admin.validation.annotation.Existed;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotNull;

/**
 * this is dataPermission controller.
 */
@RestApi("/data-permission")
public class DataPermissionController {
    
    private final DataPermissionService dataPermissionService;
    
    public DataPermissionController(final DataPermissionService dataPermissionService) {
        this.dataPermissionService = dataPermissionService;
    }
    
    /**
     * Query paginated selectors with data permission.
     *
     * @param currentPage current page
     * @param pageSize    page size
     * @param userId      user id
     * @param pluginId    plugin id
     * @param name        selector name
     * @param namespaceId namespaceId
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/selector")
    public ShenyuAdminResult listPageSelectorDataPermissions(@RequestParam("currentPage") final Integer currentPage,
                                                             @RequestParam("pageSize") final Integer pageSize,
                                                             @RequestParam("userId") final String userId,
                                                             @RequestParam("pluginId") final String pluginId,
                                                             @RequestParam(value = "name", required = false) final String name,
                                                             @Valid @Existed(message = "namespaceId is not existed",
                                                                     provider = NamespaceMapper.class) final String namespaceId) {
        CommonPager<DataPermissionPageVO> selectorList = dataPermissionService.listSelectorsByPage(
                new SelectorQuery(pluginId, name, new PageParameter(currentPage, pageSize), namespaceId), userId);
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, selectorList);
    }
    
    
    /**
     * Query paginated rules with data permission.
     *
     * @param currentPage current page
     * @param pageSize    page size
     * @param userId      user id
     * @param selectorId  selector id
     * @param name        rule name
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/rules")
    public ShenyuAdminResult listPageRuleDataPermissions(@RequestParam("currentPage") final Integer currentPage,
                                                         @RequestParam("pageSize") final Integer pageSize,
                                                         @RequestParam("userId") final String userId,
                                                         @RequestParam("selectorId") final String selectorId,
                                                         @RequestParam(value = "name", required = false) final String name) {
        CommonPager<DataPermissionPageVO> selectorList = dataPermissionService.listRulesByPage(
                new RuleQuery(selectorId, name, new PageParameter(currentPage, pageSize)), userId);
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, selectorList);
    }
    
    
    /**
     * create selector data permission.
     *
     * @param dataPermissionDTO {@linkplain DataPermissionDTO}
     * @return effect rows count
     */
    @PostMapping("/selector")
    public ShenyuAdminResult saveSelector(@RequestBody @Valid @NotNull final DataPermissionDTO dataPermissionDTO) {
        return ShenyuAdminResult.success(ShenyuResultMessage.SAVE_SUCCESS, dataPermissionService.createSelector(dataPermissionDTO));
    }
    
    /**
     * Delete selector data permission.
     *
     * @param dataPermissionDTO {@linkplain DataPermissionDTO}
     * @return effect rows count
     */
    @DeleteMapping("/selector")
    public ShenyuAdminResult deleteSelector(@RequestBody @Valid @NotNull final DataPermissionDTO dataPermissionDTO) {
        return ShenyuAdminResult.success(ShenyuResultMessage.DELETE_SUCCESS, dataPermissionService.deleteSelector(dataPermissionDTO));
    }
    
    /**
     * Delete rule data permission.
     *
     * @param dataPermissionDTO {@linkplain DataPermissionDTO}
     * @return effect rows count
     */
    @PostMapping("/rule")
    public ShenyuAdminResult saveRule(@RequestBody @Valid @NotNull final DataPermissionDTO dataPermissionDTO) {
        return ShenyuAdminResult.success(ShenyuResultMessage.SAVE_SUCCESS, dataPermissionService.createRule(dataPermissionDTO));
    }
    
    /**
     * Delete selector data permission.
     *
     * @param dataPermissionDTO {@linkplain DataPermissionDTO}
     * @return effect rows count
     */
    @DeleteMapping("/rule")
    public ShenyuAdminResult deleteRule(@RequestBody @Valid @NotNull final DataPermissionDTO dataPermissionDTO) {
        return ShenyuAdminResult.success(ShenyuResultMessage.DELETE_SUCCESS, dataPermissionService.deleteRule(dataPermissionDTO));
    }
}
