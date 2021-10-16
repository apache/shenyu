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

import org.apache.shenyu.admin.model.vo.DataPermissionPageVO;
import org.apache.shenyu.admin.service.DataPermissionService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.admin.model.dto.DataPermissionDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.RuleQuery;
import org.apache.shenyu.admin.model.query.SelectorQuery;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.Optional;


/**
 * this is dataPermission controller.
 */
@RestController
@RequestMapping("/data-permission")
public class DataPermissionController {

    private final DataPermissionService dataPermissionService;

    @Autowired(required = false)
    public DataPermissionController(final DataPermissionService dataPermissionService) {
        this.dataPermissionService = dataPermissionService;
    }

    /**
     * Query paginated selectors with data permission.
     * @param currentPage current page
     * @param pageSize page size
     * @param userId user id
     * @param pluginId plugin id
     * @param name selector name
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/selector")
    public ShenyuAdminResult listPageSelectorDataPermissions(@RequestParam("currentPage") final Integer currentPage,
                                                             @RequestParam("pageSize") final Integer pageSize,
                                                             @RequestParam("userId") final String userId,
                                                             @RequestParam("pluginId") final String pluginId,
                                                             @RequestParam(value = "name", required = false) final String name) {
        CommonPager<DataPermissionPageVO> selectorList = dataPermissionService.listSelectorsByPage(
                new SelectorQuery(pluginId, name, new PageParameter(currentPage, pageSize)), userId);
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, selectorList);
    }


    /**
     * Query paginated rules with data permission.
     * @param currentPage current page
     * @param pageSize page size
     * @param userId  user id
     * @param selectorId selector id
     * @param name  rule name
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
     * @param dataPermissionDTO {@linkplain DataPermissionDTO}
     * @return effect rows count
     */
    @PostMapping("/selector")
    public ShenyuAdminResult saveSelector(@RequestBody final DataPermissionDTO dataPermissionDTO) {
        return Optional.ofNullable(dataPermissionDTO)
                .map(item -> ShenyuAdminResult.success(ShenyuResultMessage.SAVE_SUCCESS, dataPermissionService.createSelector(dataPermissionDTO)))
                .orElseGet(() -> ShenyuAdminResult.error(ShenyuResultMessage.SAVE_FAILED));

    }

    /**
     * Delete selector data permission.
     * @param dataPermissionDTO {@linkplain DataPermissionDTO}
     * @return effect rows count
     */
    @DeleteMapping("/selector")
    public ShenyuAdminResult deleteSelector(@RequestBody final DataPermissionDTO dataPermissionDTO) {
        return Optional.ofNullable(dataPermissionDTO)
                .map(item -> ShenyuAdminResult.success(ShenyuResultMessage.DELETE_SUCCESS, dataPermissionService.deleteSelector(dataPermissionDTO)))
                .orElseGet(() -> ShenyuAdminResult.error(ShenyuResultMessage.DELETE_SUCCESS));

    }

    /**
     * Delete rule data permission.
     * @param dataPermissionDTO {@linkplain DataPermissionDTO}
     * @return effect rows count
     */
    @PostMapping("/rule")
    public ShenyuAdminResult saveRule(@RequestBody final DataPermissionDTO dataPermissionDTO) {
        return Optional.ofNullable(dataPermissionDTO)
                .map(item -> ShenyuAdminResult.success(ShenyuResultMessage.SAVE_SUCCESS, dataPermissionService.createRule(dataPermissionDTO)))
                .orElseGet(() -> ShenyuAdminResult.error(ShenyuResultMessage.SAVE_FAILED));
    }

    /**
     * Delete selector data permission.
     * @param dataPermissionDTO {@linkplain DataPermissionDTO}
     * @return effect rows count
     */
    @DeleteMapping("/rule")
    public ShenyuAdminResult deleteRule(@RequestBody final DataPermissionDTO dataPermissionDTO) {
        return Optional.ofNullable(dataPermissionDTO)
                .map(item -> ShenyuAdminResult.success(ShenyuResultMessage.DELETE_SUCCESS, dataPermissionService.deleteRule(dataPermissionDTO)))
                .orElseGet(() -> ShenyuAdminResult.error(ShenyuResultMessage.DELETE_SUCCESS));

    }
}
