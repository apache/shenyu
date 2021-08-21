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

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.admin.config.properties.SecretProperties;
import org.apache.shenyu.admin.model.dto.DashboardUserDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.DashboardUserQuery;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.DashboardUserEditVO;
import org.apache.shenyu.admin.model.vo.DashboardUserVO;
import org.apache.shenyu.admin.service.DashboardUserService;
import org.apache.shenyu.admin.utils.AesUtils;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import java.util.List;
import java.util.Optional;

/**
 * this is dashboard user controller.
 */
@Validated
@RestController
@RequestMapping("/dashboardUser")
public class DashboardUserController {

    private final SecretProperties secretProperties;

    private final DashboardUserService dashboardUserService;

    public DashboardUserController(final SecretProperties secretProperties, final DashboardUserService dashboardUserService) {
        this.secretProperties = secretProperties;
        this.dashboardUserService = dashboardUserService;
    }

    /**
     * query dashboard users.
     *
     * @param userName    user name
     * @param currentPage current page
     * @param pageSize    page size
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("")
    public ShenyuAdminResult queryDashboardUsers(final String userName, final Integer currentPage, final Integer pageSize) {
        String key = secretProperties.getKey();
        String iv = secretProperties.getIv();
        CommonPager<DashboardUserVO> commonPager = dashboardUserService.listByPage(new DashboardUserQuery(userName, new PageParameter(currentPage, pageSize)));
        if (CollectionUtils.isNotEmpty(commonPager.getDataList())) {
            commonPager.getDataList()
                    .forEach(item -> item.setPassword(AesUtils.aesDecryption(item.getPassword(), key, iv)));
            return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, commonPager);
        } else {
            return ShenyuAdminResult.error(ShenyuResultMessage.DASHBOARD_QUERY_ERROR);
        }
    }

    /**
     * detail dashboard user.
     *
     * @param id dashboard user id.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/{id}")
    public ShenyuAdminResult detailDashboardUser(@PathVariable("id") final String id) {
        DashboardUserEditVO dashboardUserEditVO = dashboardUserService.findById(id);
        return Optional.ofNullable(dashboardUserEditVO).map(item -> {
            item.setPassword("");
            return ShenyuAdminResult.success(ShenyuResultMessage.DETAIL_SUCCESS, item);
        }).orElseGet(() -> ShenyuAdminResult.error(ShenyuResultMessage.DASHBOARD_QUERY_ERROR));
    }

    /**
     * create dashboard user.
     *
     * @param dashboardUserDTO dashboard user.
     * @return {@linkplain ShenyuAdminResult}
     */
    @PostMapping("")
    public ShenyuAdminResult createDashboardUser(@Valid @RequestBody final DashboardUserDTO dashboardUserDTO) {
        String key = secretProperties.getKey();
        String iv = secretProperties.getIv();
        return Optional.ofNullable(dashboardUserDTO).map(item -> {
            item.setPassword(AesUtils.aesEncryption(item.getPassword(), key, iv));
            Integer createCount = dashboardUserService.createOrUpdate(item);
            return ShenyuAdminResult.success(ShenyuResultMessage.CREATE_SUCCESS, createCount);
        }).orElseGet(() -> ShenyuAdminResult.error(ShenyuResultMessage.DASHBOARD_CREATE_USER_ERROR));
    }

    /**
     * update dashboard user.
     *
     * @param id               primary key.
     * @param dashboardUserDTO dashboard user.
     * @return {@linkplain ShenyuAdminResult}
     */
    @PutMapping("/{id}")
    public ShenyuAdminResult updateDashboardUser(@PathVariable("id") final String id, @Valid @RequestBody final DashboardUserDTO dashboardUserDTO) {
        String key = secretProperties.getKey();
        String iv = secretProperties.getIv();
        dashboardUserDTO.setId(id);
        dashboardUserDTO.setPassword(AesUtils.aesEncryption(dashboardUserDTO.getPassword(), key, iv));
        Integer updateCount = dashboardUserService.createOrUpdate(dashboardUserDTO);
        return ShenyuAdminResult.success(ShenyuResultMessage.UPDATE_SUCCESS, updateCount);
    }

    /**
     * delete dashboard users.
     *
     * @param ids primary key.
     * @return {@linkplain ShenyuAdminResult}
     */
    @DeleteMapping("/batch")
    public ShenyuAdminResult deleteDashboardUser(@RequestBody @NotEmpty final List<@NotBlank String> ids) {
        Integer deleteCount = dashboardUserService.delete(ids);
        return ShenyuAdminResult.success(ShenyuResultMessage.DELETE_SUCCESS, deleteCount);
    }
}
