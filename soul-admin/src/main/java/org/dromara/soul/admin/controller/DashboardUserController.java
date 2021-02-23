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

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.dromara.soul.admin.config.properties.SecretProperties;
import org.dromara.soul.admin.dto.DashboardUserDTO;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.page.PageParameter;
import org.dromara.soul.admin.query.DashboardUserQuery;
import org.dromara.soul.admin.result.SoulAdminResult;
import org.dromara.soul.admin.service.DashboardUserService;
import org.dromara.soul.admin.utils.AesUtils;
import org.dromara.soul.admin.utils.SoulResultMessage;
import org.dromara.soul.admin.vo.DashboardUserEditVO;
import org.dromara.soul.admin.vo.DashboardUserVO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * this is dashboard user controller.
 *
 * @author jiangxiaofeng(Nicholas)
 * @author Jiang Jining
 */
@RestController
@RequestMapping("/dashboardUser")
@Slf4j
public class DashboardUserController {

    @Resource
    private SecretProperties secretProperties;

    private final DashboardUserService dashboardUserService;

    @Autowired(required = false)
    public DashboardUserController(final DashboardUserService dashboardUserService) {
        this.dashboardUserService = dashboardUserService;
    }

    /**
     * query dashboard users.
     *
     * @param userName    user name
     * @param currentPage current page
     * @param pageSize    page size
     * @return {@linkplain SoulAdminResult}
     */
    @GetMapping("")
    public SoulAdminResult queryDashboardUsers(final String userName, final Integer currentPage, final Integer pageSize) {
        String key = secretProperties.getKey();
        CommonPager<DashboardUserVO> commonPager = dashboardUserService.listByPage(new DashboardUserQuery(userName, new PageParameter(currentPage, pageSize)));
        if (CollectionUtils.isNotEmpty(commonPager.getDataList())) {
            commonPager.getDataList().forEach(item -> item.setPassword(AesUtils.aesDecryption(item.getPassword(), key)));
            return SoulAdminResult.success(SoulResultMessage.QUERY_SUCCESS, commonPager);
        } else {
            return SoulAdminResult.error(SoulResultMessage.DASHBOARD_QUERY_ERROR);
        }
    }

    /**
     * detail dashboard user.
     *
     * @param id dashboard user id.
     * @return {@linkplain SoulAdminResult}
     */
    @GetMapping("/{id}")
    public SoulAdminResult detailDashboardUser(@PathVariable("id") final String id) {
        String key = secretProperties.getKey();
        DashboardUserEditVO dashboardUserEditVO = dashboardUserService.findById(id);
        return Optional.ofNullable(dashboardUserEditVO).map(item -> {
            item.setPassword(AesUtils.aesDecryption(item.getPassword(), key));
            return SoulAdminResult.success(SoulResultMessage.DETAIL_SUCCESS, item);
        }).orElse(SoulAdminResult.error(SoulResultMessage.DASHBOARD_QUERY_ERROR));
    }

    /**
     * create dashboard user.
     *
     * @param dashboardUserDTO dashboard user.
     * @return {@linkplain SoulAdminResult}
     */
    @PostMapping("")
    public SoulAdminResult createDashboardUser(@RequestBody final DashboardUserDTO dashboardUserDTO) {
        String key = secretProperties.getKey();
        return Optional.ofNullable(dashboardUserDTO).map(item -> {
            item.setPassword(AesUtils.aesEncryption(item.getPassword(), key));
            Integer createCount = dashboardUserService.createOrUpdate(item);
            log.info("dashboard user created, info:[{}]", item);
            return SoulAdminResult.success(SoulResultMessage.CREATE_SUCCESS, createCount);
        }).orElse(SoulAdminResult.error(SoulResultMessage.DASHBOARD_CREATE_USER_ERROR));
    }

    /**
     * update dashboard user.
     *
     * @param id               primary key.
     * @param dashboardUserDTO dashboard user.
     * @return {@linkplain SoulAdminResult}
     */
    @PutMapping("/{id}")
    public SoulAdminResult updateDashboardUser(@PathVariable("id") final String id, @RequestBody final DashboardUserDTO dashboardUserDTO) {
        Objects.requireNonNull(dashboardUserDTO);
        String key = secretProperties.getKey();
        dashboardUserDTO.setId(id);
        dashboardUserDTO.setPassword(AesUtils.aesEncryption(dashboardUserDTO.getPassword(), key));
        Integer updateCount = dashboardUserService.createOrUpdate(dashboardUserDTO);
        log.info("dashboard user updated, id:[{}], info:[{}]", id, dashboardUserDTO);
        return SoulAdminResult.success(SoulResultMessage.UPDATE_SUCCESS, updateCount);
    }

    /**
     * delete dashboard users.
     *
     * @param ids primary key.
     * @return {@linkplain SoulAdminResult}
     */
    @DeleteMapping("/batch")
    public SoulAdminResult deleteDashboardUser(@RequestBody final List<String> ids) {
        Integer deleteCount = dashboardUserService.delete(ids);
        log.info("dashboard users deleted, id:[{}]", ids);
        return SoulAdminResult.success(SoulResultMessage.DELETE_SUCCESS, deleteCount);
    }
}
