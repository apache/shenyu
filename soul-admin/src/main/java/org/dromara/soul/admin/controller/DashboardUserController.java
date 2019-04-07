/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.admin.controller;

import org.dromara.soul.admin.dto.DashboardUserDTO;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.page.PageParameter;
import org.dromara.soul.admin.query.DashboardUserQuery;
import org.dromara.soul.admin.service.DashboardUserService;
import org.dromara.soul.admin.vo.DashboardUserVO;
import org.dromara.soul.common.result.SoulResult;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.DeleteMapping;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Objects;

/**
 * this is dashboard user controller.
 *
 * @author jiangxiaofeng(Nicholas)
 */
@RestController
@RequestMapping("/dashboardUser")
public class DashboardUserController {

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
     * @return {@linkplain Mono}
     */
    @GetMapping("")
    public Mono<SoulResult> queryDashboardUsers(final String userName, final Integer currentPage, final Integer pageSize) {
        try {
            CommonPager<DashboardUserVO> commonPager = dashboardUserService.listByPage(new DashboardUserQuery(userName, new PageParameter(currentPage, pageSize)));
            return Mono.create(soulResult -> soulResult.success(SoulResult.success("query dashboard users success", commonPager)));
        } catch (Exception e) {
            return Mono.create(soulResult -> soulResult.success(SoulResult.error("query dashboard users exception")));
        }
    }

    /**
     * detail dashboard user.
     *
     * @param id dashboard user id.
     * @return {@linkplain Mono}
     */
    @GetMapping("/{id}")
    public Mono<SoulResult> detailDashboardUser(@PathVariable("id") final String id) {
        try {
            DashboardUserVO dashboardUserVO = dashboardUserService.findById(id);
            return Mono.create(soulResult -> soulResult.success(SoulResult.success("detail dashboard user success", dashboardUserVO)));
        } catch (Exception e) {
            return Mono.create(soulResult -> soulResult.success(SoulResult.error("detail dashboard user exception")));
        }
    }

    /**
     * create dashboard user.
     *
     * @param dashboardUserDTO dashboard user.
     * @return {@linkplain Mono}
     */
    @PostMapping("")
    public Mono<SoulResult> createDashboardUser(@RequestBody final DashboardUserDTO dashboardUserDTO) {
        try {
            Integer createCount = dashboardUserService.createOrUpdate(dashboardUserDTO);
            return Mono.create(soulResult -> soulResult.success(SoulResult.success("create dashboard user success", createCount)));
        } catch (Exception e) {
            return Mono.create(soulResult -> soulResult.success(SoulResult.error("create dashboard user exception")));
        }
    }

    /**
     * update dashboard user.
     *
     * @param id               primary key.
     * @param dashboardUserDTO dashboard user.
     * @return {@linkplain Mono}
     */
    @PutMapping("/{id}")
    public Mono<SoulResult> updateDashboardUser(@PathVariable("id") final String id, @RequestBody final DashboardUserDTO dashboardUserDTO) {
        try {
            Objects.requireNonNull(dashboardUserDTO);
            dashboardUserDTO.setId(id);
            Integer updateCount = dashboardUserService.createOrUpdate(dashboardUserDTO);
            return Mono.create(soulResult -> soulResult.success(SoulResult.success("update dashboard user success", updateCount)));
        } catch (Exception e) {
            return Mono.create(soulResult -> soulResult.success(SoulResult.error("update dashboard user exception")));
        }
    }

    /**
     * delete dashboard users.
     *
     * @param ids primary key.
     * @return {@linkplain Mono}
     */
    @DeleteMapping("/batch")
    public Mono<SoulResult> deleteDashboardUser(@RequestBody final List<String> ids) {
        try {
            Integer deleteCount = dashboardUserService.delete(ids);
            return Mono.create(soulResult -> soulResult.success(SoulResult.success("delete dashboard users success", deleteCount)));
        } catch (Exception e) {
            return Mono.create(soulResult -> soulResult.success(SoulResult.error("delete dashboard users exception")));
        }
    }
}
