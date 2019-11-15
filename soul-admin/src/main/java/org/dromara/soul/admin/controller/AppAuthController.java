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

import org.dromara.soul.admin.dto.AppAuthDTO;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.page.PageParameter;
import org.dromara.soul.admin.query.AppAuthQuery;
import org.dromara.soul.admin.service.AppAuthService;
import org.dromara.soul.admin.vo.AppAuthVO;
import org.dromara.soul.common.result.SoulResult;
import org.springframework.beans.factory.annotation.Autowired;
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

/**
 * this is application authority controller.
 *
 * @author jiangxiaofeng(Nicholas)
 */
@RestController
@RequestMapping("/appAuth")
public class AppAuthController {

    private final AppAuthService appAuthService;

    @Autowired(required = false)
    public AppAuthController(final AppAuthService appAuthService) {
        this.appAuthService = appAuthService;
    }

    /**
     * query application authorities.
     *
     * @param appKey      application key.
     * @param currentPage current page.
     * @param pageSize    page size.
     * @return {@linkplain SoulResult}
     */
    @GetMapping("")
    public SoulResult queryAppAuthList(final String appKey, final Integer currentPage, final Integer pageSize) {
        CommonPager<AppAuthVO> commonPager = appAuthService.listByPage(new AppAuthQuery(appKey, new PageParameter(currentPage, pageSize)));
        return SoulResult.success("query application authorities success", commonPager);
    }

    /**
     * detail application authority.
     *
     * @param id application authority id.
     * @return {@linkplain SoulResult}
     */
    @GetMapping("/{id}")
    public SoulResult detailAppAuth(@PathVariable("id") final String id) {
        AppAuthVO appAuthVO = appAuthService.findById(id);
        return SoulResult.success("detail application authority success", appAuthVO);
    }

    /**
     * create application authority.
     *
     * @param appAuthDTO appAuth.
     * @return {@linkplain SoulResult}
     */
    @PostMapping("")
    public SoulResult createAppAuth(@RequestBody final AppAuthDTO appAuthDTO) {
        Integer createCount = appAuthService.createOrUpdate(appAuthDTO);
        return SoulResult.success("create application authority success", createCount);
    }

    /**
     * update application authority.
     *
     * @param id         primary key.
     * @param appAuthDTO AppAuth.
     * @return {@linkplain SoulResult}
     */
    @PutMapping("/{id}")
    public SoulResult updateAppAuth(@PathVariable("id") final String id, @RequestBody final AppAuthDTO appAuthDTO) {
        Objects.requireNonNull(appAuthDTO);
        appAuthDTO.setId(id);
        Integer updateCount = appAuthService.createOrUpdate(appAuthDTO);
        return SoulResult.success("update application authority success", updateCount);
    }

    /**
     * delete application authorities.
     *
     * @param ids primary key.
     * @return {@linkplain SoulResult}
     */
    @DeleteMapping("/batch")
    public SoulResult deleteAppAuths(@RequestBody final List<String> ids) {
        Integer deleteCount = appAuthService.delete(ids);
        return SoulResult.success("delete application authorities success", deleteCount);
    }
}
