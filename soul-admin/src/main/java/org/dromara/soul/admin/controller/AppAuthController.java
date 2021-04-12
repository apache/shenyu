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

import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.admin.model.dto.AppAuthDTO;
import org.dromara.soul.admin.model.dto.AuthApplyDTO;
import org.dromara.soul.admin.model.dto.AuthPathWarpDTO;
import org.dromara.soul.admin.model.dto.BatchCommonDTO;
import org.dromara.soul.admin.model.page.CommonPager;
import org.dromara.soul.admin.model.page.PageParameter;
import org.dromara.soul.admin.model.query.AppAuthQuery;
import org.dromara.soul.admin.model.result.SoulAdminResult;
import org.dromara.soul.admin.service.AppAuthService;
import org.dromara.soul.admin.utils.SoulResultMessage;
import org.dromara.soul.admin.model.vo.AppAuthVO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * this is application authority controller.
 *
 * @author jiangxiaofeng(Nicholas)
 */
@RestController
@RequestMapping("/appAuth")
public class AppAuthController {

    private final AppAuthService appAuthService;

    /**
     * Instantiates a new App auth controller.
     *
     * @param appAuthService the app auth service
     */
    @Autowired(required = false)
    public AppAuthController(final AppAuthService appAuthService) {
        this.appAuthService = appAuthService;
    }

    /**
     * Apply App auth.
     *
     * @param authApplyDTO the auth apply dto
     * @return the soul result
     */
    @PostMapping("/apply")
    public SoulAdminResult apply(@RequestBody final AuthApplyDTO authApplyDTO) {
        if (StringUtils.isNoneBlank(authApplyDTO.getAppKey())) {
            return appAuthService.applyUpdate(authApplyDTO);
        }
        return appAuthService.applyCreate(authApplyDTO);
    }

    /**
     * Update sk of App auth.
     *
     * @param appKey    the app key
     * @param appSecret the app secret
     * @return the soul result
     */
    @GetMapping("/updateSk")
    public SoulAdminResult updateSk(@RequestParam("appKey") final String appKey, @RequestParam("appSecret") final String appSecret) {
        return appAuthService.updateAppSecretByAppKey(appKey, appSecret);
    }

    /**
     * Find App auth page by query.
     *
     * @param appKey the app key
     * @param phone  specific phone
     * @param currentPage  current page of list
     * @param pageSize  page size of query
     * @return the soul result
     */
    @GetMapping("/findPageByQuery")
    public SoulAdminResult findPageByQuery(final String appKey, final String phone, final Integer currentPage, final Integer pageSize) {
        AppAuthQuery query = new AppAuthQuery();
        query.setPhone(phone);
        query.setAppKey(appKey);
        query.setPageParameter(new PageParameter(currentPage, pageSize));
        CommonPager<AppAuthVO> commonPager = appAuthService.listByPage(query);
        return SoulAdminResult.success(SoulResultMessage.QUERY_SUCCESS, commonPager);
    }

    /**
     * Get detail of App auth.
     *
     * @param id the id
     * @return the soul result
     */
    @GetMapping("/detail")
    public SoulAdminResult detail(@RequestParam("id") final String id) {
        return SoulAdminResult.success(SoulResultMessage.DETAIL_SUCCESS, appAuthService.findById(id));
    }

    /**
     * Update App auth.
     *
     * @param appAuthDTO the app auth dto
     * @return the soul result
     */
    @PostMapping("/updateDetail")
    public SoulAdminResult updateDetail(@RequestBody final AppAuthDTO appAuthDTO) {
        return appAuthService.updateDetail(appAuthDTO);
    }

    /**
     * Detail path of App auth.
     *
     * @param id the id
     * @return the soul result
     */
    @GetMapping("/detailPath")
    public SoulAdminResult detailPath(@RequestParam("id") final String id) {
        return SoulAdminResult.success(SoulResultMessage.DETAIL_SUCCESS, appAuthService.detailPath(id));
    }

    /**
     * Update detail path.
     *
     * @param authPathWarpDTO the auth path warp dto
     * @return the soul result
     */
    @PostMapping("/updateDetailPath")
    public SoulAdminResult updateDetailPath(@RequestBody final AuthPathWarpDTO authPathWarpDTO) {
        return appAuthService.updateDetailPath(authPathWarpDTO);
    }

    /**
     * delete application authorities.
     *
     * @param ids primary key.
     * @return {@linkplain SoulAdminResult}
     */
    @PostMapping("/batchDelete")
    public SoulAdminResult batchDelete(@RequestBody final List<String> ids) {
        Integer deleteCount = appAuthService.delete(ids);
        return SoulAdminResult.success(SoulResultMessage.DELETE_SUCCESS, deleteCount);
    }

    /**
     * Batch enabled App auth.
     *
     * @param batchCommonDTO the batch common dto
     * @return the soul result
     */
    @PostMapping("/batchEnabled")
    public SoulAdminResult batchEnabled(@RequestBody final BatchCommonDTO batchCommonDTO) {
        final String result = appAuthService.enabled(batchCommonDTO.getIds(), batchCommonDTO.getEnabled());
        if (StringUtils.isNoneBlank(result)) {
            return SoulAdminResult.error(result);
        }
        return SoulAdminResult.success(SoulResultMessage.ENABLE_SUCCESS);
    }

    /**
     * Sync App auth data.
     *
     * @return the soul result
     */
    @PostMapping("/syncData")
    public SoulAdminResult syncData() {
        return appAuthService.syncData();
    }
}
