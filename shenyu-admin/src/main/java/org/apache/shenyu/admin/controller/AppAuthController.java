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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.mapper.AppAuthMapper;
import org.apache.shenyu.admin.mapper.AuthPathMapper;
import org.apache.shenyu.admin.model.dto.AppAuthDTO;
import org.apache.shenyu.admin.model.dto.AuthApplyDTO;
import org.apache.shenyu.admin.model.dto.AuthPathWarpDTO;
import org.apache.shenyu.admin.model.dto.BatchCommonDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.AppAuthQuery;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.AppAuthVO;
import org.apache.shenyu.admin.service.AppAuthService;
import org.apache.shenyu.admin.service.PageService;
import org.apache.shenyu.admin.service.provider.AppKeyProvider;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.admin.validation.annotation.Existed;
import org.apache.shiro.authz.annotation.RequiresPermissions;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.util.List;

/**
 * this is application authority controller.
 */
@Validated
@RestController
@RequestMapping("/appAuth")
public class AppAuthController implements PagedController<AppAuthQuery, AppAuthVO> {
    
    private final AppAuthService appAuthService;
    
    public AppAuthController(final AppAuthService appAuthService) {
        this.appAuthService = appAuthService;
    }
    
    /**
     * Apply App auth.
     *
     * @param authApplyDTO the auth apply dto
     * @return the shenyu result
     */
    @PostMapping("/apply")
    @RequiresPermissions("system:authen:add")
    public ShenyuAdminResult apply(@Valid @RequestBody final AuthApplyDTO authApplyDTO) {
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
     * @return the shenyu result
     */
    @GetMapping("/updateSk")
    public ShenyuAdminResult updateSk(@RequestParam("appKey")
                                      @Existed(message = "app key not existed",
                                              provider = AppKeyProvider.class) final String appKey,
                                      @RequestParam("appSecret") final String appSecret) {
        return appAuthService.updateAppSecretByAppKey(appKey, appSecret);
    }
    
    /**
     * Find App auth page by query.
     *
     * @param appKey      the app key
     * @param phone       specific phone
     * @param currentPage current page of list
     * @param pageSize    page size of query
     * @return the shenyu result
     */
    @GetMapping("/findPageByQuery")
    @RequiresPermissions("system:authen:list")
    public ShenyuAdminResult findPageByQuery(final String appKey, final String phone,
                                             @RequestParam @NotNull(message = "currentPage not null") final Integer currentPage,
                                             @RequestParam @NotNull(message = "pageSize not null") final Integer pageSize) {
        AppAuthQuery query = new AppAuthQuery();
        query.setPhone(phone);
        query.setAppKey(appKey);
        query.setPageParameter(new PageParameter(currentPage, pageSize));
        CommonPager<AppAuthVO> commonPager = appAuthService.listByPage(query);
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, commonPager);
    }
    
    /**
     * Get detail of App auth.
     *
     * @param id the id
     * @return the shenyu result
     */
    @GetMapping("/detail")
    @RequiresPermissions("system:authen:editResourceDetails")
    public ShenyuAdminResult detail(@RequestParam("id")
                                    @Existed(message = "app key not existed",
                                            provider = AppAuthMapper.class) final String id) {
        return ShenyuAdminResult.success(ShenyuResultMessage.DETAIL_SUCCESS, appAuthService.findById(id));
    }
    
    /**
     * Update App auth.
     *
     * @param appAuthDTO the app auth dto
     * @return the shenyu result
     */
    @PostMapping("/updateDetail")
    @RequiresPermissions("system:authen:edit")
    public ShenyuAdminResult updateDetail(@RequestBody @Valid final AppAuthDTO appAuthDTO) {
        return appAuthService.updateDetail(appAuthDTO);
    }
    
    /**
     * Detail path of App auth.
     *
     * @param authId the authId
     * @return the shenyu result
     */
    @GetMapping("/detailPath")
    @RequiresPermissions("system:authen:editResourceDetails")
    public ShenyuAdminResult detailPath(@RequestParam("id")
                                            @Existed(message = "auth path not existed",
                                                    providerMethodName = "existedByAuthId",
                                                    provider = AuthPathMapper.class)
                                        @NotBlank final String authId) {
        return ShenyuAdminResult.success(ShenyuResultMessage.DETAIL_SUCCESS, appAuthService.detailPath(authId));
    }
    
    /**
     * Update detail path.
     *
     * @param authPathWarpDTO the auth path warp dto
     * @return the shenyu result
     */
    @PostMapping("/updateDetailPath")
    @RequiresPermissions("system:authen:editResourceDetails")
    public ShenyuAdminResult updateDetailPath(@RequestBody @Valid final AuthPathWarpDTO authPathWarpDTO) {
        return appAuthService.updateDetailPath(authPathWarpDTO);
    }
    
    /**
     * delete application authorities.
     *
     * @param ids primary key.
     * @return {@linkplain ShenyuAdminResult}
     */
    @PostMapping("/batchDelete")
    @RequiresPermissions("system:authen:delete")
    public ShenyuAdminResult batchDelete(@RequestBody @NotEmpty final List<@NotBlank String> ids) {
        Integer deleteCount = appAuthService.delete(ids);
        return ShenyuAdminResult.success(ShenyuResultMessage.DELETE_SUCCESS, deleteCount);
    }
    
    /**
     * Batch enabled App auth.
     *
     * @param batchCommonDTO the batch common dto
     * @return the shenyu result
     */
    @PostMapping("/batchEnabled")
    @RequiresPermissions("system:authen:disable")
    public ShenyuAdminResult batchEnabled(@Valid @RequestBody final BatchCommonDTO batchCommonDTO) {
        final String result = appAuthService.enabled(batchCommonDTO.getIds(), batchCommonDTO.getEnabled());
        if (StringUtils.isNoneBlank(result)) {
            return ShenyuAdminResult.error(result);
        }
        return ShenyuAdminResult.success(ShenyuResultMessage.ENABLE_SUCCESS);
    }
    
    /**
     * Sync App auth data.
     *
     * @return the shenyu result
     */
    @PostMapping("/syncData")
    @RequiresPermissions("system:authen:modify")
    public ShenyuAdminResult syncData() {
        return appAuthService.syncData();
    }
    
    @Override
    public PageService<AppAuthQuery, AppAuthVO> pageService() {
        return appAuthService;
    }
}
