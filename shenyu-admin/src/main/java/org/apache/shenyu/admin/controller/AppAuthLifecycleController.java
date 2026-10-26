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
import org.apache.shenyu.admin.model.dto.AppAuthApplyRequestDTO;
import org.apache.shenyu.admin.model.dto.AppAuthReplacePathsRequestDTO;
import org.apache.shenyu.admin.model.dto.AppAuthResultQueryDTO;
import org.apache.shenyu.admin.model.dto.AppAuthRevokeRequestDTO;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.service.AppAuthLifecycleService;
import org.apache.shiro.authz.annotation.RequiresPermissions;
import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;

/**
 * Idempotent app-auth lifecycle controller for machine callers.
 * Validates input, delegates to the service and returns the result.
 * The caller identity is derived from the authenticated session instead of a request parameter,
 * and the four dedicated permissions below are required so that dashboard list permissions
 * alone can never obtain credentials.
 */
@RestApi("/appAuth/lifecycle")
public class AppAuthLifecycleController {

    private final AppAuthLifecycleService appAuthLifecycleService;

    public AppAuthLifecycleController(final AppAuthLifecycleService appAuthLifecycleService) {
        this.appAuthLifecycleService = appAuthLifecycleService;
    }

    /**
     * Apply for app auth credentials.
     *
     * @param request apply request
     * @return {@link ShenyuAdminResult} apply result
     */
    @PostMapping("/apply")
    @RequiresPermissions("system:authen:lifecycleApply")
    public ShenyuAdminResult apply(@Valid @RequestBody final AppAuthApplyRequestDTO request) {
        return appAuthLifecycleService.apply(request);
    }

    /**
     * Query the result of a previous apply (read-only, GET parameter binding).
     *
     * @param query result query by namespaceId and requestId
     * @return {@link ShenyuAdminResult} current apply state
     */
    @GetMapping("/result")
    @RequiresPermissions("system:authen:lifecycleResult")
    public ShenyuAdminResult result(@Valid final AppAuthResultQueryDTO query) {
        return appAuthLifecycleService.result(query);
    }

    /**
     * Revoke a previously applied app auth.
     *
     * @param request revoke request
     * @return {@link ShenyuAdminResult} revoke result
     */
    @PostMapping("/revoke")
    @RequiresPermissions("system:authen:lifecycleRevoke")
    public ShenyuAdminResult revoke(@Valid @RequestBody final AppAuthRevokeRequestDTO request) {
        return appAuthLifecycleService.revoke(request);
    }

    /**
     * Replace the managed auth paths as a whole.
     *
     * @param request replacePaths request
     * @return {@link ShenyuAdminResult} replace result
     */
    @PutMapping("/replacePaths")
    @RequiresPermissions("system:authen:lifecycleReplacePaths")
    public ShenyuAdminResult replacePaths(@Valid @RequestBody final AppAuthReplacePathsRequestDTO request) {
        return appAuthLifecycleService.replacePaths(request);
    }
}
