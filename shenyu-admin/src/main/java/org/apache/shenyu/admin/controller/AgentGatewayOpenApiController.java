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
import org.apache.shenyu.admin.aspect.annotation.RestApi;
import org.apache.shenyu.admin.model.dto.AgentGatewayOpenApiRequest;
import org.apache.shenyu.admin.model.dto.AgentGatewayOpenApiRequest.Apply;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.service.NamespaceUserService;
import org.apache.shenyu.admin.service.openapi.AgentGatewayOpenApiService;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.apache.shiro.authz.annotation.RequiresPermissions;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import java.util.Objects;

/** Separate preview and explicit publish operations using existing plugin-edit permission. */
@RestApi("/agent-gateway/openapi")
public class AgentGatewayOpenApiController {

    private static final Logger LOG = LoggerFactory.getLogger(AgentGatewayOpenApiController.class);

    private final AgentGatewayOpenApiService service;

    private final NamespaceUserService namespaceUsers;

    public AgentGatewayOpenApiController(final AgentGatewayOpenApiService service, final NamespaceUserService namespaceUsers) {
        this.service = service;
        this.namespaceUsers = namespaceUsers;
    }

    /**
     * Preview only; no configuration writes.
     * @param request explicit import request
     * @return preview and per-operation diagnostics
     */
    @PostMapping("/preview")
    @RequiresPermissions("system:plugin:edit")
    public ShenyuAdminResult preview(@Valid @RequestBody final AgentGatewayOpenApiRequest request) {
        final ShenyuAdminResult denied = authorize(request.namespaceId());
        if (Objects.nonNull(denied)) {
            return denied;
        }
        try {
            return ShenyuAdminResult.success(service.preview(request));
        } catch (IllegalArgumentException error) {
            // Invalid import input and stale previews are client request errors.
            LOG.warn("Failed to preview agent gateway OpenAPI import", error);
            return ShenyuAdminResult.error(HttpStatus.BAD_REQUEST.value(), "Import failed: " + error.getMessage());
        } catch (Exception error) {
            LOG.error("Failed to preview agent gateway OpenAPI import", error);
            return ShenyuAdminResult.error("Import failed: " + error.getMessage());
        }
    }

    /**
     * Publish selected valid changes after confirming a matching preview.
     * @param request preview identity and explicit selections
     * @return refreshed preview
     */
    @PostMapping("/apply")
    @RequiresPermissions("system:plugin:edit")
    public ShenyuAdminResult apply(@Valid @RequestBody final Apply request) {
        final ShenyuAdminResult denied = authorize(request.request().namespaceId());
        if (Objects.nonNull(denied)) {
            return denied;
        }
        try {
            return ShenyuAdminResult.success(service.apply(request));
        } catch (IllegalArgumentException error) {
            // Invalid selections, stale revisions and conflicting plans are client request errors.
            LOG.warn("Failed to apply agent gateway OpenAPI import", error);
            return ShenyuAdminResult.error(HttpStatus.BAD_REQUEST.value(), "Import failed: " + error.getMessage());
        } catch (Exception error) {
            LOG.error("Failed to apply agent gateway OpenAPI import", error);
            return ShenyuAdminResult.error("Import failed: " + error.getMessage());
        }
    }

    /**
     * Namespace membership check. Returns the established permission-denied result instead of
     * throwing: RestControllerAspect wraps controller exceptions into ShenyuAdminException, which
     * would bypass the UnauthorizedException handler and lose the permission error code.
     *
     * @param namespace requested namespace
     * @return null when allowed, otherwise the permission-denied result
     */
    private ShenyuAdminResult authorize(final String namespace) {
        if (!SessionUtil.isAdmin() && !namespaceUsers.listNamespaceIdByUserId(SessionUtil.visitor().getUserId()).contains(namespace)) {
            return ShenyuAdminResult.error(CommonErrorCode.TOKEN_NO_PERMISSION, ShenyuResultMessage.TOKEN_HAS_NO_PERMISSION);
        }
        return null;
    }
}
