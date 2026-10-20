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

package org.apache.shenyu.admin.service.impl;

import org.apache.shenyu.admin.config.AppAuthLifecycleProperties;
import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.mapper.AppAuthMapper;
import org.apache.shenyu.admin.mapper.AppAuthRequestMapper;
import org.apache.shenyu.admin.model.entity.AppAuthDO;
import org.apache.shenyu.admin.model.entity.AppAuthRequestDO;
import org.apache.shenyu.admin.service.AppAuthAccessPolicy;
import org.apache.shenyu.admin.service.NamespaceUserService;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.apache.shiro.SecurityUtils;
import org.apache.shiro.UnavailableSecurityManagerException;
import org.springframework.stereotype.Service;

import java.util.Objects;

/**
 * AppAuthAccessPolicy impl.
 * Triple check: service-account whitelist (immutable userId) -> Shiro action permission -> namespace ownership.
 * Default users (-1/unknown), an empty whitelist and a missing Shiro context are always rejected.
 */
@Service
public class AppAuthAccessPolicyImpl implements AppAuthAccessPolicy {

    private final AppAuthLifecycleProperties properties;

    private final NamespaceUserService namespaceUserService;

    private final AppAuthRequestMapper requestMapper;

    private final AppAuthMapper appAuthMapper;

    public AppAuthAccessPolicyImpl(final AppAuthLifecycleProperties properties,
                                   final NamespaceUserService namespaceUserService,
                                   final AppAuthRequestMapper requestMapper,
                                   final AppAuthMapper appAuthMapper) {
        this.properties = properties;
        this.namespaceUserService = namespaceUserService;
        this.requestMapper = requestMapper;
        this.appAuthMapper = appAuthMapper;
    }

    @Override
    public String requireServiceCaller(final String permission) {
        final String userId = SessionUtil.visitorId();
        if (Objects.isNull(userId) || "-1".equals(userId) || "unknown".equals(userId)) {
            throw new ShenyuAdminException("Service identity check failed: call with a configured service account");
        }
        if (!properties.getServiceUserIds().contains(userId)) {
            throw new ShenyuAdminException("Service identity check failed: current account is not in the service whitelist");
        }
        if (!isPermitted(permission)) {
            throw new ShenyuAdminException("Service identity check failed: missing action permission " + permission);
        }
        // callerKey is the immutable userId: renaming the user never changes the identity key
        return userId;
    }

    @Override
    public void requireNamespace(final String callerKey, final String namespaceId) {
        if (!namespaceUserService.listNamespaceIdByUserId(callerKey).contains(namespaceId)) {
            throw new ShenyuAdminException("Namespace ownership check failed");
        }
    }

    @Override
    public AppAuthRequestDO requireOwnedAuth(final String callerKey, final String namespaceId, final String authId) {
        final AppAuthRequestDO record = requestMapper.selectByAuthId(authId);
        if (Objects.isNull(record)
                || !callerKey.equals(record.getCallerKey())
                || !namespaceId.equals(record.getNamespaceId())
                || !"CREATED".equals(record.getState())) {
            throw new ShenyuAdminException("App auth ownership check failed");
        }
        final AppAuthDO auth = appAuthMapper.selectById(authId);
        if (Objects.isNull(auth) || !namespaceId.equals(auth.getNamespaceId())) {
            throw new ShenyuAdminException("App auth ownership check failed");
        }
        return record;
    }

    private boolean isPermitted(final String permission) {
        try {
            return SecurityUtils.getSubject().isPermitted(permission);
        } catch (UnavailableSecurityManagerException | IllegalStateException e) {
            // reject whenever no Shiro context exists (unexpected call path or direct invocation)
            return false;
        }
    }
}
