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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.mapper.DataPermissionMapper;
import org.apache.shenyu.admin.service.DataPermissionChecker;
import org.apache.shenyu.admin.service.NamespaceUserService;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shiro.authz.UnauthorizedException;
import org.springframework.stereotype.Service;

import java.util.Objects;

/**
 * Default implementation of {@link DataPermissionChecker}.
 */
@Service
public class DataPermissionCheckerImpl implements DataPermissionChecker {

    private static final String ANONYMOUS_USER_ID = "-1";

    private static final String ANONYMOUS_USER_NAME = "unknown";

    private final DataPermissionMapper dataPermissionMapper;

    private final NamespaceUserService namespaceUserService;

    public DataPermissionCheckerImpl(final DataPermissionMapper dataPermissionMapper,
                                     final NamespaceUserService namespaceUserService) {
        this.dataPermissionMapper = dataPermissionMapper;
        this.namespaceUserService = namespaceUserService;
    }

    @Override
    public void requireSelectorReadable(final String selectorId, final String namespaceId) {
        requireReadable(selectorId, namespaceId, AdminConstants.SELECTOR_DATA_TYPE);
    }

    @Override
    public void requireRuleReadable(final String ruleId, final String namespaceId) {
        requireReadable(ruleId, namespaceId, AdminConstants.RULE_DATA_TYPE);
    }

    private void requireReadable(final String dataId, final String namespaceId, final Integer dataType) {
        if (SessionUtil.isAdmin()) {
            return;
        }
        final String userId = SessionUtil.visitorId();
        if (StringUtils.isBlank(userId) || ANONYMOUS_USER_ID.equals(userId) || ANONYMOUS_USER_NAME.equals(userId)) {
            throw new UnauthorizedException("Data permission check failed: the caller is not an authenticated dashboard user");
        }
        final boolean belongsToNamespace = StringUtils.isNotBlank(namespaceId)
                && namespaceUserService.listNamespaceIdByUserId(userId).contains(namespaceId);
        if (!belongsToNamespace || Objects.isNull(dataPermissionMapper.findOneByUniqueKey(dataId, userId, dataType))) {
            throw new UnauthorizedException("Data permission check failed: the resource is not accessible to the caller");
        }
    }
}
