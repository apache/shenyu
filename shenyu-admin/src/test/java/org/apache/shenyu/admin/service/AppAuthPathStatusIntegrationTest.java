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

package org.apache.shenyu.admin.service;

import jakarta.annotation.Resource;
import org.apache.shenyu.admin.AbstractSpringIntegrationTest;
import org.apache.shenyu.admin.mapper.AppAuthMapper;
import org.apache.shenyu.admin.mapper.AuthPathMapper;
import org.apache.shenyu.admin.model.dto.AuthApplyDTO;
import org.apache.shenyu.admin.model.dto.AuthPathDTO;
import org.apache.shenyu.admin.model.dto.AuthPathWarpDTO;
import org.apache.shenyu.admin.model.entity.AppAuthDO;
import org.apache.shenyu.admin.model.entity.AuthPathDO;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.junit.jupiter.api.Test;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Verifies resource replacement states against the Admin database.
 */
@Transactional
public final class AppAuthPathStatusIntegrationTest extends AbstractSpringIntegrationTest {

    @Resource
    private AppAuthService appAuthService;

    @Resource
    private AppAuthMapper appAuthMapper;

    @Resource
    private AuthPathMapper authPathMapper;

    @Test
    public void testReplacementPersistsDisabledPathsAndEnablesNewGrants() {
        AppAuthDO auth = createAuth();
        AuthPathDO disabled = AuthPathDO.create("/retained-disabled", auth.getId(), "orders");
        disabled.setEnabled(false);
        authPathMapper.save(disabled);
        AuthPathWarpDTO update = new AuthPathWarpDTO();
        update.setId(auth.getId());
        update.setAuthPathDTOList(List.of(requestedPath("/retained-disabled", true), requestedPath("/new-grant", false)));

        assertEquals(CommonErrorCode.SUCCESSFUL, appAuthService.updateDetailPath(update).getCode().intValue());

        Map<String, Boolean> stored = authPathMapper.findByAuthId(auth.getId()).stream()
                .collect(Collectors.toMap(AuthPathDO::getPath, AuthPathDO::getEnabled));
        assertEquals(Map.of("/retained-disabled", false, "/new-grant", true), stored);
    }

    @Test
    public void testDisabledPathFromAnotherApplicationDoesNotDisableNewGrant() {
        final AppAuthDO first = createAuth();
        final AppAuthDO second = createAuth();
        AuthPathDO disabled = AuthPathDO.create("/shared", first.getId(), "orders");
        disabled.setEnabled(false);
        authPathMapper.save(disabled);
        AuthPathWarpDTO update = new AuthPathWarpDTO();
        update.setId(second.getId());
        update.setAuthPathDTOList(List.of(requestedPath("/shared", false)));

        assertEquals(CommonErrorCode.SUCCESSFUL, appAuthService.updateDetailPath(update).getCode().intValue());

        assertEquals(Boolean.FALSE, authPathMapper.findByAuthId(first.getId()).get(0).getEnabled());
        assertEquals(Boolean.TRUE, authPathMapper.findByAuthId(second.getId()).get(0).getEnabled());
    }

    private AppAuthDO createAuth() {
        AuthApplyDTO apply = new AuthApplyDTO();
        apply.setUserId("test-user");
        apply.setPhone("00000000000");
        apply.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        apply.setOpen(true);
        AppAuthDO auth = AppAuthDO.create(apply);
        assertEquals(1, appAuthMapper.insert(auth));
        return auth;
    }

    private AuthPathDTO requestedPath(final String path, final boolean enabled) {
        AuthPathDTO dto = new AuthPathDTO();
        dto.setAppName("orders");
        dto.setPath(path);
        dto.setEnabled(enabled);
        return dto;
    }
}
