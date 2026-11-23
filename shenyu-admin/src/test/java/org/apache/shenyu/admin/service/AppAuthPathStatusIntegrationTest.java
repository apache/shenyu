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
import org.apache.shenyu.admin.listener.DataChangedListener;
import org.apache.shenyu.admin.mapper.AppAuthMapper;
import org.apache.shenyu.admin.mapper.AuthParamMapper;
import org.apache.shenyu.admin.mapper.AuthPathMapper;
import org.apache.shenyu.admin.model.dto.AppAuthDTO;
import org.apache.shenyu.admin.model.dto.AuthApplyDTO;
import org.apache.shenyu.admin.model.dto.AuthPathDTO;
import org.apache.shenyu.admin.model.dto.AuthPathWarpDTO;
import org.apache.shenyu.admin.model.entity.AppAuthDO;
import org.apache.shenyu.admin.model.entity.AuthPathDO;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.ArgumentCaptor;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.support.TransactionTemplate;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.clearInvocations;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * Verifies persisted resource states and commit-scoped gateway delivery against the Admin database.
 */
public final class AppAuthPathStatusIntegrationTest extends AbstractSpringIntegrationTest {

    @Resource
    private AppAuthService appAuthService;

    @Resource
    private AppAuthMapper appAuthMapper;

    @Resource
    private AuthPathMapper authPathMapper;

    @Resource
    private AuthParamMapper authParamMapper;

    @Resource
    private PlatformTransactionManager transactionManager;

    @MockBean(name = "authorizationPathStateListener")
    private DataChangedListener listener;

    private final List<String> authIds = new ArrayList<>();

    @BeforeEach
    public void clearListener() {
        clearInvocations(listener);
    }

    @AfterEach
    public void cleanup() {
        for (String authId : authIds) {
            authPathMapper.deleteByAuthId(authId);
            authParamMapper.deleteByAuthId(authId);
            appAuthMapper.delete(authId);
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"path", "detail", "apply"})
    public void testReplacementPersistsDisabledPathsAndEnablesNewGrants(final String endpoint) {
        AppAuthDO auth = createAuth();
        saveDisabledPath(auth, "/retained-disabled");

        replacePaths(endpoint, auth, List.of(requestedPath("/retained-disabled", true), requestedPath("/new-grant", false)));

        assertEquals(Map.of("/retained-disabled", false, "/new-grant", true), persistedStates(auth));
    }

    @ParameterizedTest
    @ValueSource(strings = {"path", "detail", "apply"})
    public void testDisabledPathFromAnotherAuthorizationDoesNotDisableNewGrant(final String endpoint) {
        final AppAuthDO first = createAuth();
        final AppAuthDO second = createAuth();
        saveDisabledPath(first, "/shared");

        replacePaths(endpoint, second, List.of(requestedPath("/shared", false)));

        assertEquals(Map.of("/shared", false), persistedStates(first));
        assertEquals(Map.of("/shared", true), persistedStates(second));
    }

    @ParameterizedTest
    @ValueSource(strings = {"path", "detail", "apply"})
    public void testRollbackRestoresPersistedPathsAndDoesNotDeliver(final String endpoint) {
        final AppAuthDO auth = createAuth();
        saveDisabledPath(auth, "/retained-disabled");

        new TransactionTemplate(transactionManager).executeWithoutResult(status -> {
            replacePaths(endpoint, auth, List.of(requestedPath("/retained-disabled", true), requestedPath("/new-grant", false)));
            assertEquals(Map.of("/retained-disabled", false, "/new-grant", true), persistedStates(auth));
            verifyNoInteractions(listener);
            status.setRollbackOnly();
        });

        assertEquals(Map.of("/retained-disabled", false), persistedStates(auth));
        verifyNoInteractions(listener);
    }

    @ParameterizedTest
    @ValueSource(strings = {"path", "detail", "apply"})
    public void testCommittedReplacementDeliversStoredStatesOnlyAfterCommit(final String endpoint) {
        final AppAuthDO auth = createAuth();
        saveDisabledPath(auth, "/retained-disabled");

        new TransactionTemplate(transactionManager).executeWithoutResult(status -> {
            replacePaths(endpoint, auth, List.of(requestedPath("/retained-disabled", true), requestedPath("/new-grant", false)));
            verifyNoInteractions(listener);
        });

        ArgumentCaptor<List<AppAuthData>> events = ArgumentCaptor.forClass(List.class);
        verify(listener).onAppAuthChanged(events.capture(), any(DataEventTypeEnum.class), isNull());
        AppAuthData data = events.getValue().get(0);
        assertEquals(auth.getAppKey(), data.getAppKey());
        assertEquals(auth.getNamespaceId(), data.getNamespaceId());
        assertEquals(Map.of("/retained-disabled", false, "/new-grant", true), data.getPathDataList().stream()
                .collect(Collectors.toMap(path -> path.getPath(), path -> path.getEnabled())));
        assertEquals(Map.of("/retained-disabled", false, "/new-grant", true), persistedStates(auth));
    }

    private Map<String, Boolean> persistedStates(final AppAuthDO auth) {
        return authPathMapper.findByAuthId(auth.getId()).stream().collect(Collectors.toMap(AuthPathDO::getPath, AuthPathDO::getEnabled));
    }

    private void saveDisabledPath(final AppAuthDO auth, final String path) {
        AuthPathDO disabled = AuthPathDO.create(path, auth.getId(), "orders");
        disabled.setEnabled(false);
        assertEquals(1, authPathMapper.save(disabled));
    }

    private void replacePaths(final String endpoint, final AppAuthDO auth, final List<AuthPathDTO> paths) {
        ShenyuAdminResult result;
        if ("path".equals(endpoint)) {
            AuthPathWarpDTO update = new AuthPathWarpDTO();
            update.setId(auth.getId());
            update.setAuthPathDTOList(paths);
            result = appAuthService.updateDetailPath(update);
        } else if ("detail".equals(endpoint)) {
            AppAuthDTO update = new AppAuthDTO();
            update.setId(auth.getId());
            update.setAppKey(auth.getAppKey());
            update.setNamespaceId(auth.getNamespaceId());
            update.setAuthPathList(paths);
            result = appAuthService.updateDetail(update);
        } else {
            AuthApplyDTO update = new AuthApplyDTO();
            update.setAppKey(auth.getAppKey());
            update.setAppName("orders");
            update.setOpen(true);
            update.setPathList(paths.stream().map(AuthPathDTO::getPath).collect(Collectors.toList()));
            result = appAuthService.applyUpdate(update);
        }
        assertEquals(CommonErrorCode.SUCCESSFUL, result.getCode().intValue());
    }

    private AppAuthDO createAuth() {
        AuthApplyDTO apply = new AuthApplyDTO();
        apply.setUserId("test-user");
        apply.setPhone("00000000000");
        apply.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        apply.setOpen(true);
        AppAuthDO auth = AppAuthDO.create(apply);
        assertEquals(1, appAuthMapper.insert(auth));
        authIds.add(auth.getId());
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
