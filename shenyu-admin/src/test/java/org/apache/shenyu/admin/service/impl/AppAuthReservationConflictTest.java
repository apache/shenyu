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

import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.mapper.AppAuthMapper;
import org.apache.shenyu.admin.mapper.AppAuthRequestMapper;
import org.apache.shenyu.admin.mapper.AuthParamMapper;
import org.apache.shenyu.admin.mapper.AuthPathMapper;
import org.apache.shenyu.admin.model.dto.AppAuthApplyRequestDTO;
import org.apache.shenyu.admin.model.dto.AppAuthRevokeRequestDTO;
import org.apache.shenyu.admin.model.entity.AppAuthDO;
import org.apache.shenyu.admin.model.entity.AppAuthRequestDO;
import org.apache.shenyu.admin.model.vo.AppAuthResultVO;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.dao.DuplicateKeyException;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * A lost reservation insert (Oracle ORA-00001) falls through to the locked row.
 * H2 does not raise that error for MERGE or ON CONFLICT, so the overlap is the exception the database would throw.
 */
class AppAuthReservationConflictTest {

    private AppAuthRequestMapper requestMapper;

    private AppAuthMapper appAuthMapper;

    private AuthPathMapper authPathMapper;

    private ApplicationEventPublisher eventPublisher;

    private AppAuthTransactionService service;

    @BeforeEach
    void setUp() {
        requestMapper = mock(AppAuthRequestMapper.class);
        appAuthMapper = mock(AppAuthMapper.class);
        authPathMapper = mock(AuthPathMapper.class);
        eventPublisher = mock(ApplicationEventPublisher.class);
        final AuthParamMapper authParamMapper = mock(AuthParamMapper.class);
        service = new AppAuthTransactionService(requestMapper, appAuthMapper, authParamMapper, authPathMapper, eventPublisher);
    }

    @Test
    void sameKeyApplyOverlapReturnsTheWinningAuth() {
        when(requestMapper.reserveIfAbsent(any())).thenThrow(new DuplicateKeyException("ORA-00001"));
        when(requestMapper.selectByKeyForUpdate("caller", "ns", "req")).thenReturn(locked("winner", "CREATED", "hash", "auth-1"));
        when(appAuthMapper.selectById("auth-1")).thenReturn(AppAuthDO.builder()
                .id("auth-1").appKey("key").appSecret("secret").enabled(false).open(true).namespaceId("ns").build());
        when(authPathMapper.findByAuthId("auth-1")).thenReturn(List.of());

        final AppAuthResultVO result = service.apply("caller", applyRequest(), "hash");

        assertEquals("auth-1", result.getId());
        assertEquals("key", result.getAppKey());
        verify(appAuthMapper, never()).insert(any());
        verify(eventPublisher, never()).publishEvent(any());
    }

    @Test
    void sameKeyApplyOverlapWithDifferentHashIsRejected() {
        when(requestMapper.reserveIfAbsent(any())).thenThrow(new DuplicateKeyException("ORA-00001"));
        when(requestMapper.selectByKeyForUpdate("caller", "ns", "req")).thenReturn(locked("winner", "CREATED", "other", "auth-1"));

        final ShenyuAdminException error = assertThrows(ShenyuAdminException.class,
                () -> service.apply("caller", applyRequest(), "hash"));

        assertTrue(error.getMessage().contains("different parameters"));
        verify(appAuthMapper, never()).insert(any());
    }

    @Test
    void sameKeyRevokeOverlapIsIdempotent() {
        when(requestMapper.reserveIfAbsent(any())).thenThrow(new DuplicateKeyException("ORA-00001"));
        when(requestMapper.selectByKeyForUpdate("caller", "ns", "req")).thenReturn(locked("winner", "REVOKED", null, "auth-1"));

        final AppAuthResultVO result = service.revoke("caller", revokeRequest());

        assertEquals("REVOKED", result.getState());
        verify(appAuthMapper, never()).deleteByIds(any());
        verify(eventPublisher, never()).publishEvent(any());
    }

    @Test
    void otherIntegrityErrorsStillPropagate() {
        when(requestMapper.reserveIfAbsent(any())).thenThrow(new DataIntegrityViolationException("not a duplicate key"));

        assertThrows(DataIntegrityViolationException.class, () -> service.apply("caller", applyRequest(), "hash"));
        verify(requestMapper, never()).selectByKeyForUpdate(anyString(), anyString(), anyString());
    }

    private AppAuthRequestDO locked(final String id, final String state, final String hash, final String authId) {
        final AppAuthRequestDO locked = new AppAuthRequestDO();
        locked.setId(id);
        locked.setState(state);
        locked.setRequestHash(hash);
        locked.setAuthId(authId);
        locked.setCallerKey("caller");
        locked.setNamespaceId("ns");
        locked.setRequestId("req");
        return locked;
    }

    private AppAuthApplyRequestDTO applyRequest() {
        final AppAuthApplyRequestDTO request = new AppAuthApplyRequestDTO();
        request.setNamespaceId("ns");
        request.setRequestId("req");
        request.setAppName("app");
        request.setPathList(List.of("/a"));
        return request;
    }

    private AppAuthRevokeRequestDTO revokeRequest() {
        final AppAuthRevokeRequestDTO request = new AppAuthRevokeRequestDTO();
        request.setNamespaceId("ns");
        request.setRequestId("req");
        request.setReason("drill");
        return request;
    }
}
