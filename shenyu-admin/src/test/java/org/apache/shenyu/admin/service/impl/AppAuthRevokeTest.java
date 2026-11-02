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
import org.apache.shenyu.admin.mapper.AppAuthRequestMapper;
import org.apache.shenyu.authtest.AppAuthTestSupport;
import org.apache.shenyu.admin.model.dto.AppAuthApplyRequestDTO;
import org.apache.shenyu.admin.model.dto.AppAuthRevokeRequestDTO;
import org.apache.shenyu.admin.model.entity.AppAuthRequestDO;
import org.apache.shenyu.admin.model.vo.AppAuthResultVO;
import org.apache.ibatis.session.SqlSessionFactory;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import java.sql.Connection;
import java.sql.Statement;
import java.util.List;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Revoke verification (isolated MySQL): revoking before creation leaves a tombstone blocking late applies, repeated revokes are idempotent,
 * a created auth is deleted with its child rows and marked in the same transaction, delete failures roll everything back, and a foreign caller cannot revoke someone else's auth.
 */
@org.junit.jupiter.api.condition.EnabledIfSystemProperty(named = "shenyu.auth.test.jdbcUrl", matches = ".+")
class AppAuthRevokeTest {

    private static AnnotationConfigApplicationContext context;

    private static AppAuthTransactionService txService;

    private static AppAuthRequestMapper requestMapper;

    private static SqlSessionFactory sqlSessionFactory;

    @BeforeAll
    static void init() throws Exception {
        AppAuthTestSupport.recreateTables();
        context = AppAuthTestSupport.openContext();
        txService = context.getBean(AppAuthTransactionService.class);
        requestMapper = context.getBean(AppAuthRequestMapper.class);
        sqlSessionFactory = context.getBean(SqlSessionFactory.class);
    }

    @AfterAll
    static void close() {
        if (Objects.nonNull(context)) {
            context.close();
        }
    }

    @BeforeEach
    void resetState() throws Exception {
        try (Connection connection = AppAuthMySqlFixture.openConnection();
             Statement statement = connection.createStatement()) {
            AppAuthMySqlFixture.verifyTestDatabaseIdentity(connection);
            statement.execute("DELETE FROM app_auth_request");
            statement.execute("DELETE FROM auth_path");
            statement.execute("DELETE FROM auth_param");
            statement.execute("DELETE FROM app_auth");
        }
    }

    private AppAuthApplyRequestDTO applyRequest() {
        final AppAuthApplyRequestDTO request = new AppAuthApplyRequestDTO();
        request.setNamespaceId("ns-demo");
        request.setRequestId("req-revoke");
        request.setAppName("demo-app");
        request.setUserId("10001");
        request.setPathList(List.of("/api/a"));
        return request;
    }

    private AppAuthRevokeRequestDTO revokeRequest() {
        final AppAuthRevokeRequestDTO request = new AppAuthRevokeRequestDTO();
        request.setNamespaceId("ns-demo");
        request.setRequestId("req-revoke");
        request.setReason("revoke drill");
        return request;
    }

    @Test
    void revokeBeforeCreateLeavesTombstoneAndBlocksLateApply() throws Exception {
        final AppAuthResultVO vo = txService.revoke("svc-a", revokeRequest());

        assertEquals("REVOKED", vo.getState());
        assertNull(vo.getAppSecret());
        assertEquals(1, AppAuthTestSupport.countRows(sqlSessionFactory, "app_auth_request", ""));
        assertEquals(0, AppAuthTestSupport.countRows(sqlSessionFactory, "app_auth", ""));

        assertThrows(ShenyuAdminException.class, () -> txService.apply("svc-a", applyRequest(), "hash-late"));
        assertEquals(0, AppAuthTestSupport.countRows(sqlSessionFactory, "app_auth", ""), "a late apply must not resurrect");
    }

    @Test
    void repeatedRevokeIsIdempotent() throws Exception {
        assertEquals("REVOKED", txService.revoke("svc-a", revokeRequest()).getState());
        assertEquals("REVOKED", txService.revoke("svc-a", revokeRequest()).getState());
        assertEquals(1, AppAuthTestSupport.countRows(sqlSessionFactory, "app_auth_request", ""));
    }

    @Test
    void revokeAfterCreateDeletesAuthAndChildrenAndKeepsHistory() throws Exception {
        final AppAuthResultVO created = txService.apply("svc-a", applyRequest(), "hash-r");
        assertEquals(1, AppAuthTestSupport.countRows(sqlSessionFactory, "app_auth", ""));

        assertEquals("REVOKED", txService.revoke("svc-a", revokeRequest()).getState());

        assertEquals(0, AppAuthTestSupport.countRows(sqlSessionFactory, "app_auth", ""));
        assertEquals(0, AppAuthTestSupport.countRows(sqlSessionFactory, "auth_param", ""));
        assertEquals(0, AppAuthTestSupport.countRows(sqlSessionFactory, "auth_path", ""));
        final AppAuthRequestDO record = requestMapper.selectByKey("svc-a", "ns-demo", "req-revoke");
        assertEquals("REVOKED", record.getState());
        assertEquals(created.getId(), record.getAuthId(), "revocation keeps the historical authId");
        assertEquals("hash-r", record.getRequestHash());
    }

    @Test
    void deletionFailureRollsBackMarkerToo() throws Exception {
        // a CREATED record pointing to a missing auth: the delete chain fails -> everything rolls back and the marking never lands first
        final AppAuthRequestDO ghost = new AppAuthRequestDO();
        ghost.setId("ghost-r");
        ghost.setCallerKey("svc-a");
        ghost.setNamespaceId("ns-demo");
        ghost.setRequestId("req-revoke");
        ghost.setState("CREATED");
        requestMapper.reserveIfAbsent(ghost);
        requestMapper.completeCreated("ghost-r", "auth-nowhere", "CREATED");

        assertThrows(ShenyuAdminException.class, () -> txService.revoke("svc-a", revokeRequest()));
        final AppAuthRequestDO record = requestMapper.selectByKey("svc-a", "ns-demo", "req-revoke");
        assertTrue(Objects.isNull(record) || "CREATED".equals(record.getState()), "a failed revoke must not rewrite the original state");
    }

    @Test
    void otherCallerCannotRevokeForeignAuth() throws Exception {
        txService.apply("svc-a", applyRequest(), "hash-r");

        final AppAuthResultVO vo = txService.revoke("svc-b", revokeRequest());

        assertEquals("REVOKED", vo.getState());
        // svc-a's auth is unaffected by svc-b's tombstone
        assertEquals(1, AppAuthTestSupport.countRows(sqlSessionFactory, "app_auth", ""));
        final AppAuthRequestDO foreign = requestMapper.selectByKey("svc-a", "ns-demo", "req-revoke");
        assertEquals("CREATED", foreign.getState());
    }
}
