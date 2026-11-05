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
import org.apache.shenyu.admin.model.entity.AppAuthRequestDO;
import org.apache.shenyu.admin.model.vo.AppAuthResultVO;
import org.apache.ibatis.session.SqlSessionFactory;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.List;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Verify the apply transaction algorithm (isolated MySQL):
 * same-key same-parameter replay returns the same credentials, same-key different-parameter is rejected, REVOKED is rejected,
 * open=true/enabled=false are fixed, child-row failures roll back everything, sync events fire only after commit, and the canonical digest matches the shared vector.
 */
@org.junit.jupiter.api.condition.EnabledIfSystemProperty(named = "shenyu.auth.test.jdbcUrl", matches = ".+")
class AppAuthApplyTest {

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
        AppAuthTestSupport.TxConfig.PUBLISHED_EVENTS.clear();
        try (Connection connection = AppAuthMySqlFixture.openConnection();
             Statement statement = connection.createStatement()) {
            AppAuthMySqlFixture.verifyTestDatabaseIdentity(connection);
            statement.execute("DELETE FROM app_auth_request");
            statement.execute("DELETE FROM auth_path");
            statement.execute("DELETE FROM auth_param");
            statement.execute("DELETE FROM app_auth");
        }
    }

    private AppAuthApplyRequestDTO applyRequest(final String requestId, final String appName, final List<String> pathList) {
        final AppAuthApplyRequestDTO request = new AppAuthApplyRequestDTO();
        request.setNamespaceId("ns-demo");
        request.setRequestId(requestId);
        request.setAppName(appName);
        request.setUserId("10001");
        request.setPathList(pathList);
        return request;
    }

    @Test
    void sameKeySameParamsReplayReturnsSameCredentials() throws Exception {
        final AppAuthApplyRequestDTO request = applyRequest("req-replay", "demo-app", List.of("/api/a", "/api/b"));

        final AppAuthResultVO first = txService.apply("svc-a", request, "hash-replay");
        final AppAuthResultVO second = txService.apply("svc-a", request, "hash-replay");

        assertEquals(first.getId(), second.getId());
        assertEquals(first.getAppKey(), second.getAppKey());
        assertEquals(first.getAppSecret(), second.getAppSecret());
        // no orphans in the table: one apply lands exactly one set of rows
        assertEquals(1, AppAuthTestSupport.countRows(sqlSessionFactory, "app_auth_request", ""));
        assertEquals(1, AppAuthTestSupport.countRows(sqlSessionFactory, "app_auth", ""));
        assertEquals(1, AppAuthTestSupport.countRows(sqlSessionFactory, "auth_param", ""));
        assertEquals(2, AppAuthTestSupport.countRows(sqlSessionFactory, "auth_path", ""));
        // exactly one post-commit event (the method returned, so afterCommit is visible)
        assertEquals(1, AppAuthTestSupport.TxConfig.PUBLISHED_EVENTS.size());
        assertNotNull(AppAuthTestSupport.TxConfig.PUBLISHED_EVENTS.get(0));
    }

    @Test
    void sameKeyDifferentParamsRejectedWithoutMutation() throws Exception {
        final AppAuthResultVO first = txService.apply("svc-a",
                applyRequest("req-conflict", "demo-app", List.of("/api/a")), "hash-1");
        final ShenyuAdminException ex = assertThrows(ShenyuAdminException.class, () ->
                txService.apply("svc-a", applyRequest("req-conflict", "other-app", List.of("/api/a")), "hash-2"));
        assertTrue(ex.getMessage().contains("different parameters"));

        assertEquals(1, AppAuthTestSupport.countRows(sqlSessionFactory, "app_auth", ""));
        final AppAuthRequestDO record = requestMapper.selectByKey("svc-a", "ns-demo", "req-conflict");
        assertEquals("hash-1", record.getRequestHash());
        assertEquals(first.getId(), record.getAuthId());
    }

    @Test
    void revokedKeyRejectsLateApply() throws Exception {
        final AppAuthRequestDO tombstone = new AppAuthRequestDO();
        tombstone.setId("ts-1");
        tombstone.setCallerKey("svc-a");
        tombstone.setNamespaceId("ns-demo");
        tombstone.setRequestId("req-dead");
        tombstone.setState("REVOKED");
        requestMapper.reserveIfAbsent(tombstone);
        requestMapper.markRevoked("ts-1", "drill", "REVOKED");

        assertThrows(ShenyuAdminException.class, () ->
                txService.apply("svc-a", applyRequest("req-dead", "demo-app", List.of("/api/a")), "hash-x"));
        assertEquals(0, AppAuthTestSupport.countRows(sqlSessionFactory, "app_auth", ""));
        assertEquals(0, AppAuthTestSupport.countRows(sqlSessionFactory, "auth_path", ""));
    }

    @Test
    void firstCreationIsFixedOpenAndDisabled() throws Exception {
        final AppAuthResultVO vo = txService.apply("svc-a",
                applyRequest("req-flags", "demo-app", List.of("/api/a")), "hash-flags");

        assertEquals("CREATED", vo.getState());
        assertEquals(Boolean.TRUE, vo.getOpen());
        assertEquals(Boolean.FALSE, vo.getEnabled());

        try (Connection connection = AppAuthMySqlFixture.openConnection();
             Statement statement = connection.createStatement();
             ResultSet rs = statement.executeQuery(
                     "SELECT open, enabled FROM app_auth WHERE id = '" + vo.getId() + "'")) {
            assertTrue(rs.next());
            assertTrue(rs.getBoolean(1));
            assertFalse(rs.getBoolean(2));
        }
    }

    @Test
    void paramWriteFailureRollsBackEverything() throws Exception {
        // appName null: the auth_param.app_name NOT NULL constraint fails -> association, auth and paths all roll back
        assertThrows(Exception.class, () ->
                txService.apply("svc-a", applyRequest("req-fail", null, List.of("/api/a")), "hash-fail"));

        assertEquals(0, AppAuthTestSupport.countRows(sqlSessionFactory, "app_auth_request", ""));
        assertEquals(0, AppAuthTestSupport.countRows(sqlSessionFactory, "app_auth", ""));
        assertEquals(0, AppAuthTestSupport.countRows(sqlSessionFactory, "auth_param", ""));
        assertEquals(0, AppAuthTestSupport.countRows(sqlSessionFactory, "auth_path", ""));
        assertEquals(0, AppAuthTestSupport.TxConfig.PUBLISHED_EVENTS.size(), "no event may fire after rollback");
    }

    @Test
    void canonicalHashMatchesSharedVector() throws Exception {
        // matches the frozen vector: appName with whitespace and a shuffled duplicated pathList yield the same digest
        final AppAuthApplyRequestDTO request = new AppAuthApplyRequestDTO();
        request.setNamespaceId("ns-demo");
        request.setRequestId("req-ignored-by-hash");
        request.setAppName("  demo-app  ");
        request.setUserId("10001");
        request.setPathList(List.of("/api/b", "/api/a", "/api/b"));

        final String expected = Files.readString(
                Paths.get("src/test/resources/app-auth/canonical-request.sha256"),
                StandardCharsets.UTF_8).trim();
        // same-package direct call of the package-private static method
        assertEquals(expected, AppAuthLifecycleServiceImpl.canonicalRequestHash(request));
    }
}
