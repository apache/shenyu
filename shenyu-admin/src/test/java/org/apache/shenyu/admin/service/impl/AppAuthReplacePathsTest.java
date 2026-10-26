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
import org.apache.shenyu.admin.model.dto.AppAuthReplacePathsRequestDTO;
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
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * ReplacePaths verification (isolated MySQL): null is rejected, an empty array really clears, non-empty replaces as a whole, duplicate paths are normalized,
 * REVOKED/foreign ownership/unknown authId/open=false are all rejected without writes, and failed writes keep the original paths.
 */
@org.junit.jupiter.api.condition.EnabledIfSystemProperty(named = "shenyu.auth.test.jdbcUrl", matches = ".+")
class AppAuthReplacePathsTest {

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

    private AppAuthResultVO createdAuth() {
        final AppAuthApplyRequestDTO request = new AppAuthApplyRequestDTO();
        request.setNamespaceId("ns-demo");
        request.setRequestId("req-paths");
        request.setAppName("demo-app");
        request.setUserId("10001");
        request.setPathList(List.of("/api/old1", "/api/old2"));
        return txService.apply("svc-a", request, "hash-p");
    }

    private AppAuthReplacePathsRequestDTO replaceRequest(final String authId, final List<String> pathList) {
        final AppAuthReplacePathsRequestDTO request = new AppAuthReplacePathsRequestDTO();
        request.setNamespaceId("ns-demo");
        request.setAuthId(authId);
        request.setPathList(pathList);
        return request;
    }

    private int pathCount() throws Exception {
        return AppAuthTestSupport.countRows(sqlSessionFactory, "auth_path", "");
    }

    @Test
    void nullPathListRejectedAndOriginalUnchanged() throws Exception {
        final AppAuthResultVO created = createdAuth();
        final AppAuthReplacePathsRequestDTO request = replaceRequest(created.getId(), null);

        assertThrows(ShenyuAdminException.class, () -> txService.replacePaths("svc-a", request));
        assertEquals(2, pathCount(), "the original paths are unchanged");
    }

    @Test
    void emptyListReallyClearsAllPaths() throws Exception {
        final AppAuthResultVO created = createdAuth();

        final AppAuthResultVO vo = txService.replacePaths("svc-a", replaceRequest(created.getId(), List.of()));

        assertEquals("CREATED", vo.getState());
        assertEquals(List.of(), vo.getPathList());
        assertEquals(0, pathCount(), "an empty array really clears");
        assertTrue(AppAuthTestSupport.TxConfig.PUBLISHED_EVENTS.size() >= 1, "clearing must also publish an UPDATE sync");
    }

    @Test
    void nonEmptyListReplacesFullyAndNormalizesDuplicates() throws Exception {
        final AppAuthResultVO created = createdAuth();

        final AppAuthResultVO vo = txService.replacePaths("svc-a",
                replaceRequest(created.getId(), List.of("/api/new1", "/api/new2", "/api/new1")));

        assertEquals(List.of("/api/new1", "/api/new2"), vo.getPathList());
        assertEquals(2, pathCount());
        try (Connection connection = AppAuthMySqlFixture.openConnection();
             Statement statement = connection.createStatement();
             var rs = statement.executeQuery("SELECT COUNT(*) FROM auth_path WHERE path LIKE '/api/old%'")) {
            rs.next();
            assertEquals(0, rs.getInt(1), "all old paths must be removed");
        }
    }

    @Test
    void revokedForeignUnknownAndOpenFalseAreAllRejectedWithoutWrites() throws Exception {
        final AppAuthResultVO created = createdAuth();

        // REVOKED is rejected
        final AppAuthRevokeRequestDTO revoke = new AppAuthRevokeRequestDTO();
        revoke.setNamespaceId("ns-demo");
        revoke.setRequestId("req-paths");
        revoke.setReason("drill");
        txService.revoke("svc-a", revoke);
        assertThrows(ShenyuAdminException.class, () ->
                txService.replacePaths("svc-a", replaceRequest(created.getId(), List.of("/api/x"))));

        resetState();
        final AppAuthResultVO second = createdAuth();
        // foreign caller is rejected
        assertThrows(ShenyuAdminException.class, () ->
                txService.replacePaths("svc-b", replaceRequest(second.getId(), List.of("/api/x"))));
        // unknown authId is rejected
        assertThrows(ShenyuAdminException.class, () ->
                txService.replacePaths("svc-a", replaceRequest("auth-nowhere", List.of("/api/x"))));
        assertEquals(2, pathCount(), "no rejection case may produce a write");

        // open=false is rejected: simulate a historically disabled auth by editing the database directly
        try (Connection connection = AppAuthMySqlFixture.openConnection();
             Statement statement = connection.createStatement()) {
            statement.execute("UPDATE app_auth SET open = 0 WHERE id = '" + second.getId() + "'");
        }
        assertThrows(ShenyuAdminException.class, () ->
                txService.replacePaths("svc-a", replaceRequest(second.getId(), List.of("/api/x"))));
        assertEquals(2, pathCount());
    }

    @Test
    void writeFailureKeepsOriginalPaths() throws Exception {
        final AppAuthResultVO created = createdAuth();
        // pathList with a null element: the batch insert violates NOT NULL -> everything rolls back and the original paths survive
        final List<String> bad = new java.util.ArrayList<>();
        bad.add("/api/ok");
        bad.add(null);

        assertThrows(Exception.class, () -> txService.replacePaths("svc-a", replaceRequest(created.getId(), bad)));
        assertEquals(2, pathCount(), "the original paths survive the failure");
    }
}
