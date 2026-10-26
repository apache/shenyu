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
import org.apache.shenyu.admin.model.dto.AppAuthResultQueryDTO;
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

/**
 * Result read-only semantics verification (isolated MySQL):
 * no record yields NOT_FOUND, REVOKED carries no credentials, CREATED returns the full state with the original credentials,
 * foreign callers see nothing, a broken association raises an invariant error, and a lost response recovers the same credentials via result.
 */
@org.junit.jupiter.api.condition.EnabledIfSystemProperty(named = "shenyu.auth.test.jdbcUrl", matches = ".+")
class AppAuthResultTest {

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
        request.setRequestId("req-result");
        request.setAppName("demo-app");
        request.setUserId("10001");
        request.setPathList(List.of("/api/a", "/api/b"));
        return request;
    }

    private AppAuthResultQueryDTO query() {
        final AppAuthResultQueryDTO query = new AppAuthResultQueryDTO();
        query.setNamespaceId("ns-demo");
        query.setRequestId("req-result");
        return query;
    }

    @Test
    void noRecordReturnsNotFoundWithoutMutation() throws Exception {
        final AppAuthResultVO vo = txService.result("svc-a", query());

        assertEquals("NOT_FOUND", vo.getState());
        assertNull(vo.getAppKey());
        assertNull(vo.getAppSecret());
        assertEquals(0, AppAuthTestSupport.countRows(sqlSessionFactory, "app_auth_request", ""));
        assertEquals(0, AppAuthTestSupport.countRows(sqlSessionFactory, "app_auth", ""));
        assertEquals(0, AppAuthTestSupport.countRows(sqlSessionFactory, "auth_param", ""));
        assertEquals(0, AppAuthTestSupport.countRows(sqlSessionFactory, "auth_path", ""));
    }

    @Test
    void revokedRecordReturnsNoCredentials() throws Exception {
        final AppAuthResultVO created = txService.apply("svc-a", applyRequest(), "hash-r");
        final AppAuthRequestDO record = requestMapper.selectByKey("svc-a", "ns-demo", "req-result");
        requestMapper.markRevoked(record.getId(), "drill-revoke");

        final AppAuthResultVO vo = txService.result("svc-a", query());

        assertEquals("REVOKED", vo.getState());
        assertNull(vo.getAppKey());
        assertNull(vo.getAppSecret());
        assertNull(vo.getId());
        // after revocation late applies are also rejected; credentials must not resurrect
        assertThrows(ShenyuAdminException.class, () -> txService.apply("svc-a", applyRequest(), "hash-r"));
        assertEquals(1, AppAuthTestSupport.countRows(sqlSessionFactory, "app_auth", ""));
    }

    @Test
    void createdRecordReturnsFullStateAndOriginalCredentials() throws Exception {
        final AppAuthResultVO created = txService.apply("svc-a", applyRequest(), "hash-r");

        final AppAuthResultVO vo = txService.result("svc-a", query());

        assertEquals("CREATED", vo.getState());
        assertEquals(created.getId(), vo.getId());
        assertEquals(created.getAppKey(), vo.getAppKey());
        assertEquals(created.getAppSecret(), vo.getAppSecret());
        assertEquals(Boolean.FALSE, vo.getEnabled());
        assertEquals(Boolean.TRUE, vo.getOpen());
        assertEquals(List.of("/api/a", "/api/b"), vo.getPathList());
    }

    @Test
    void otherCallerCannotReadSameRequestId() throws Exception {
        txService.apply("svc-a", applyRequest(), "hash-r");

        final AppAuthResultVO vo = txService.result("svc-b", query());

        assertEquals("NOT_FOUND", vo.getState());
        assertNull(vo.getAppSecret());
    }

    @Test
    void missingAuthRecordIsInvariantError() throws Exception {
        final AppAuthRequestDO ghost = new AppAuthRequestDO();
        ghost.setId("ghost-1");
        ghost.setCallerKey("svc-a");
        ghost.setNamespaceId("ns-demo");
        ghost.setRequestId("req-result");
        ghost.setState("CREATED");
        requestMapper.reserveIfAbsent(ghost);
        requestMapper.completeCreated("ghost-1", "auth-nowhere");

        assertThrows(ShenyuAdminException.class, () -> txService.result("svc-a", query()));
    }

    @Test
    void responseLossRecoveredByResultWithSameCredentials() throws Exception {
        final AppAuthResultVO created = txService.apply("svc-a", applyRequest(), "hash-r");
        // simulate a lost response: drop created and recover solely by requestId via result

        final AppAuthResultVO recovered = txService.result("svc-a", query());

        assertEquals(created.getId(), recovered.getId());
        assertEquals(created.getAppKey(), recovered.getAppKey());
        assertEquals(created.getAppSecret(), recovered.getAppSecret());
        assertEquals(1, AppAuthTestSupport.countRows(sqlSessionFactory, "app_auth", ""), "recovery must not create duplicates");
        assertEquals(1, AppAuthTestSupport.countRows(sqlSessionFactory, "app_auth_request", ""));
    }
}
