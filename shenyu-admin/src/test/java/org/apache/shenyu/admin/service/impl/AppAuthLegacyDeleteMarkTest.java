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

import java.sql.Connection;
import java.sql.Statement;
import java.util.List;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Legacy-delete linkage verification (isolated MySQL):
 * legacy delete marks the idempotency key REVOKED and blocks late applies; missing auth with a live association is still marked;
 * non-lifecycle legacy auths keep the original behaviour (no fabricated request records).
 */
@org.junit.jupiter.api.condition.EnabledIfSystemProperty(named = "shenyu.auth.test.jdbcUrl", matches = ".+")
class AppAuthLegacyDeleteMarkTest {

    private static AnnotationConfigApplicationContext context;

    private static AppAuthTransactionService txService;

    private static org.apache.shenyu.admin.service.AppAuthService appAuthService;

    private static AppAuthRequestMapper requestMapper;

    private static SqlSessionFactory sqlSessionFactory;

    @BeforeAll
    static void init() throws Exception {
        AppAuthTestSupport.recreateTables();
        context = AppAuthTestSupport.openContext();
        txService = context.getBean(AppAuthTransactionService.class);
        // AppAuthServiceImpl is served through a JDK interface proxy; look it up by the interface type
        appAuthService = context.getBean(org.apache.shenyu.admin.service.AppAuthService.class);
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

    private AppAuthResultVO applyLifecycleAuth() {
        final AppAuthApplyRequestDTO request = new AppAuthApplyRequestDTO();
        request.setNamespaceId("ns-demo");
        request.setRequestId("req-legacy-del");
        request.setAppName("demo-app");
        request.setUserId("10001");
        request.setPathList(List.of("/api/a"));
        return txService.apply("svc-a", request, "hash-ld");
    }

    @Test
    void legacyDeleteMarksLifecycleKeyRevokedAndBlocksLateApply() throws Exception {
        final AppAuthResultVO created = applyLifecycleAuth();

        final int deleted = appAuthService.delete(List.of(created.getId()));

        assertEquals(1, deleted);
        assertEquals(0, AppAuthTestSupport.countRows(sqlSessionFactory, "app_auth", ""));
        assertEquals(0, AppAuthTestSupport.countRows(sqlSessionFactory, "auth_param", ""));
        assertEquals(0, AppAuthTestSupport.countRows(sqlSessionFactory, "auth_path", ""));
        final AppAuthRequestDO record = requestMapper.selectByKey("svc-a", "ns-demo", "req-legacy-del");
        assertEquals("REVOKED", record.getState());
        assertEquals(created.getId(), record.getAuthId(), "the historical authId is kept");

        // late applies are rejected; credentials must not resurrect
        assertThrows(ShenyuAdminException.class, () -> applyLifecycleAuth());
        assertEquals(0, AppAuthTestSupport.countRows(sqlSessionFactory, "app_auth", ""));
    }

    @Test
    void missingAuthButAssociationPresentStillMarked() throws Exception {
        final AppAuthRequestDO ghost = new AppAuthRequestDO();
        ghost.setId("ghost-ld");
        ghost.setCallerKey("svc-a");
        ghost.setNamespaceId("ns-demo");
        ghost.setRequestId("req-legacy-del");
        ghost.setState("CREATED");
        requestMapper.reserveIfAbsent(ghost);
        requestMapper.completeCreated("ghost-ld", "auth-gone", "CREATED");

        // app_auth already missing: the early-return path must not skip the REVOKED marking
        final int deleted = appAuthService.delete(List.of("auth-gone"));

        assertEquals(0, deleted);
        final AppAuthRequestDO record = requestMapper.selectByKey("svc-a", "ns-demo", "req-legacy-del");
        assertNotNull(record);
        assertEquals("REVOKED", record.getState());
    }

    @Test
    void legacyNonLifecycleDeleteKeepsOriginalBehavior() throws Exception {
        // insert a legacy auth without any lifecycle association directly
        try (Connection connection = AppAuthMySqlFixture.openConnection();
             Statement statement = connection.createStatement()) {
            statement.execute("INSERT INTO app_auth (id, app_key, app_secret, open, enabled, namespace_id, date_created, date_updated) "
                    + "VALUES ('legacy-1', 'lk-1', 'ls-1', 1, 1, 'ns-demo', NOW(3), NOW(3))");
        }

        final int deleted = appAuthService.delete(List.of("legacy-1"));

        assertEquals(1, deleted);
        assertEquals(0, AppAuthTestSupport.countRows(sqlSessionFactory, "app_auth_request", ""), "no request record may be fabricated");
    }

    @Test
    void interopWithResultAndRevokeNoResurrection() throws Exception {
        final AppAuthResultVO created = applyLifecycleAuth();
        appAuthService.delete(List.of(created.getId()));

        // result returns no credentials for REVOKED; a second revoke is idempotent
        final org.apache.shenyu.admin.model.dto.AppAuthResultQueryDTO query =
                new org.apache.shenyu.admin.model.dto.AppAuthResultQueryDTO();
        query.setNamespaceId("ns-demo");
        query.setRequestId("req-legacy-del");
        assertEquals("REVOKED", txService.result("svc-a", query).getState());

        final org.apache.shenyu.admin.model.dto.AppAuthRevokeRequestDTO revoke =
                new org.apache.shenyu.admin.model.dto.AppAuthRevokeRequestDTO();
        revoke.setNamespaceId("ns-demo");
        revoke.setRequestId("req-legacy-del");
        revoke.setReason("follow-up revoke");
        assertEquals("REVOKED", txService.revoke("svc-a", revoke).getState());
        assertTrue(true);
    }
}
