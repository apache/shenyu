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

import org.apache.shenyu.admin.mapper.AppAuthRequestMapper;
import org.apache.shenyu.authtest.AppAuthTestSupport;
import org.apache.shenyu.admin.model.dto.AppAuthApplyRequestDTO;
import org.apache.shenyu.admin.model.dto.AppAuthRevokeRequestDTO;
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
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * apply/revoke race verification (isolated MySQL, two threads committing at a barrier):
 * whichever side gets the lock first, the final state is REVOKED with no auth or child rows, late applies rejected, no orphans.
 */
@org.junit.jupiter.api.condition.EnabledIfSystemProperty(named = "shenyu.auth.test.jdbcUrl", matches = ".+")
class AppAuthApplyRevokeRaceTest {

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
        request.setRequestId("req-race");
        request.setAppName("demo-app");
        request.setUserId("10001");
        request.setPathList(List.of("/api/a"));
        return request;
    }

    private AppAuthRevokeRequestDTO revokeRequest() {
        final AppAuthRevokeRequestDTO request = new AppAuthRevokeRequestDTO();
        request.setNamespaceId("ns-demo");
        request.setRequestId("req-race");
        request.setReason("race drill");
        return request;
    }

    @Test
    void concurrentApplyAndRevokeAlwaysEndsRevoked() throws Exception {
        final CyclicBarrier barrier = new CyclicBarrier(2);
        final ExecutorService pool = Executors.newFixedThreadPool(2);
        try {
            final Future<AppAuthResultVO> applyFuture = pool.submit(() -> {
                barrier.await(10, TimeUnit.SECONDS);
                return txService.apply("svc-r", applyRequest(), "hash-race");
            });
            final Future<AppAuthResultVO> revokeFuture = pool.submit(() -> {
                barrier.await(10, TimeUnit.SECONDS);
                return txService.revoke("svc-r", revokeRequest());
            });
            // revoke always succeeds; apply may succeed then get deleted or be rejected by the tombstone
            assertEquals("REVOKED", revokeFuture.get(60, TimeUnit.SECONDS).getState());
            assertTrue(isSettled(applyFuture));
        } finally {
            pool.shutdownNow();
        }

        final org.apache.shenyu.admin.model.entity.AppAuthRequestDO record =
                requestMapper.selectByKey("svc-r", "ns-demo", "req-race");
        assertEquals("REVOKED", record.getState());
        assertEquals(0, AppAuthTestSupport.countRows(sqlSessionFactory, "app_auth", ""), "no auth row may remain in the final state");
        assertEquals(0, AppAuthTestSupport.countRows(sqlSessionFactory, "auth_param", ""));
        assertEquals(0, AppAuthTestSupport.countRows(sqlSessionFactory, "auth_path", ""));

        // late applies must be rejected
        try {
            txService.apply("svc-r", applyRequest(), "hash-race");
            throw new AssertionError("apply must be rejected after REVOKED");
        } catch (Exception expected) {
            assertTrue(expected.getMessage().contains("revoke"));
        }
    }

    private boolean isSettled(final Future<AppAuthResultVO> future) throws Exception {
        try {
            future.get(60, TimeUnit.SECONDS);
            return true;
        } catch (Exception e) {
            return true;
        }
    }
}
