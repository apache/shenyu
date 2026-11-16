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
import org.apache.shenyu.admin.model.vo.AppAuthResultVO;
import org.apache.ibatis.session.SqlSessionFactory;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Real-concurrency apply verification (isolated MySQL, two threads committing at a barrier):
 * same-parameter concurrency returns the same auth; different-parameter concurrency lets exactly one side win; no orphan rows remain.
 */
@org.junit.jupiter.api.condition.EnabledIfSystemProperty(named = "shenyu.auth.test.jdbcUrl", matches = ".+")
class AppAuthApplyConcurrencyTest {

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

    private AppAuthApplyRequestDTO applyRequest(final String requestId, final String appName) {
        final AppAuthApplyRequestDTO request = new AppAuthApplyRequestDTO();
        request.setNamespaceId("ns-demo");
        request.setRequestId(requestId);
        request.setAppName(appName);
        request.setUserId("10001");
        request.setPathList(List.of("/api/a", "/api/b"));
        return request;
    }

    @Test
    void sameParamsConcurrentApplyReturnsSameAuth() throws Exception {
        final AppAuthApplyRequestDTO request = applyRequest("req-race-same", "demo-app");
        final CyclicBarrier barrier = new CyclicBarrier(2);
        final ExecutorService pool = Executors.newFixedThreadPool(2);
        try {
            final Future<AppAuthResultVO> first = pool.submit(() -> raceApply(barrier, "svc-c", request, "hash-same"));
            final Future<AppAuthResultVO> second = pool.submit(() -> raceApply(barrier, "svc-c", request, "hash-same"));
            final AppAuthResultVO vo1 = first.get(60, TimeUnit.SECONDS);
            final AppAuthResultVO vo2 = second.get(60, TimeUnit.SECONDS);

            assertEquals(vo1.getId(), vo2.getId());
            assertEquals(vo1.getAppKey(), vo2.getAppKey());
            assertEquals(vo1.getAppSecret(), vo2.getAppSecret());
        } finally {
            pool.shutdownNow();
        }
        assertNoOrphans();
    }

    @Test
    void differentParamsConcurrentApplyExactlyOneWins() throws Exception {
        final CyclicBarrier barrier = new CyclicBarrier(2);
        final ExecutorService pool = Executors.newFixedThreadPool(2);
        try {
            final Future<AppAuthResultVO> winnerA = pool.submit(() ->
                    raceApply(barrier, "svc-c", applyRequest("req-race-diff", "app-alpha"), "hash-alpha"));
            final Future<AppAuthResultVO> winnerB = pool.submit(() ->
                    raceApply(barrier, "svc-c", applyRequest("req-race-diff", "app-beta"), "hash-beta"));

            final AppAuthResultVO succeeded;
            final Future<AppAuthResultVO> failed;
            if (succeededFuture(winnerA)) {
                succeeded = winnerA.get(60, TimeUnit.SECONDS);
                failed = winnerB;
            } else {
                succeeded = winnerB.get(60, TimeUnit.SECONDS);
                failed = winnerA;
            }
            assertTrue(Objects.nonNull(succeeded.getId()));
            assertThrows(Exception.class, () -> failed.get(60, TimeUnit.SECONDS));

            final String storedHash = requestMapper.selectByKey("svc-c", "ns-demo", "req-race-diff").getRequestHash();
            assertTrue("hash-alpha".equals(storedHash) || "hash-beta".equals(storedHash),
                    "the surviving record must carry the winner's digest");
        } finally {
            pool.shutdownNow();
        }
        assertNoOrphans();
    }

    private boolean succeededFuture(final Future<AppAuthResultVO> future) throws Exception {
        try {
            future.get(60, TimeUnit.SECONDS);
            return true;
        } catch (Exception e) {
            return false;
        }
    }

    private AppAuthResultVO raceApply(final CyclicBarrier barrier, final String callerKey,
                                      final AppAuthApplyRequestDTO request, final String hash) throws Exception {
        barrier.await(10, TimeUnit.SECONDS);
        return txService.apply(callerKey, request, hash);
    }

    private void assertNoOrphans() throws SQLException {
        assertEquals(1, AppAuthTestSupport.countRows(sqlSessionFactory, "app_auth_request", ""));
        assertEquals(1, AppAuthTestSupport.countRows(sqlSessionFactory, "app_auth", ""));
        assertEquals(1, AppAuthTestSupport.countRows(sqlSessionFactory, "auth_param", ""));
        assertEquals(2, AppAuthTestSupport.countRows(sqlSessionFactory, "auth_path", ""));
    }
}
