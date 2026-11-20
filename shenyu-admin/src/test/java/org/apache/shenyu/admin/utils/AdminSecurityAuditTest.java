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

package org.apache.shenyu.admin.utils;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.transaction.support.TransactionTemplate;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Verify bounded state/output, caller-independent correlation and commit ownership.
 */
class AdminSecurityAuditTest {

    @AfterEach
    void cleanup() {
        if (TransactionSynchronizationManager.isSynchronizationActive()) {
            TransactionSynchronizationManager.clearSynchronization();
        }
        TransactionSynchronizationManager.setActualTransactionActive(false);
    }

    @Test
    void correlationIsServerOwnedAndRequestScoped() {
        MockHttpServletRequest first = new MockHttpServletRequest();
        first.addHeader(AdminSecurityAudit.REQUEST_ID_HEADER, "caller-controlled");
        final String id = AdminSecurityAudit.requestId(first);
        assertNotEquals("caller-controlled", id);
        assertEquals(id, AdminSecurityAudit.requestId(first));
        assertNotEquals(id, AdminSecurityAudit.requestId(new MockHttpServletRequest()));
    }

    @Test
    void boundedFieldsAndJsonEscapingDoNotReadRequestSecrets() {
        List<String> events = new ArrayList<>();
        final AdminSecurityAudit audit = new AdminSecurityAudit(() -> 0, events::add);
        MockHttpServletRequest request = new MockHttpServletRequest("POST", "/raw-secret-path");
        request.addHeader("Authorization", "secret-token");
        request.setQueryString("password=secret-password");
        request.setContent("secret-body".getBytes(java.nio.charset.StandardCharsets.UTF_8));
        audit.record(AdminSecurityAudit.Reason.RESOURCE_DENIED, request, "actor", "read", "ns", "x\n\"" + "r".repeat(1000));
        String json = events.get(0);
        JsonObject event = JsonParser.parseString(json).getAsJsonObject();
        assertEquals(128, event.get("resource_id").getAsString().length());
        assertEquals("actor", event.get("actor_id").getAsString());
        assertFalse(json.contains("\n"));
        assertFalse(json.contains("secret"));
        assertEquals(1, event.get("schema").getAsInt());
        assertTrue(event.get("timestamp_ms").getAsLong() > 0);
    }

    @Test
    void burstOutputIsBoundedAndSuppressionIsReportedAfterRefill() {
        AtomicLong time = new AtomicLong(Long.MIN_VALUE);
        List<String> events = new ArrayList<>();
        final AdminSecurityAudit audit = new AdminSecurityAudit(time::get, events::add);
        for (int i = 0; i < 1000; i++) {
            audit.record(AdminSecurityAudit.Reason.REJECTED_TOKEN, null, "unknown", "authenticate", null, null);
        }
        assertEquals(AdminSecurityAudit.FAILURE_LIMIT, events.size());
        time.addAndGet(TimeUnit.SECONDS.toNanos(1));
        audit.record(AdminSecurityAudit.Reason.REJECTED_TOKEN, null, "unknown", "authenticate", null, null);
        assertEquals(990, JsonParser.parseString(events.get(10)).getAsJsonObject().get("suppressed_since_previous").getAsInt());
        audit.record(AdminSecurityAudit.Reason.ACTION_DENIED, null, "actor", "read", null, null);
        assertEquals(12, events.size());
    }

    @Test
    void concurrentCallersCannotExceedTheCategoryBudget() throws Exception {
        List<String> events = Collections.synchronizedList(new ArrayList<>());
        final AdminSecurityAudit audit = new AdminSecurityAudit(() -> 0, events::add);
        ExecutorService pool = Executors.newFixedThreadPool(8);
        CountDownLatch start = new CountDownLatch(1);
        try {
            List<java.util.concurrent.Future<?>> futures = new ArrayList<>();
            for (int worker = 0; worker < 8; worker++) {
                futures.add(pool.submit(() -> {
                    try {
                        start.await();
                        for (int i = 0; i < 1000; i++) {
                            audit.record(AdminSecurityAudit.Reason.MISSING_TOKEN, null, "unknown", "authenticate", null, null);
                        }
                    } catch (InterruptedException ex) {
                        Thread.currentThread().interrupt();
                        throw new IllegalStateException(ex);
                    }
                }));
            }
            start.countDown();
            for (java.util.concurrent.Future<?> future : futures) {
                future.get(5, TimeUnit.SECONDS);
            }
            assertEquals(AdminSecurityAudit.FAILURE_LIMIT, events.size());
        } finally {
            pool.shutdownNow();
            assertTrue(pool.awaitTermination(5, TimeUnit.SECONDS));
        }
    }

    @Test
    void revocationCapturesIdentityAndPublishesOnlyAfterCommit() {
        List<String> events = new ArrayList<>();
        final AdminSecurityAudit audit = new AdminSecurityAudit(() -> 0, events::add);
        MockHttpServletRequest request = new MockHttpServletRequest();
        final String id = AdminSecurityAudit.requestId(request);
        TransactionSynchronizationManager.initSynchronization();
        TransactionSynchronizationManager.setActualTransactionActive(true);
        audit.revoked(request, "admin-id", "target-id");
        assertTrue(events.isEmpty());
        TransactionSynchronizationManager.getSynchronizations().forEach(TransactionSynchronization::afterCommit);
        JsonObject event = JsonParser.parseString(events.get(0)).getAsJsonObject();
        assertEquals("CREDENTIAL_REVOKED", event.get("reason").getAsString());
        assertEquals("success", event.get("outcome").getAsString());
        assertEquals("admin-id", event.get("actor_id").getAsString());
        assertEquals("target-id", event.get("resource_id").getAsString());
        assertEquals(id, event.get("request_id").getAsString());
    }

    @Test
    void rolledBackRevocationDoesNotReportSuccess() {
        List<String> events = new ArrayList<>();
        final AdminSecurityAudit audit = new AdminSecurityAudit(() -> 0, events::add);
        TransactionSynchronizationManager.initSynchronization();
        TransactionSynchronizationManager.setActualTransactionActive(true);
        audit.revoked(null, "admin-id", "target-id");
        TransactionSynchronizationManager.getSynchronizations().forEach(s -> s.afterCompletion(TransactionSynchronization.STATUS_ROLLED_BACK));
        assertTrue(events.isEmpty());
    }

    @Test
    void realSpringTransactionCommitAndRollbackOwnRevocationPublication() {
        List<String> events = new ArrayList<>();
        final AdminSecurityAudit audit = new AdminSecurityAudit(() -> 0, events::add);
        TransactionTemplate transaction = new TransactionTemplate(new DataSourceTransactionManager(
                new DriverManagerDataSource("jdbc:h2:mem:admin_security_audit", "sa", "")));
        transaction.execute(status -> {
            audit.revoked(null, "actor", "rolled-back-user");
            assertTrue(events.isEmpty());
            status.setRollbackOnly();
            return null;
        });
        assertTrue(events.isEmpty());
        transaction.execute(status -> {
            audit.revoked(null, "actor", "committed-user");
            assertTrue(events.isEmpty());
            return null;
        });
        assertEquals(1, events.size());
        assertTrue(events.get(0).contains("committed-user"));
        assertFalse(events.get(0).contains("rolled-back-user"));
    }

    @Test
    void sinkFailureDoesNotChangeTheBusinessOutcome() {
        final AdminSecurityAudit audit = new AdminSecurityAudit(() -> 0, event -> {
            throw new IllegalStateException("unavailable sink");
        });
        assertDoesNotThrow(() -> audit.record(AdminSecurityAudit.Reason.ACTION_DENIED, null, "actor", "read", null, null));
        assertDoesNotThrow(() -> audit.revoked(null, "actor", "target"));
    }
}
