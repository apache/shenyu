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

package org.apache.shenyu.register.client.api;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.timer.Timer;
import org.apache.shenyu.common.timer.WheelTimerFactory;
import org.apache.shenyu.register.client.api.ClientRegistrationDiagnostics.FailureCategory;
import org.apache.shenyu.register.client.api.retry.FailureRegistryTask;
import org.apache.shenyu.register.client.api.ClientRegistrationDiagnostics.RegistrationEntry;
import org.apache.shenyu.register.client.api.ClientRegistrationDiagnostics.RegistrationSnapshot;
import org.apache.shenyu.register.client.api.ClientRegistrationDiagnostics.RegistrationStatus;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.lang.reflect.Field;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeoutException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Registration diagnostics recorded at the real failback boundaries (issue #8430): the
 * lifecycle fixture, duplicate failures, identity scoping, the bounded entry cap and
 * orderly-close retirement.
 */
class FailbackRegistryRepositoryDiagnosticsTest {

    private final ClientRegistrationDiagnostics diagnostics = ClientRegistrationDiagnostics.getInstance();

    private TestFailbackRegistryRepository repository;

    private MockedStatic<WheelTimerFactory> timerFactory;

    private Timer timer;

    @BeforeEach
    public void setUp() {
        timer = Mockito.mock(Timer.class);
        timerFactory = Mockito.mockStatic(WheelTimerFactory.class);
        timerFactory.when(WheelTimerFactory::getSharedTimer).thenReturn(timer);
        repository = new TestFailbackRegistryRepository();
        diagnostics.resetForTest();
    }

    @AfterEach
    public void tearDown() {
        timerFactory.close();
        diagnostics.resetForTest();
    }

    @Test
    void lifecycleRecordsSuccessRetryRecoveryExhaustionAndClose() {
        URIRegisterDTO dto = uriDto("ns-1", "127.0.0.1", 9195);
        String identity = uriIdentity(dto);

        repository.persistURI(dto);
        RegistrationEntry entry = entry(identity);
        assertEquals(RegistrationStatus.REGISTERED, entry.status());
        assertEquals(FailureCategory.NONE, entry.failureCategory());
        assertNull(entry.failureReason());
        assertEquals(1, snapshot().registeredCount());

        repository.failNext();
        repository.persistURI(dto);
        entry = entry(identity);
        assertEquals(RegistrationStatus.RETRYING, entry.status());
        assertEquals(FailureCategory.TRANSPORT, entry.failureCategory());
        assertEquals("offline", entry.failureReason());

        // recovery at the real retry boundary: the attempt is counted, success acknowledges
        repository.succeedNext();
        repository.retry(firstFailureMapKey());
        entry = entry(identity);
        assertEquals(RegistrationStatus.REGISTERED, entry.status());
        assertEquals(1, entry.attempts());
        assertEquals(0, failureMapSize());

        // exhaustion at the owner-aware removal boundary
        repository.failNext();
        repository.persistURI(dto);
        String key = firstFailureMapKey();
        repository.remove(key, repository.currentRetryTask(key));
        entry = entry(identity);
        assertEquals(RegistrationStatus.EXHAUSTED, entry.status());
        assertEquals(1, snapshot().exhaustedCount());

        // orderly close retires everything as offline
        repository.closeRepository();
        assertEquals(RegistrationStatus.OFFLINE, entry(identity).status());
        assertTrue(snapshot().offlineCount() >= 1);
    }

    @Test
    void duplicateFailuresKeepOneEntryWithoutInflatingCounts() {
        URIRegisterDTO dto = uriDto("ns-1", "127.0.0.1", 9195);
        final String identity = uriIdentity(dto);
        repository.failNext();
        repository.persistURI(dto);
        repository.persistURI(dto);
        assertEquals(1, snapshot().entries().size(), "one identity, one entry");
        assertEquals(2, snapshot().retryingCount(), "each failed boundary is observable once");
        assertEquals(RegistrationStatus.RETRYING, entry(identity).status());
        // recovery still lands on the same entry with a counted attempt
        repository.succeedNext();
        repository.retry(firstFailureMapKey());
        assertEquals(RegistrationStatus.REGISTERED, entry(identity).status());
        assertEquals(1, snapshot().entries().size());
    }

    @Test
    void differentNamespacesScopeSeparateIdentities() {
        URIRegisterDTO first = uriDto("ns-1", "127.0.0.1", 9195);
        URIRegisterDTO second = uriDto("ns-2", "127.0.0.1", 9195);
        repository.failNext();
        repository.persistURI(first);
        repository.persistURI(second);
        assertEquals(2, snapshot().entries().size());
        assertEquals(RegistrationStatus.RETRYING, entry(uriIdentity(first)).status());
        assertEquals(RegistrationStatus.RETRYING, entry(uriIdentity(second)).status());
    }

    @Test
    void timeoutsCarryTheTimeoutCategory() {
        URIRegisterDTO dto = uriDto("ns-1", "127.0.0.1", 9195);
        repository.failNextWithCause(new TimeoutException("read timed out"));
        repository.persistURI(dto);
        assertEquals(FailureCategory.TIMEOUT, entry(uriIdentity(dto)).failureCategory());
    }

    @Test
    void unconditionallyRemovedPendingBecomesUnknown() {
        URIRegisterDTO dto = uriDto("ns-1", "127.0.0.1", 9195);
        repository.failNext();
        repository.persistURI(dto);
        repository.remove(firstFailureMapKey());
        RegistrationEntry entry = entry(uriIdentity(dto));
        assertEquals(RegistrationStatus.UNKNOWN, entry.status());
        assertEquals("pending registration removed", entry.failureReason());
    }

    @Test
    void entryCapOverflowsIntoACounterWithoutDroppingObservations() {
        for (int i = 0; i < 513; i++) {
            URIRegisterDTO dto = uriDto("ns-1", "10.0.0." + (i % 200), 9100 + i);
            repository.failNext();
            repository.persistURI(dto);
        }
        RegistrationSnapshot snapshot = snapshot();
        assertEquals(512, snapshot.entries().size(), "retained identities are hard-capped");
        assertEquals(1, snapshot.overflowedCount());
        assertEquals(513, snapshot.retryingCount(), "counters are not capped");
    }

    @Test
    void failureReasonsAreTruncatedBeforeExport() {
        URIRegisterDTO dto = uriDto("ns-1", "127.0.0.1", 9195);
        repository.failNextWithMessage("x".repeat(400));
        repository.persistURI(dto);
        assertEquals(256, entry(uriIdentity(dto)).failureReason().length());
    }

    @Test
    void queuedEventsAreCountedAtTheEnqueueBoundary() {
        diagnostics.recordQueuedEvent();
        diagnostics.recordQueuedEvent();
        assertEquals(2, snapshot().queuedEvents());
        assertTrue(snapshot().entries().isEmpty(), "no per-event identity exists before a boundary");
    }

    private URIRegisterDTO uriDto(final String namespace, final String host, final int port) {
        URIRegisterDTO dto = new URIRegisterDTO();
        dto.setNamespaceId(namespace);
        dto.setProtocol("http");
        dto.setAppName("demo-app");
        dto.setContextPath("/demo");
        dto.setRpcType("http");
        dto.setHost(host);
        dto.setPort(port);
        return dto;
    }

    private static String uriIdentity(final URIRegisterDTO dto) {
        return String.join(":", value(dto.getNamespaceId()), value(dto.getProtocol()), value(dto.getAppName()),
                value(dto.getContextPath()), value(dto.getRpcType()), value(dto.getHost()), value(dto.getPort()))
                + ":" + Constants.URI;
    }

    private static String value(final Object value) {
        return java.util.Objects.toString(value, "");
    }

    private RegistrationEntry entry(final String identity) {
        return snapshot().entries().stream()
                .filter(candidate -> candidate.identity().equals(identity))
                .findFirst()
                .orElseThrow(() -> new AssertionError("missing diagnostics entry for " + identity));
    }

    private RegistrationSnapshot snapshot() {
        return diagnostics.snapshot();
    }

    private String firstFailureMapKey() {
        try {
            Field field = FailbackRegistryRepository.class.getDeclaredField("concurrentHashMap");
            field.setAccessible(true);
            Map<String, ?> map = (Map<String, ?>) field.get(repository);
            return map.keySet().iterator().next();
        } catch (ReflectiveOperationException ex) {
            throw new IllegalStateException(ex);
        }
    }

    private int failureMapSize() {
        try {
            Field field = FailbackRegistryRepository.class.getDeclaredField("concurrentHashMap");
            field.setAccessible(true);
            return ((ConcurrentHashMap<?, ?>) field.get(repository)).size();
        } catch (ReflectiveOperationException ex) {
            throw new IllegalStateException(ex);
        }
    }

    private static class TestFailbackRegistryRepository extends FailbackRegistryRepository {

        private RuntimeException nextFailure;

        private void failNext() {
            this.nextFailure = new IllegalStateException("offline");
        }

        private void failNextWithCause(final Throwable cause) {
            this.nextFailure = new IllegalStateException("wrapped: " + cause.getMessage(), cause);
        }

        private void failNextWithMessage(final String message) {
            this.nextFailure = new IllegalStateException(message);
        }

        private void succeedNext() {
            this.nextFailure = null;
        }

        @Override
        protected void doPersistURI(final URIRegisterDTO registerDTO) {
            if (java.util.Objects.nonNull(nextFailure)) {
                throw nextFailure;
            }
        }

        @Override
        protected void doPersistInterface(final org.apache.shenyu.register.common.dto.MetaDataRegisterDTO registerDTO) {
            /* Test implementation */
        }

        @Override
        protected void doPersistApiDoc(final org.apache.shenyu.register.common.dto.ApiDocRegisterDTO apiDocRegisterDTO) {
            /* Test implementation */
        }

        @Override
        protected void doPersistMcpTools(final org.apache.shenyu.register.common.dto.McpToolsRegisterDTO registerDTO) {
            /* Test implementation */
        }

        private FailureRegistryTask currentRetryTask(final String key) {
            try {
                Field holderField = FailbackRegistryRepository.class.getDeclaredField("concurrentHashMap");
                holderField.setAccessible(true);
                Map<String, Object> map = (Map<String, Object>) holderField.get(this);
                Object holder = map.get(key);
                Field taskField = holder.getClass().getDeclaredField("retryTask");
                taskField.setAccessible(true);
                return (FailureRegistryTask) taskField.get(holder);
            } catch (ReflectiveOperationException ex) {
                throw new IllegalStateException(ex);
            }
        }
    }
}
