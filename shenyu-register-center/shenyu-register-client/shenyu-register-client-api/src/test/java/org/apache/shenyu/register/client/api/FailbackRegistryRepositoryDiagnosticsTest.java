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

import org.apache.shenyu.common.timer.Timer;
import org.apache.shenyu.common.timer.WheelTimerFactory;
import org.apache.shenyu.register.client.api.ClientRegistrationDiagnostics.FailureCategory;
import org.apache.shenyu.register.client.api.ClientRegistrationDiagnostics.RegistrationEntry;
import org.apache.shenyu.register.client.api.ClientRegistrationDiagnostics.RegistrationSnapshot;
import org.apache.shenyu.register.client.api.ClientRegistrationDiagnostics.RegistrationStatus;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.dto.McpToolsRegisterDTO;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Registration diagnostics recorded at the real failback boundaries (issue #8430): the
 * lifecycle fixture, duplicate failures, identity scoping, the bounded entry cap,
 * orderly-close retirement and reason redaction.
 */
class FailbackRegistryRepositoryDiagnosticsTest {

    private static final int ENTRY_CAP = 512;

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
        String identity = FailbackRegistryRepository.uriKey(dto);

        repository.persistURI(dto);
        RegistrationEntry entry = entry(identity);
        assertEquals(RegistrationStatus.REGISTERED, entry.status());
        assertEquals(FailureCategory.NONE, entry.failureCategory());
        assertNull(entry.failureReason());
        assertEquals(1, snapshot().registeredEvents());

        repository.failNext();
        repository.persistURI(dto);
        entry = entry(identity);
        assertEquals(RegistrationStatus.RETRYING, entry.status());
        assertEquals(FailureCategory.TRANSPORT, entry.failureCategory());
        assertEquals("offline", entry.failureReason());

        // recovery at the real retry boundary: the attempt is counted, success acknowledges
        repository.succeedNext();
        String key = firstPendingKey();
        repository.retry(key, repository.pendingFailureTask(key));
        entry = entry(identity);
        assertEquals(RegistrationStatus.REGISTERED, entry.status());
        assertEquals(1, entry.attempts());
        assertTrue(repository.pendingFailureKeys().isEmpty());

        // exhaustion at the owner-aware removal boundary
        repository.failNext();
        repository.persistURI(dto);
        key = firstPendingKey();
        repository.remove(key, repository.pendingFailureTask(key));
        entry = entry(identity);
        assertEquals(RegistrationStatus.EXHAUSTED, entry.status());
        assertEquals(1, snapshot().exhaustedEvents());

        // orderly close retires everything owned by this repository
        repository.closeRepository();
        assertEquals(RegistrationStatus.OFFLINE, entry(identity).status());
        assertEquals(1, snapshot().offlineEvents());
    }

    @Test
    void duplicateFailuresKeepOneEntryWithoutInflatingCounts() {
        URIRegisterDTO dto = uriDto("ns-1", "127.0.0.1", 9195);
        final String identity = FailbackRegistryRepository.uriKey(dto);
        repository.failNext();
        repository.persistURI(dto);
        repository.persistURI(dto);
        assertEquals(1, snapshot().entries().size(), "one identity, one entry");
        assertEquals(2, snapshot().retryingEvents(), "each failed boundary is one observable event");
        assertEquals(RegistrationStatus.RETRYING, entry(identity).status());
        // recovery still lands on the same entry with a counted attempt
        repository.succeedNext();
        String key = firstPendingKey();
        repository.retry(key, repository.pendingFailureTask(key));
        assertEquals(RegistrationStatus.REGISTERED, entry(identity).status());
        assertEquals(1, snapshot().entries().size());
        assertEquals(2, snapshot().retryingEvents(), "cumulative event counter, not a gauge");
    }

    @Test
    void differentNamespacesScopeSeparateIdentities() {
        URIRegisterDTO first = uriDto("ns-1", "127.0.0.1", 9195);
        URIRegisterDTO second = uriDto("ns-2", "127.0.0.1", 9195);
        repository.failNext();
        repository.persistURI(first);
        repository.persistURI(second);
        assertEquals(2, snapshot().entries().size());
        assertEquals(RegistrationStatus.RETRYING, entry(FailbackRegistryRepository.uriKey(first)).status());
        assertEquals(RegistrationStatus.RETRYING, entry(FailbackRegistryRepository.uriKey(second)).status());
    }

    @Test
    void timeoutsCarryTheTimeoutCategory() {
        URIRegisterDTO dto = uriDto("ns-1", "127.0.0.1", 9195);
        repository.failNextWithCause(new TimeoutException("read timed out"));
        repository.persistURI(dto);
        assertEquals(FailureCategory.TIMEOUT, entry(FailbackRegistryRepository.uriKey(dto)).failureCategory());
    }

    @Test
    void unconditionallyRemovedPendingBecomesUnknown() {
        URIRegisterDTO dto = uriDto("ns-1", "127.0.0.1", 9195);
        repository.failNext();
        repository.persistURI(dto);
        repository.remove(firstPendingKey());
        RegistrationEntry entry = entry(FailbackRegistryRepository.uriKey(dto));
        assertEquals(RegistrationStatus.UNKNOWN, entry.status());
        assertEquals("pending registration removed", entry.failureReason());
    }

    @Test
    void removingAnAbsentKeyLeavesTheLatestObservation() {
        URIRegisterDTO dto = uriDto("ns-1", "127.0.0.1", 9195);
        repository.persistURI(dto);
        repository.remove("absent-key");
        assertEquals(RegistrationStatus.REGISTERED, entry(FailbackRegistryRepository.uriKey(dto)).status());
        assertEquals(1, snapshot().entries().size(), "a removal that removed nothing records nothing");
    }

    @Test
    void apiDocLifecycleKeepsOneEntryAcrossPersistAndRetry() {
        ApiDocRegisterDTO dto = apiDocDto();
        String identity = FailbackRegistryRepository.apiDocKey(dto);
        repository.failNext();
        repository.persistApiDoc(dto);
        assertEquals(RegistrationStatus.RETRYING, entry(identity).status());
        assertEquals(1, snapshot().entries().size(), "persist and retry boundaries share one key");
        repository.succeedNext();
        repository.retry(identity, repository.pendingFailureTask(identity));
        assertEquals(RegistrationStatus.REGISTERED, entry(identity).status());
        assertEquals(1, snapshot().entries().size());
    }

    @Test
    void skippedPersistIsCountedButNeverReportedAsRegistered() {
        URIRegisterDTO dto = uriDto("ns-1", "127.0.0.1", 9195);
        repository.skipPersist();
        repository.persistURI(dto);
        assertTrue(snapshot().entries().isEmpty(), "a persisted nothing is not an acknowledgement");
        assertEquals(1, snapshot().skippedEvents());
        assertEquals(0, snapshot().registeredEvents());
    }

    @Test
    void attemptMarksAttemptingAndKeepsTheLastFailure() {
        URIRegisterDTO dto = uriDto("ns-1", "127.0.0.1", 9195);
        String identity = FailbackRegistryRepository.uriKey(dto);
        repository.failNext();
        repository.persistURI(dto);
        repository.succeedNext();
        repository.onPersist(() -> {
            RegistrationEntry inFlight = entry(identity);
            assertEquals(RegistrationStatus.ATTEMPTING, inFlight.status());
            assertEquals(FailureCategory.TRANSPORT, inFlight.failureCategory());
            assertEquals("offline", inFlight.failureReason());
        });
        String key = firstPendingKey();
        repository.retry(key, repository.pendingFailureTask(key));
        assertEquals(RegistrationStatus.REGISTERED, entry(identity).status());
        assertEquals(1, entry(identity).attempts());
    }

    @Test
    void capHoldsUnderConcurrentInserts() throws Exception {
        repository.failAlways();
        for (int i = 0; i < ENTRY_CAP - 1; i++) {
            repository.persistURI(uriDto("ns-fill", "10.1.0.0", 10000 + i));
        }
        int threads = 16;
        CountDownLatch start = new CountDownLatch(1);
        ExecutorService executor = Executors.newFixedThreadPool(threads);
        try {
            List<Future<?>> futures = new ArrayList<>();
            for (int t = 0; t < threads; t++) {
                final int worker = t;
                futures.add(executor.submit(() -> {
                    start.await();
                    repository.persistURI(uriDto("ns-race-" + worker, "10.2.0." + worker, 11000 + worker));
                    return null;
                }));
            }
            start.countDown();
            for (Future<?> future : futures) {
                future.get(30, TimeUnit.SECONDS);
            }
        } finally {
            executor.shutdownNow();
        }
        assertTrue(snapshot().entries().size() <= ENTRY_CAP,
                "retained identities never exceed the cap, observed " + snapshot().entries().size());
    }

    @Test
    void activeEntriesAreNeverEvictedAndOverflowIsCounted() {
        repository.failAlways();
        for (int i = 0; i < ENTRY_CAP; i++) {
            repository.persistURI(uriDto("ns-full", "10.3.0.0", 12000 + i));
        }
        repository.persistURI(uriDto("ns-late", "10.3.1.0", 13000));
        RegistrationSnapshot snapshot = snapshot();
        assertEquals(ENTRY_CAP, snapshot.entries().size());
        assertEquals(1, snapshot.overflowedEvents());
        assertEquals(0, snapshot.evictedEvents(), "active entries are never discarded");
    }

    @Test
    void retiredEntriesAreEvictedBeforeANewIdentityIsAdmitted() {
        for (int i = 0; i < ENTRY_CAP; i++) {
            repository.persistURI(uriDto("ns-full", "10.4.0.0", 14000 + i));
        }
        repository.closeRepository();
        URIRegisterDTO late = uriDto("ns-late", "10.4.1.0", 15000);
        repository.persistURI(late);
        RegistrationSnapshot snapshot = snapshot();
        assertEquals(ENTRY_CAP, snapshot.entries().size());
        assertEquals(1, snapshot.evictedEvents());
        assertEquals(0, snapshot.overflowedEvents());
        assertEquals(RegistrationStatus.REGISTERED, entry(FailbackRegistryRepository.uriKey(late)).status());
    }

    @Test
    void closeRetiresOnlyThisRepositoriesIdentities() {
        TestFailbackRegistryRepository other = new TestFailbackRegistryRepository();
        URIRegisterDTO first = uriDto("ns-1", "127.0.0.1", 9195);
        URIRegisterDTO second = uriDto("ns-1", "127.0.0.1", 9196);
        repository.persistURI(first);
        other.persistURI(second);
        repository.closeRepository();
        assertEquals(RegistrationStatus.OFFLINE, entry(FailbackRegistryRepository.uriKey(first)).status());
        assertEquals(RegistrationStatus.REGISTERED, entry(FailbackRegistryRepository.uriKey(second)).status());
        assertEquals(1, snapshot().offlineEvents());
    }

    @Test
    void credentialBearingReasonsAreRedacted() {
        URIRegisterDTO dto = uriDto("ns-1", "127.0.0.1", 9195);
        repository.failNextWithMessage(
                "connect to http://admin:s3cret@nacos:8848/nacos?password=abc123&token=xyz failed for Bearer eyJhbGciOi");
        repository.persistURI(dto);
        String reason = entry(FailbackRegistryRepository.uriKey(dto)).failureReason();
        assertFalse(reason.contains("s3cret"), reason);
        assertFalse(reason.contains("abc123"), reason);
        assertFalse(reason.contains("xyz"), reason);
        assertFalse(reason.contains("eyJhbGciOi"), reason);
        assertTrue(reason.contains("nacos"), "non-secret context stays readable");
    }

    @Test
    void failureReasonsAreTruncatedBeforeExport() {
        URIRegisterDTO dto = uriDto("ns-1", "127.0.0.1", 9195);
        repository.failNextWithMessage("x".repeat(400));
        repository.persistURI(dto);
        assertEquals(256, entry(FailbackRegistryRepository.uriKey(dto)).failureReason().length());
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

    private ApiDocRegisterDTO apiDocDto() {
        ApiDocRegisterDTO dto = new ApiDocRegisterDTO();
        dto.setContextPath("/demo");
        dto.setApiPath("/api/orders");
        dto.setHttpMethod(1);
        dto.setRpcType("http");
        dto.setVersion("1.0");
        return dto;
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

    private String firstPendingKey() {
        return repository.pendingFailureKeys().iterator().next();
    }

    private static class TestFailbackRegistryRepository extends FailbackRegistryRepository {

        private RuntimeException nextFailure;

        private boolean failAlways;

        private boolean skipPersist;

        private Runnable onPersist;

        private void failNext() {
            this.nextFailure = new IllegalStateException("offline");
        }

        private void failAlways() {
            this.failAlways = true;
        }

        private void skipPersist() {
            this.skipPersist = true;
        }

        private void onPersist(final Runnable hook) {
            this.onPersist = hook;
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
        protected boolean doPersistURI(final URIRegisterDTO registerDTO) {
            if (Objects.nonNull(onPersist)) {
                onPersist.run();
            }
            if (skipPersist) {
                return false;
            }
            if (failAlways) {
                throw new IllegalStateException("offline");
            }
            if (Objects.nonNull(nextFailure)) {
                throw nextFailure;
            }
            return true;
        }

        @Override
        protected boolean doPersistInterface(final MetaDataRegisterDTO registerDTO) {
            return true;
        }

        @Override
        protected boolean doPersistApiDoc(final ApiDocRegisterDTO apiDocRegisterDTO) {
            if (Objects.nonNull(nextFailure)) {
                throw nextFailure;
            }
            return true;
        }

        @Override
        protected boolean doPersistMcpTools(final McpToolsRegisterDTO registerDTO) {
            return true;
        }
    }
}