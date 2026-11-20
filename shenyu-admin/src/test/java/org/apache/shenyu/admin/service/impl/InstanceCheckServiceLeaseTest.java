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

import org.apache.shenyu.admin.config.properties.InstanceRetentionProperties;
import org.apache.shenyu.admin.config.properties.RegistrationLeaseProperties;
import org.apache.shenyu.admin.model.vo.InstanceInfoVO;
import org.apache.shenyu.admin.service.InstanceInfoService;
import org.apache.shenyu.admin.service.fencing.RegistrationFencingGuard;
import org.apache.shenyu.common.constant.InstanceTypeConstants;
import org.apache.shenyu.common.enums.InstanceStatusEnum;
import org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO;
import org.apache.shenyu.register.common.protocol.RegistrationLeaseProtocol;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.test.util.ReflectionTestUtils;

import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import java.util.concurrent.atomic.AtomicLong;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.when;

/**
 * Integration of the registration lease with the liveness check service, driven by a
 * controlled clock: expiry happens at the exact lease deadline, a rejected beat never
 * refreshes liveness, and fenced addresses stay out of lease scope. No test sleeps.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class InstanceCheckServiceLeaseTest {

    private static final long START_MILLIS = 1_000_000L;

    @Mock
    private InstanceInfoService instanceInfoService;

    @Mock
    private RegistrationFencingGuard registrationFencingGuard;

    private InstanceCheckService instanceCheckService;

    private InstanceLeaseService leaseService;

    private AtomicLong nowMillis;

    @BeforeEach
    void setUp() {
        nowMillis = new AtomicLong(START_MILLIS);
        Clock clock = new Clock() {
            @Override
            public ZoneId getZone() {
                return ZoneId.systemDefault();
            }

            @Override
            public Clock withZone(final ZoneId zone) {
                return this;
            }

            @Override
            public Instant instant() {
                return Instant.ofEpochMilli(nowMillis.get());
            }
        };
        RegistrationLeaseProperties properties = new RegistrationLeaseProperties();
        properties.setEnabled(true);
        leaseService = new InstanceLeaseService(properties);
        instanceCheckService = new InstanceCheckService(instanceInfoService, registrationFencingGuard,
                new InstanceRetentionProperties(), clock);
        instanceCheckService.setInstanceLeaseService(leaseService);
    }

    @Test
    void leaseBeatGrantsLeaseAndRefreshesLiveness() {
        RegistrationLeaseProtocol.LeaseResponse response = instanceCheckService.handleLeaseBeat(beat(null));
        assertEquals(RegistrationLeaseProtocol.OK, response.code());
        InstanceInfoVO instance = instanceCheckService.getInstanceHealthBeatInfo("10.0.0.1:8080@http#default");
        assertNotNull(instance);
        assertEquals(START_MILLIS, instance.getLastHeartBeatTime());
        assertEquals(InstanceStatusEnum.ONLINE.getCode(), instance.getInstanceState());
    }

    @Test
    void rejectedLeaseBeatDoesNotRefreshLiveness() {
        final RegistrationLeaseProtocol.LeaseResponse granted = instanceCheckService.handleLeaseBeat(beat(null));
        nowMillis.addAndGet(3_000L);

        RegistrationLeaseProtocol.LeaseResponse rejected = instanceCheckService.handleLeaseBeat(beat("foreign-lease"));

        assertEquals(RegistrationLeaseProtocol.STALE_LEASE, rejected.code());
        InstanceInfoVO instance = instanceCheckService.getInstanceHealthBeatInfo("10.0.0.1:8080@http#default");
        assertEquals(START_MILLIS, instance.getLastHeartBeatTime(), "a rejected beat must not refresh liveness");

        // the lease deadline still ends the instance at the granted deadline
        nowMillis.set(granted.expiryDeadlineMs() + 1L);
        ReflectionTestUtils.invokeMethod(instanceCheckService, "doCheck");
        assertEquals(InstanceStatusEnum.OFFLINE.getCode(), instance.getInstanceState());
    }

    @Test
    void instanceGoesOfflineExactlyAtTheLeaseDeadlineAndNotBefore() {
        RegistrationLeaseProtocol.LeaseResponse granted = instanceCheckService.handleLeaseBeat(beat(null));
        InstanceInfoVO instance = instanceCheckService.getInstanceHealthBeatInfo("10.0.0.1:8080@http#default");
        long deadline = granted.expiryDeadlineMs();

        nowMillis.set(deadline);
        ReflectionTestUtils.invokeMethod(instanceCheckService, "doCheck");
        assertEquals(InstanceStatusEnum.ONLINE.getCode(), instance.getInstanceState(),
                "a renewal due exactly at the deadline is still accepted");

        nowMillis.set(deadline + 1L);
        ReflectionTestUtils.invokeMethod(instanceCheckService, "doCheck");
        assertEquals(InstanceStatusEnum.OFFLINE.getCode(), instance.getInstanceState());
    }

    @Test
    void renewalAdvancesTheDeadlineAndKeepsTheInstanceOnline() {
        instanceCheckService.handleLeaseBeat(beat(null));
        final InstanceInfoVO instance = instanceCheckService.getInstanceHealthBeatInfo("10.0.0.1:8080@http#default");
        final RegistrationLeaseProtocol.LeaseResponse granted = instanceCheckService.handleLeaseBeat(beat(null));

        nowMillis.set(granted.expiryDeadlineMs() - 1L);
        // a renewal accepted at the previous receipt keeps the instance alive past the
        // original deadline
        instanceCheckService.handleLeaseBeat(renewal(granted.leaseId()));
        nowMillis.set(granted.expiryDeadlineMs() + 1_000L);
        ReflectionTestUtils.invokeMethod(instanceCheckService, "doCheck");
        assertEquals(InstanceStatusEnum.ONLINE.getCode(), instance.getInstanceState());
    }

    @Test
    void compactRenewalPreservesTheLastObservation() {
        InstanceBeatInfoDTO first = beat(null);
        first.setInstanceInfo("{\"cpu\":8}");
        instanceCheckService.handleLeaseBeat(first);

        nowMillis.addAndGet(1_000L);
        InstanceBeatInfoDTO renewal = renewal(instanceCheckService.handleLeaseBeat(beat(null)).leaseId());
        renewal.setInstanceInfo(null);
        instanceCheckService.handleLeaseBeat(renewal);

        InstanceInfoVO instance = instanceCheckService.getInstanceHealthBeatInfo("10.0.0.1:8080@http#default");
        assertEquals("{\"cpu\":8}", instance.getInstanceInfo(), "a compact renewal must not wipe the observation");
        assertEquals(nowMillis.get(), instance.getLastHeartBeatTime());
    }

    @Test
    void fencedAddressIsOutOfLeaseScope() {
        InstanceBeatInfoDTO dto = beat(null);
        dto.setInstanceType(InstanceTypeConstants.CLIENT_INSTANCE_TYPE);
        when(registrationFencingGuard.isProtectedAddress("default", "10.0.0.1", 8080)).thenReturn(true);

        RegistrationLeaseProtocol.LeaseResponse response = instanceCheckService.handleLeaseBeat(dto);

        assertEquals(RegistrationLeaseProtocol.LEASE_NOT_ENABLED, response.code());
        assertNull(instanceCheckService.getInstanceHealthBeatInfo(instanceCheckService.getInstanceKey(dto)),
                "a fenced address must not enter the lease-managed liveness map");
    }

    @Test
    void withoutLeaseServiceTheLegacyPathIsUntouched() {
        InstanceCheckService legacyService = new InstanceCheckService(instanceInfoService, registrationFencingGuard,
                new InstanceRetentionProperties(), Clock.systemDefaultZone());

        RegistrationLeaseProtocol.LeaseResponse response = legacyService.handleLeaseBeat(beat(null));
        assertEquals(RegistrationLeaseProtocol.LEASE_NOT_ENABLED, response.code());

        InstanceBeatInfoDTO dto = beat(null);
        dto.setInstanceInfo("{\"cpu\":8}");
        assertEquals(Boolean.TRUE, legacyService.handleBeatInfo(dto), "legacy beat must keep v1 semantics");
        assertNotNull(legacyService.getInstanceHealthBeatInfo("10.0.0.1:8080@http#default"));
    }

    private InstanceBeatInfoDTO beat(final String leaseId) {
        InstanceBeatInfoDTO dto = new InstanceBeatInfoDTO();
        dto.setInstanceIp("10.0.0.1");
        dto.setInstancePort("8080");
        dto.setInstanceType("http");
        dto.setNamespaceId("default");
        dto.setLeaseId(leaseId);
        return dto;
    }

    private InstanceBeatInfoDTO renewal(final String leaseId) {
        return beat(leaseId);
    }
}
