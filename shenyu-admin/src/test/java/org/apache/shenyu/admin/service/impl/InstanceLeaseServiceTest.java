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

import org.apache.shenyu.admin.config.properties.RegistrationLeaseProperties;
import org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO;
import org.apache.shenyu.register.common.protocol.RegistrationLeaseProtocol;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.time.Duration;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Unit tests for the server-issued registration lease: idempotent renewal, stale and
 * expired leases and interval negotiation, all against explicit receipt timestamps so
 * no test relies on sleeps.
 */
class InstanceLeaseServiceTest {

    private static final String KEY = "10.0.0.1:8080@http#default";

    private RegistrationLeaseProperties properties;

    private InstanceLeaseService leaseService;

    @BeforeEach
    void setUp() {
        properties = new RegistrationLeaseProperties();
        properties.setEnabled(true);
        leaseService = new InstanceLeaseService(properties);
    }

    @Test
    void grantOnFirstBeatUsesDefaultIntervalAndFactor() {
        RegistrationLeaseProtocol.LeaseResponse response = leaseService.onBeat(KEY, beat(null, null), 1_000L);
        assertEquals(RegistrationLeaseProtocol.OK, response.code());
        assertNotNull(response.leaseId());
        assertEquals(5, response.intervalSeconds());
        assertEquals(16_000L, response.expiryDeadlineMs());
    }

    @Test
    void renewalWithMatchingLeaseIdExtendsDeadlineFromReceipt() {
        RegistrationLeaseProtocol.LeaseResponse granted = leaseService.onBeat(KEY, beat(null, null), 1_000L);
        RegistrationLeaseProtocol.LeaseResponse renewed = leaseService.onBeat(KEY,
                beat(granted.leaseId(), null), 6_000L);
        assertEquals(RegistrationLeaseProtocol.OK, renewed.code());
        assertEquals(granted.leaseId(), renewed.leaseId());
        assertEquals(21_000L, renewed.expiryDeadlineMs());
    }

    @Test
    void delayedOrReplayedRenewalCanNotShrinkTheDeadline() {
        RegistrationLeaseProtocol.LeaseResponse granted = leaseService.onBeat(KEY, beat(null, null), 1_000L);
        leaseService.onBeat(KEY, beat(granted.leaseId(), null), 10_000L);
        RegistrationLeaseProtocol.LeaseResponse replay = leaseService.onBeat(KEY,
                beat(granted.leaseId(), null), 5_000L);
        assertEquals(RegistrationLeaseProtocol.OK, replay.code());
        assertEquals(25_000L, replay.expiryDeadlineMs(), "a late replay must keep the monotonic deadline");
    }

    @Test
    void duplicateRenewalsAreIdempotent() {
        RegistrationLeaseProtocol.LeaseResponse granted = leaseService.onBeat(KEY, beat(null, null), 1_000L);
        RegistrationLeaseProtocol.LeaseResponse first = leaseService.onBeat(KEY, beat(granted.leaseId(), null), 3_000L);
        RegistrationLeaseProtocol.LeaseResponse duplicate = leaseService.onBeat(KEY,
                beat(granted.leaseId(), null), 3_000L);
        assertEquals(first.expiryDeadlineMs(), duplicate.expiryDeadlineMs());
        assertEquals(first.leaseId(), duplicate.leaseId());
    }

    @Test
    void unknownLeaseIdIsRejectedAndKeepsTheDeadline() {
        RegistrationLeaseProtocol.LeaseResponse granted = leaseService.onBeat(KEY, beat(null, null), 1_000L);
        RegistrationLeaseProtocol.LeaseResponse rejected = leaseService.onBeat(KEY, beat("foreign-lease", null), 3_000L);
        assertEquals(RegistrationLeaseProtocol.STALE_LEASE, rejected.code());
        assertNotEquals(granted.leaseId(), rejected.leaseId());
        assertEquals(granted.expiryDeadlineMs(), leaseService.offlineDeadlineMillis(KEY, 0L));
    }

    @Test
    void reRegistrationWithBlankLeaseIdAttachesWithoutResettingTheDeadline() {
        RegistrationLeaseProtocol.LeaseResponse granted = leaseService.onBeat(KEY, beat(null, null), 1_000L);
        leaseService.onBeat(KEY, beat("foreign-lease", null), 3_000L);
        RegistrationLeaseProtocol.LeaseResponse attached = leaseService.onBeat(KEY, beat(null, null), 4_000L);
        assertEquals(RegistrationLeaseProtocol.OK, attached.code());
        assertEquals(granted.expiryDeadlineMs(), attached.expiryDeadlineMs(),
                "a blank lease id must not reset the deadline of the running lease");
    }

    @Test
    void expiredLeaseAnswersExpiredAndSurvivesUntilReRegistration() {
        RegistrationLeaseProtocol.LeaseResponse granted = leaseService.onBeat(KEY, beat(null, null), 1_000L);
        RegistrationLeaseProtocol.LeaseResponse expired = leaseService.onBeat(KEY,
                beat(granted.leaseId(), null), 17_000L);
        assertEquals(RegistrationLeaseProtocol.EXPIRED, expired.code());
        RegistrationLeaseProtocol.LeaseResponse fresh = leaseService.onBeat(KEY, beat(null, null), 18_000L);
        assertEquals(RegistrationLeaseProtocol.OK, fresh.code());
        assertNotEquals(granted.leaseId(), fresh.leaseId());
        assertEquals(33_000L, fresh.expiryDeadlineMs());
    }

    @Test
    void proposalsAreClampedToTheNegotiableBounds() {
        RegistrationLeaseProtocol.LeaseResponse tooSmall = leaseService.onBeat(KEY, beat(null, 0), 1_000L);
        assertEquals(2, tooSmall.intervalSeconds());
        RegistrationLeaseProtocol.LeaseResponse tooLarge = leaseService.onBeat(KEY, beat(tooSmall.leaseId(), 1_000), 2_000L);
        assertEquals(60, tooLarge.intervalSeconds());
        RegistrationLeaseProtocol.LeaseResponse inBounds = leaseService.onBeat(KEY, beat(tooLarge.leaseId(), 10), 3_000L);
        assertEquals(10, inBounds.intervalSeconds());
    }

    @Test
    void offlineDeadlineFallsBackToLegacyValueWithoutALease() {
        assertEquals(123L, leaseService.offlineDeadlineMillis("unknown-key", 123L));
        assertTrue(leaseService.isEnabled());
    }

    @Test
    void adminRestartAnswersStaleThenAttachesTheFreshLease() {
        // before the restart the client held a lease; after the restart the map is empty
        InstanceLeaseService restartedService = new InstanceLeaseService(properties);
        RegistrationLeaseProtocol.LeaseResponse stale = restartedService.onBeat(KEY, beat("pre-restart-lease", null), 1_000L);
        assertEquals(RegistrationLeaseProtocol.STALE_LEASE, stale.code());

        // the stale beat already granted a fresh lease; the re-registration attaches to
        // it without resetting its deadline
        RegistrationLeaseProtocol.LeaseResponse attached = restartedService.onBeat(KEY, beat(null, null), 2_000L);
        assertEquals(RegistrationLeaseProtocol.OK, attached.code());
        assertEquals(16_000L, attached.expiryDeadlineMs(), "the attach must not reset the deadline");
    }

    @Test
    void misconfiguredBoundsFailFastWhenEnabled() {
        RegistrationLeaseProperties broken = new RegistrationLeaseProperties();
        broken.setEnabled(true);
        broken.setMinInterval(Duration.ofSeconds(10));
        broken.setMaxInterval(Duration.ofSeconds(5));
        assertThrows(IllegalStateException.class, () -> new InstanceLeaseService(broken));

        RegistrationLeaseProperties zeroFactor = new RegistrationLeaseProperties();
        zeroFactor.setEnabled(true);
        zeroFactor.setExpiryFactor(0);
        assertThrows(IllegalStateException.class, () -> new InstanceLeaseService(zeroFactor));
    }

    private InstanceBeatInfoDTO beat(final String leaseId, final Integer proposedIntervalSeconds) {
        InstanceBeatInfoDTO dto = new InstanceBeatInfoDTO();
        dto.setInstanceIp("10.0.0.1");
        dto.setInstancePort("8080");
        dto.setInstanceType("http");
        dto.setNamespaceId("default");
        dto.setLeaseId(leaseId);
        dto.setProposedIntervalSeconds(proposedIntervalSeconds);
        return dto;
    }
}
