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
import org.springframework.stereotype.Component;

import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Server-issued registration leases for instance liveness
 * ({@code registration-lease-v1}).
 *
 * <p>Every leased instance holds one immutable record keyed by the same instance key
 * the liveness map uses. A beat renews its lease by replacing the record atomically:
 * the new deadline is the maximum of the previous deadline and {@code receipt +
 * expiryFactor * interval}, so duplicate, delayed or replayed renewal beats are
 * idempotent and can never push the deadline past the last server receipt. A beat
 * that presents an unknown lease id is rejected with {@code STALE_LEASE}; a renewal
 * of an already expired lease answers {@code EXPIRED}. Both are terminal for the
 * presented lease: the client drops it and re-registers with a blank lease id, which
 * grants a fresh lease.</p>
 *
 * <p>The lease state lives only in memory of the Admin that owns the liveness checks
 * (the cluster master, which receives forwarded beats and rebuilds from the database
 * on failover). An Admin restart therefore invalidates every outstanding lease;
 * clients recover by re-registering, which is the same path an expired lease takes.
 * Fenced business addresses are owned by boot-session fencing and are out of lease
 * scope.</p>
 */
@Component
public class InstanceLeaseService {

    private final RegistrationLeaseProperties properties;

    private final ConcurrentHashMap<String, LeaseRecord> leases = new ConcurrentHashMap<>();

    public InstanceLeaseService(final RegistrationLeaseProperties properties) {
        this.properties = properties;
        if (properties.isEnabled()) {
            // a misconfigured bound (min > max) or a non-positive expiry factor would
            // expire every lease immediately and mark every instance offline, so the
            // deployment must fail fast instead
            if (properties.getMinInterval().isZero() || properties.getMinInterval().isNegative()
                    || properties.getMinInterval().compareTo(properties.getMaxInterval()) > 0
                    || properties.getExpiryFactor() < 1) {
                throw new IllegalStateException("Invalid shenyu.register.lease configuration: "
                        + "minInterval must be positive and not greater than maxInterval, "
                        + "expiryFactor must be at least 1");
            }
        }
    }

    /**
     * Whether the lease feature is enabled. When false, callers must keep the exact
     * legacy v1 beat semantics.
     *
     * @return whether enabled
     */
    public boolean isEnabled() {
        return properties.isEnabled();
    }

    /**
     * Grant or renew the lease of one instance for a lease-capable beat.
     *
     * @param instanceKey the instance key of the beat
     * @param beat the beat payload carrying an optional lease id and proposed interval
     * @param nowMillis the server receipt time
     * @return the wire response for the beat
     */
    public RegistrationLeaseProtocol.LeaseResponse onBeat(final String instanceKey, final InstanceBeatInfoDTO beat,
                                                          final long nowMillis) {
        final String presentedLeaseId = beat.getLeaseId();
        final int intervalSeconds = decideIntervalSeconds(beat.getProposedIntervalSeconds());
        final long ttlMillis = properties.getExpiryFactor() * intervalSeconds * 1000L;
        final LeaseRecord updated = leases.compute(instanceKey, (key, existing) -> {
            if (Objects.isNull(existing)) {
                return LeaseRecord.grant(UUID.randomUUID().toString(), intervalSeconds, nowMillis, ttlMillis);
            }
            if (Objects.isNull(presentedLeaseId) && existing.isExpired(nowMillis)) {
                // an explicit re-registration recovers an expired lease with a fresh one;
                // a live lease keeps its deadline (a blank beat must not extend it)
                return LeaseRecord.grant(UUID.randomUUID().toString(), intervalSeconds, nowMillis, ttlMillis);
            }
            if (!Objects.equals(existing.leaseId(), presentedLeaseId)) {
                // keep the record: a replay of the replaced (or foreign) lease must stay rejected
                return existing;
            }
            if (existing.isExpired(nowMillis)) {
                return existing;
            }
            return existing.renew(intervalSeconds, nowMillis, ttlMillis);
        });
        if (Objects.equals(updated.leaseId(), presentedLeaseId) && updated.isExpired(nowMillis)) {
            return RegistrationLeaseProtocol.LeaseResponse.error(RegistrationLeaseProtocol.EXPIRED,
                    "lease expired, re-register without a lease id");
        }
        if (Objects.nonNull(presentedLeaseId) && !Objects.equals(updated.leaseId(), presentedLeaseId)) {
            return RegistrationLeaseProtocol.LeaseResponse.error(RegistrationLeaseProtocol.STALE_LEASE,
                    "unknown lease id, re-register without a lease id");
        }
        return RegistrationLeaseProtocol.LeaseResponse.ok(updated.leaseId(), updated.intervalSeconds(),
                updated.deadlineMillis());
    }

    /**
     * The offline deadline of one instance: its lease deadline when leased, otherwise
     * the legacy heartbeat timeout counted from the last heartbeat.
     *
     * @param instanceKey the instance key
     * @param legacyDeadlineMillis the legacy deadline derived from the last heartbeat
     * @return the absolute offline deadline in epoch millis
     */
    public long offlineDeadlineMillis(final String instanceKey, final long legacyDeadlineMillis) {
        LeaseRecord record = leases.get(instanceKey);
        return Objects.isNull(record) ? legacyDeadlineMillis : record.deadlineMillis();
    }

    /**
     * Drop the lease of one removed instance.
     *
     * @param instanceKey the instance key
     */
    public void remove(final String instanceKey) {
        leases.remove(instanceKey);
    }

    private int decideIntervalSeconds(final Integer proposedIntervalSeconds) {
        long minMillis = properties.getMinInterval().toMillis();
        long maxMillis = properties.getMaxInterval().toMillis();
        long proposed = Objects.isNull(proposedIntervalSeconds)
                ? properties.getInterval().toMillis() : proposedIntervalSeconds * 1000L;
        long clamped = Math.max(minMillis, Math.min(maxMillis, proposed));
        return (int) (clamped / 1000L);
    }

    /**
     * One immutable lease record; renewals replace it atomically through
     * {@code ConcurrentHashMap.compute}.
     */
    private record LeaseRecord(String leaseId, int intervalSeconds, long deadlineMillis) {

        static LeaseRecord grant(final String leaseId, final int intervalSeconds, final long nowMillis,
                                 final long ttlMillis) {
            return new LeaseRecord(leaseId, intervalSeconds, nowMillis + ttlMillis);
        }

        LeaseRecord renew(final int newIntervalSeconds, final long nowMillis, final long ttlMillis) {
            return new LeaseRecord(leaseId, newIntervalSeconds,
                    Math.max(deadlineMillis, nowMillis + ttlMillis));
        }

        boolean isExpired(final long nowMillis) {
            return nowMillis > deadlineMillis;
        }
    }
}
