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

package org.apache.shenyu.register.common.protocol;

/**
 * Wire contract for the opt-in server-issued registration lease.
 *
 * <p>The lease rides the existing {@code /instance/beat} endpoint owned by
 * {@link RegistrationProtocol}: a capability-aware client adds the optional lease
 * fields to the beat payload and receives a structured lease response, while legacy
 * clients keep the exact v1 string response. The lease only advances instance
 * liveness; readiness, fencing and health-probe outcomes always take precedence over
 * a successful renewal.</p>
 */
public final class RegistrationLeaseProtocol {

    /** Capability advertised only by an Admin that enabled the lease feature. */
    public static final String CAPABILITY = "registration-lease-v1";

    /** Stable success code: the lease was granted or renewed. */
    public static final String OK = "OK";

    /** The presented lease id is unknown to this cluster; the client must re-register. */
    public static final String STALE_LEASE = "STALE_LEASE";

    /** The presented lease already expired; the client must re-register. */
    public static final String EXPIRED = "EXPIRED";

    /** The beat was rejected for an incomplete identity (legacy semantics). */
    public static final String INVALID_REQUEST = "INVALID_REQUEST";

    /** The lease is not active on this Admin cluster; the client falls back to v1. */
    public static final String LEASE_NOT_ENABLED = "LEASE_NOT_ENABLED";

    private RegistrationLeaseProtocol() {
    }

    /**
     * Structured response returned to capability-aware beat requests. Serialized only
     * when the request presents the lease capability; every other request keeps the
     * legacy plain-text response.
     *
     * @param code stable response code ({@code OK}, {@code STALE_LEASE},
     *             {@code EXPIRED}, {@code INVALID_REQUEST})
     * @param leaseId server-issued lease identity, present on {@code OK}
     * @param intervalSeconds server-decided heartbeat interval in seconds; the client
     *                        reschedules its reporter to this value
     * @param expiryDeadlineMs absolute epoch millis when the server will consider the
     *                        instance offline without a further accepted renewal
     * @param message diagnostic message without credentials
     */
    public record LeaseResponse(String code, String leaseId, Integer intervalSeconds,
                                Long expiryDeadlineMs, String message) {

        /**
         * Build a success response.
         *
         * @param leaseId server-issued lease identity
         * @param intervalSeconds decided heartbeat interval
         * @param expiryDeadlineMs absolute expiry deadline
         * @return success response
         */
        public static LeaseResponse ok(final String leaseId, final int intervalSeconds, final long expiryDeadlineMs) {
            return new LeaseResponse(OK, leaseId, intervalSeconds, expiryDeadlineMs, OK);
        }

        /**
         * Build a rejection response.
         *
         * @param code stable error code
         * @param message safe message
         * @return rejection response
         */
        public static LeaseResponse error(final String code, final String message) {
            return new LeaseResponse(code, null, null, null, message);
        }
    }
}
