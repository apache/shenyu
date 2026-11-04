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

package org.apache.shenyu.admin.security;

import org.apache.shenyu.admin.utils.SecurityAuditUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;

import java.util.Locale;

/**
 * Single emission point for structured security audit records. Every record is a single-line
 * {@code SECURITY_AUDIT} event with fixed key=value fields in a fixed order; user-controlled
 * values are sanitized, missing values use explicit safe placeholders, and emission is bounded
 * by a fixed-slot {@link SecurityAuditRateLimiter}. Successful operations are logged at INFO,
 * failures and denials at WARN, so under a saturated async appender queue failures survive
 * while successes may be dropped by the logging framework. The audit logger is a plain SLF4J
 * logger so operators can route it to a dedicated appender without code changes.
 */
public final class SecurityAuditLog {

    /**
     * name of the dedicated audit logger.
     */
    public static final String LOGGER_NAME = "org.apache.shenyu.admin.security.AUDIT";

    /**
     * MDC key carrying the current request id, populated by {@link RequestIdFilter}.
     */
    public static final String REQUEST_ID_MDC_KEY = "requestId";

    private static final Logger AUDIT = LoggerFactory.getLogger(LOGGER_NAME);

    private static final String PREFIX = "SECURITY_AUDIT";

    private static final String PLACEHOLDER = "n/a";

    private static final String NO_REQUEST = "no-request";

    private static volatile SecurityAuditRateLimiter rateLimiter = new SecurityAuditRateLimiter();

    private SecurityAuditLog() {
    }

    /**
     * audit outcome of the recorded operation.
     */
    public enum Outcome {

        /**
         * the operation succeeded.
         */
        SUCCESS,

        /**
         * the operation failed.
         */
        FAILURE,

        /**
         * the operation was denied by an access control decision.
         */
        DENIED
    }

    /**
     * start building an audit event of the given type.
     *
     * @param eventType event type
     * @return event builder
     */
    public static EventBuilder event(final SecurityAuditEventType eventType) {
        return new EventBuilder(eventType);
    }

    /**
     * replace the rate limiter. Intended for tests.
     *
     * @param limiter rate limiter to install
     */
    public static void setRateLimiter(final SecurityAuditRateLimiter limiter) {
        rateLimiter = limiter;
    }

    /**
     * restore a fresh default rate limiter. Intended for tests.
     */
    public static void resetRateLimiter() {
        rateLimiter = new SecurityAuditRateLimiter();
    }

    private static void emitSuppressedSummary(final SecurityAuditEventType suppressedType, final long suppressedTotal) {
        SecurityAuditRateLimiter.Acquisition acquisition = rateLimiter.tryAcquire(SecurityAuditEventType.AUDIT_SUPPRESSED);
        // the suppression slot has its own quota, so summary lines stay bounded even under a
        // sustained flood; a rollover flush of the suppression slot itself is dropped on purpose
        // because summaries are already bounded to at most one per window per event type.
        if (!acquisition.isAllowed()) {
            return;
        }
        AUDIT.warn(PREFIX
                + " eventType=" + SecurityAuditEventType.AUDIT_SUPPRESSED.getCode()
                + " outcome=" + PLACEHOLDER
                + " action=rate_limit"
                + " reason=suppressed"
                + " suppressedEventType=" + suppressedType.getCode()
                + " suppressedCount=" + suppressedTotal
                + " requestId=" + requestId());
    }

    private static String requestId() {
        final String value = SecurityAuditUtils.sanitize(MDC.get(REQUEST_ID_MDC_KEY));
        return value.isEmpty() ? NO_REQUEST : value;
    }

    private static String value(final String raw) {
        final String value = SecurityAuditUtils.sanitize(raw);
        return value.isEmpty() ? PLACEHOLDER : value;
    }

    /**
     * builder for one audit event. All values are sanitized at emission time; unset fields fall
     * back to explicit safe placeholders and no identity is ever fabricated.
     */
    public static final class EventBuilder {

        private final SecurityAuditEventType eventType;

        private Outcome outcome = Outcome.FAILURE;

        private String action = PLACEHOLDER;

        private String reason = PLACEHOLDER;

        private String actor = "unknown";

        private String submittedIdentity = PLACEHOLDER;

        private String namespace = PLACEHOLDER;

        private String resource = PLACEHOLDER;

        private String remoteAddr = PLACEHOLDER;

        private String fingerprint = PLACEHOLDER;

        private EventBuilder(final SecurityAuditEventType eventType) {
            this.eventType = eventType;
        }

        /**
         * set the outcome.
         *
         * @param newOutcome outcome
         * @return this builder
         */
        public EventBuilder outcome(final Outcome newOutcome) {
            this.outcome = newOutcome;
            return this;
        }

        /**
         * set the fixed action code, for example {@code login} or {@code permission_check}.
         *
         * @param newAction action code
         * @return this builder
         */
        public EventBuilder action(final String newAction) {
            this.action = newAction;
            return this;
        }

        /**
         * set the fixed reason category, never an exception message.
         *
         * @param newReason reason category
         * @return this builder
         */
        public EventBuilder reason(final String newReason) {
            this.reason = newReason;
            return this;
        }

        /**
         * set the actor. Pass a verified identity only; for unauthenticated requests use an
         * explicit placeholder such as {@code unauthenticated}.
         *
         * @param newActor actor name
         * @return this builder
         */
        public EventBuilder actor(final String newActor) {
            this.actor = newActor;
            return this;
        }

        /**
         * set the identity the client submitted (for example a login user name). It is sanitized
         * and length-bounded, and stays clearly separated from the verified actor.
         *
         * @param newSubmittedIdentity submitted identity
         * @return this builder
         */
        public EventBuilder submittedIdentity(final String newSubmittedIdentity) {
            this.submittedIdentity = newSubmittedIdentity;
            return this;
        }

        /**
         * set the namespace id, when the event belongs to a namespaced flow.
         *
         * @param newNamespace namespace id
         * @return this builder
         */
        public EventBuilder namespace(final String newNamespace) {
            this.namespace = newNamespace;
            return this;
        }

        /**
         * set the resource, typically the request URI (without query string) or an auth id.
         *
         * @param newResource resource identifier
         * @return this builder
         */
        public EventBuilder resource(final String newResource) {
            this.resource = newResource;
            return this;
        }

        /**
         * set the remote address of the TCP peer.
         *
         * @param newRemoteAddr remote address
         * @return this builder
         */
        public EventBuilder remoteAddr(final String newRemoteAddr) {
            this.remoteAddr = newRemoteAddr;
            return this;
        }

        /**
         * set the truncated credential fingerprint built by
         * {@link SecurityAuditUtils#fingerprint(String)}.
         *
         * @param newFingerprint truncated fingerprint
         * @return this builder
         */
        public EventBuilder fingerprint(final String newFingerprint) {
            this.fingerprint = newFingerprint;
            return this;
        }

        /**
         * apply the rate limiter and write the audit line. Suppressed windows are reported once
         * per window per event type by a bounded summary line before the next emitted event.
         */
        public void emit() {
            final SecurityAuditRateLimiter.Acquisition acquisition = rateLimiter.tryAcquire(eventType);
            if (acquisition.getFlushedSuppressed() > 0L) {
                emitSuppressedSummary(eventType, acquisition.getFlushedSuppressed());
            }
            if (!acquisition.isAllowed()) {
                return;
            }
            final String line = PREFIX
                    + " eventType=" + eventType.getCode()
                    + " outcome=" + outcome.name().toLowerCase(Locale.ROOT)
                    + " action=" + value(action)
                    + " reason=" + value(reason)
                    + " actor=" + value(actor)
                    + " submittedIdentity=" + value(submittedIdentity)
                    + " namespace=" + value(namespace)
                    + " resource=" + value(resource)
                    + " remoteAddr=" + value(remoteAddr)
                    + " requestId=" + requestId()
                    + " fingerprint=" + value(fingerprint);
            if (Outcome.SUCCESS == outcome) {
                AUDIT.info(line);
            } else {
                AUDIT.warn(line);
            }
        }
    }
}
