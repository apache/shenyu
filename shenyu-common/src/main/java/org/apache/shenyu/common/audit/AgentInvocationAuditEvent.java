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

package org.apache.shenyu.common.audit;

import java.time.Instant;
import java.util.Objects;
import java.util.Optional;

/**
 * Immutable, credential-free invocation audit metadata. Prompts, arguments and response bodies are intentionally absent.
 */
public final class AgentInvocationAuditEvent {

    /**
     * Stable metadata contract version. Bump it only when fields or semantics change.
     */
    public static final int SCHEMA_VERSION = 1;

    private final RecordType recordType;

    private final String requestId;

    private final String tenant;

    private final String subject;

    private final String apiKeyId;

    private final String selectorId;

    private final String ruleId;

    private final String configurationVersion;

    private final String policyVersion;

    private final Decision decision;

    private final String target;

    private final String outcome;

    private final String reason;

    private final Integer httpStatus;

    private final Integer attempt;

    private final long elapsedMillis;

    private final Instant timestamp;

    private AgentInvocationAuditEvent(final Builder builder) {
        recordType = Objects.requireNonNull(builder.recordType, "recordType");
        requestId = text(builder.requestId, "requestId");
        tenant = nullableText(builder.tenant, "tenant");
        subject = nullableText(builder.subject, "subject");
        apiKeyId = nullableText(builder.apiKeyId, "apiKeyId");
        selectorId = nullableText(builder.selectorId, "selectorId");
        ruleId = nullableText(builder.ruleId, "ruleId");
        configurationVersion = text(builder.configurationVersion, "configurationVersion");
        policyVersion = text(builder.policyVersion, "policyVersion");
        decision = Objects.requireNonNull(builder.decision, "decision");
        target = nullableText(builder.target, "target");
        outcome = text(builder.outcome, "outcome");
        reason = text(builder.reason, "reason");
        httpStatus = Objects.isNull(builder.httpStatus) ? null : builder.httpStatus;
        attempt = Objects.isNull(builder.attempt) ? null : builder.attempt;
        if (Objects.nonNull(attempt) && attempt < 1) {
            throw new IllegalArgumentException("attempt must be positive");
        }
        elapsedMillis = builder.elapsedMillis;
        if (elapsedMillis < 0) {
            throw new IllegalArgumentException("elapsedMillis must not be negative");
        }
        timestamp = Objects.requireNonNullElseGet(builder.timestamp, Instant::now);
    }

    public static Builder logical() {
        return new Builder(RecordType.LOGICAL);
    }

    public static Builder attempt(final int attempt) {
        return new Builder(RecordType.ATTEMPT).attempt(attempt);
    }

    public RecordType getRecordType() {
        return recordType;
    }

    public String getRequestId() {
        return requestId;
    }

    public Optional<String> getTenant() {
        return Optional.ofNullable(tenant);
    }

    public Optional<String> getSubject() {
        return Optional.ofNullable(subject);
    }

    public Optional<String> getApiKeyId() {
        return Optional.ofNullable(apiKeyId);
    }

    public int getSchemaVersion() {
        return SCHEMA_VERSION;
    }

    public Optional<String> getSelectorId() {
        return Optional.ofNullable(selectorId);
    }

    public Optional<String> getRuleId() {
        return Optional.ofNullable(ruleId);
    }

    public String getConfigurationVersion() {
        return configurationVersion;
    }

    public String getPolicyVersion() {
        return policyVersion;
    }

    public Decision getDecision() {
        return decision;
    }

    public Optional<String> getTarget() {
        return Optional.ofNullable(target);
    }

    public String getOutcome() {
        return outcome;
    }

    public String getReason() {
        return reason;
    }

    public Optional<Integer> getHttpStatus() {
        return Optional.ofNullable(httpStatus);
    }

    public Optional<Integer> getAttempt() {
        return Optional.ofNullable(attempt);
    }

    public long getElapsedMillis() {
        return elapsedMillis;
    }

    public Instant getTimestamp() {
        return timestamp;
    }

    private static String text(final String value, final String name) {
        String result = nullableText(value, name);
        return Objects.requireNonNull(result, name);
    }

    static String nullableText(final String value, final String name) {
        if (Objects.isNull(value)) {
            return null;
        }
        if (value.isBlank() || value.length() > 256 || !value.equals(value.strip()) || value.chars().anyMatch(Character::isISOControl)) {
            throw new IllegalArgumentException(name + " must be bounded nonblank text");
        }
        return value;
    }

    public enum RecordType {
        LOGICAL,
        ATTEMPT
    }

    public enum Decision {
        ALLOWED,
        REJECTED
    }

    public static final class Builder {

        private final RecordType recordType;

        private String requestId;

        private String tenant;

        private String subject;

        private String apiKeyId;

        private String selectorId;

        private String ruleId;

        private String configurationVersion = "unknown";

        private String policyVersion = "unknown";

        private Decision decision = Decision.ALLOWED;

        private String target;

        private String outcome;

        private String reason = "UNKNOWN";

        private Integer httpStatus;

        private Integer attempt;

        private long elapsedMillis;

        private Instant timestamp;

        private Builder(final RecordType recordType) {
            this.recordType = recordType;
        }

        public Builder requestId(final String value) {
            requestId = value;
            return this;
        }

        public Builder tenant(final String value) {
            tenant = value;
            return this;
        }

        public Builder subject(final String value) {
            subject = value;
            return this;
        }

        public Builder apiKeyId(final String value) {
            apiKeyId = value;
            return this;
        }

        public Builder selectorId(final String value) {
            selectorId = value;
            return this;
        }

        public Builder ruleId(final String value) {
            ruleId = value;
            return this;
        }

        public Builder configurationVersion(final long value) {
            configurationVersion = Long.toString(value);
            return this;
        }

        public Builder configurationVersion(final String value) {
            configurationVersion = value;
            return this;
        }

        public Builder policyVersion(final String value) {
            policyVersion = value;
            return this;
        }

        public Builder decision(final Decision value) {
            decision = value;
            return this;
        }

        public Builder target(final String value) {
            target = value;
            return this;
        }

        public Builder outcome(final String value) {
            outcome = value;
            return this;
        }

        public Builder reason(final String value) {
            reason = value;
            return this;
        }

        public Builder httpStatus(final Integer value) {
            httpStatus = value;
            return this;
        }

        private Builder attempt(final Integer value) {
            attempt = value;
            return this;
        }

        public Builder elapsedMillis(final long value) {
            elapsedMillis = value;
            return this;
        }

        public Builder timestamp(final Instant value) {
            timestamp = value;
            return this;
        }

        public AgentInvocationAuditEvent build() {
            return new AgentInvocationAuditEvent(this);
        }
    }
}
