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

package org.apache.shenyu.common.governance;

import java.time.Instant;
import java.util.Objects;

/**
 * Immutable shadow/enforce decision shared by MCP quota and LLM budget consumers.
 * Tenant, subject and trace id are intentionally absent: they are correlation facts,
 * not bounded metric labels.
 */
public final class AgentGovernanceDecision {

    public static final int SCHEMA_VERSION = 1;

    private final Consumer consumer;

    private final String policyVersion;

    private final String configurationVersion;

    private final Mode mode;

    private final Result result;

    private final Reason reason;

    private final String correlationId;

    private final String ledgerGeneration;

    private final Instant effectiveTime;

    private AgentGovernanceDecision(final Builder builder) {
        consumer = Objects.requireNonNull(builder.consumer, "consumer");
        policyVersion = boundedText(builder.policyVersion, "policyVersion");
        configurationVersion = boundedText(builder.configurationVersion, "configurationVersion");
        mode = Objects.requireNonNull(builder.mode, "mode");
        reason = Objects.requireNonNull(builder.reason, "reason");
        correlationId = boundedText(builder.correlationId, "correlationId");
        ledgerGeneration = boundedText(builder.ledgerGeneration, "ledgerGeneration");
        effectiveTime = Objects.requireNonNull(builder.effectiveTime, "effectiveTime");
        result = normalizeResult(Objects.requireNonNull(builder.result, "result"), mode, reason);
    }

    public static Builder builder(final Consumer consumer, final Mode mode) {
        return new Builder(consumer, mode);
    }

    public int getSchemaVersion() {
        return SCHEMA_VERSION;
    }

    public Consumer getConsumer() {
        return consumer;
    }

    public String getPolicyVersion() {
        return policyVersion;
    }

    public String getConfigurationVersion() {
        return configurationVersion;
    }

    public Mode getMode() {
        return mode;
    }

    public Result getResult() {
        return result;
    }

    public Reason getReason() {
        return reason;
    }

    public String getCorrelationId() {
        return correlationId;
    }

    public String getLedgerGeneration() {
        return ledgerGeneration;
    }

    public Instant getEffectiveTime() {
        return effectiveTime;
    }

    public boolean deniesTraffic() {
        return result == Result.DENY;
    }

    private static Result normalizeResult(final Result candidate, final Mode mode, final Reason reason) {
        if (reason == Reason.ALLOWED) {
            if (candidate != Result.ALLOW) {
                throw new IllegalArgumentException("ALLOWED reason requires ALLOW result");
            }
            return Result.ALLOW;
        }
        if (candidate == Result.ALLOW && reason == Reason.POLICY_ERROR) {
            // A fail-open adapter explicitly reports an unavailable policy backend while
            // allowing traffic. It is not an unexplained allow-after-deny.
            return Result.ALLOW;
        }
        if (candidate == Result.ALLOW) {
            throw new IllegalArgumentException("deny reason cannot produce ALLOW result");
        }
        if (candidate == Result.WOULD_DENY && mode != Mode.SHADOW) {
            throw new IllegalArgumentException("WOULD_DENY is valid only in shadow mode");
        }
        if (candidate == Result.DENY && mode != Mode.ENFORCE) {
            throw new IllegalArgumentException("DENY is valid only in enforce mode");
        }
        return candidate;
    }

    private static String boundedText(final String value, final String name) {
        if (Objects.isNull(value) || value.isBlank() || value.length() > 128
                || !value.equals(value.strip()) || value.chars().anyMatch(Character::isISOControl)) {
            throw new IllegalArgumentException(name + " must be bounded nonblank text");
        }
        return value;
    }

    public enum Consumer {
        MCP_QUOTA,
        LLM_BUDGET
    }

    public enum Mode {
        SHADOW,
        ENFORCE
    }

    public enum Result {
        ALLOW,
        WOULD_DENY,
        DENY
    }

    public enum Reason {
        ALLOWED,
        MCP_INVOCATION_LIMIT,
        MCP_CONCURRENCY_LIMIT,
        TOKEN_BUDGET_EXCEEDED,
        COST_BUDGET_EXCEEDED,
        RESERVATION_EXPIRED,
        AUTHENTICATION_REJECTED,
        AUTHORIZATION_REJECTED,
        RESOURCE_LIMIT_REJECTED,
        POLICY_ERROR
    }

    public static final class Builder {

        private final Consumer consumer;

        private final Mode mode;

        private String policyVersion = "unknown";

        private String configurationVersion = "unknown";

        private Result result;

        private Reason reason;

        private String correlationId;

        private String ledgerGeneration = "unknown";

        private Instant effectiveTime;

        private Builder(final Consumer consumer, final Mode mode) {
            this.consumer = consumer;
            this.mode = mode;
        }

        public Builder policyVersion(final String value) {
            policyVersion = value;
            return this;
        }

        public Builder configurationVersion(final String value) {
            configurationVersion = value;
            return this;
        }

        public Builder result(final Result value) {
            result = value;
            return this;
        }

        public Builder reason(final Reason value) {
            reason = value;
            return this;
        }

        public Builder correlationId(final String value) {
            correlationId = value;
            return this;
        }

        public Builder ledgerGeneration(final String value) {
            ledgerGeneration = value;
            return this;
        }

        public Builder effectiveTime(final Instant value) {
            effectiveTime = value;
            return this;
        }

        public AgentGovernanceDecision build() {
            effectiveTime = Objects.requireNonNullElseGet(effectiveTime, Instant::now);
            return new AgentGovernanceDecision(this);
        }
    }
}
