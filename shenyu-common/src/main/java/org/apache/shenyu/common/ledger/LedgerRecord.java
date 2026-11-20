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

package org.apache.shenyu.common.ledger;

import java.math.BigDecimal;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Instant;
import java.util.HexFormat;
import java.util.Map;
import java.util.Objects;

/**
 * Immutable body-free ledger evidence. Only ATTEMPT rows contribute original usage/cost.
 * @param id bounded idempotency key
 * @param kind record purpose
 * @param originalId original attempt for an explicit revaluation
 * @param valuationId operator supplied revaluation revision
 * @param invocationId internal logical execution identity, never a trace identity
 * @param attemptNumber actual attempt sequence, zero for logical summaries
 * @param phase actual primary, retry or fallback selection
 * @param observedAt gateway observation time, not provider billing time
 * @param recordedAt record creation time
 * @param facts trusted attribution; absent facts stay unknown
 * @param usage normalized upstream quantities, not reservation estimates
 * @param estimate operator-price estimate, never a provider invoice
 * @param outcome semantic terminal outcome
 * @param httpStatus assigned or received HTTP status
 * @param expectedAttempts logical attempt coverage, not another debit
 * @param schemaVersion ledger evidence schema, currently 1
 * @param streaming whether the source attempt streamed; the shared contract does not certify a final usage frame
 */
public record LedgerRecord(String id, Kind kind, String originalId, String valuationId, String invocationId,
                            int attemptNumber, String phase, Instant observedAt, Instant recordedAt, Facts facts,
                            Usage usage, Estimate estimate, String outcome, Integer httpStatus, int expectedAttempts, int schemaVersion, boolean streaming) {

    public LedgerRecord {
        required(id);
        required(invocationId);
        Objects.requireNonNull(kind, "kind");
        Objects.requireNonNull(observedAt, "observedAt");
        Objects.requireNonNull(recordedAt, "recordedAt");
        Objects.requireNonNull(facts, "facts");
        Objects.requireNonNull(usage, "usage");
        Objects.requireNonNull(estimate, "estimate");
        required(outcome);
        optional(phase);
        optional(originalId);
        optional(valuationId);
        if (schemaVersion != 1 || attemptNumber < 0 || expectedAttempts < 0
                || kind == Kind.LOGICAL && attemptNumber != 0
                || kind != Kind.LOGICAL && attemptNumber < 1
                || kind == Kind.REVALUATION && (Objects.isNull(originalId) || Objects.isNull(valuationId))) {
            throw new IllegalArgumentException("Invalid ledger sequence or reference");
        }
    }

    /**
     * Compare delivered facts, ignoring observation/receipt times and later price configuration.
     * @param other duplicate candidate
     * @return whether the same original fact was redelivered
     */
    public boolean sameEvidence(final LedgerRecord other) {
        return kind == other.kind && Objects.equals(invocationId, other.invocationId)
                && attemptNumber == other.attemptNumber && Objects.equals(phase, other.phase)
                && Objects.equals(facts, other.facts) && Objects.equals(usage, other.usage)
                && Objects.equals(outcome, other.outcome) && Objects.equals(httpStatus, other.httpStatus)
                && expectedAttempts == other.expectedAttempts && schemaVersion == other.schemaVersion && streaming == other.streaming
                && Objects.equals(originalId, other.originalId)
                && Objects.equals(valuationId, other.valuationId)
                && (kind != Kind.REVALUATION || Objects.equals(estimate, other.estimate));
    }

    /**
     * Hash length-prefixed components, avoiding delimiter collisions and exposing no identities.
     * @param values internal key components
     * @return fixed-size key
     */
    public static String key(final String... values) {
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            for (String value : values) {
                byte[] bytes = Objects.requireNonNullElse(value, "").getBytes(StandardCharsets.UTF_8);
                digest.update(ByteBuffer.allocate(Integer.BYTES).putInt(bytes.length).array());
                digest.update(bytes);
            }
            return HexFormat.of().formatHex(digest.digest());
        } catch (NoSuchAlgorithmException error) {
            throw new IllegalStateException("SHA-256 is unavailable", error);
        }
    }

    /**
     * Validate a required bounded metadata fact without including its value in errors.
     * @param value metadata fact
     * @return validated fact
     */
    public static String required(final String value) {
        if (Objects.isNull(value)) {
            throw new IllegalArgumentException("Required ledger fact is unavailable");
        }
        return optional(value);
    }

    /**
     * Validate an optional bounded metadata fact without inventing a default.
     * @param value nullable metadata fact
     * @return original fact or null
     */
    public static String optional(final String value) {
        if (Objects.nonNull(value) && (value.isBlank() || value.length() > 256 || !value.equals(value.strip())
                || value.chars().anyMatch(Character::isISOControl) || !StandardCharsets.UTF_8.newEncoder().canEncode(value))) {
            throw new IllegalArgumentException("Invalid bounded ledger fact");
        }
        return value;
    }

    /** Record ownership and counting purpose. */
    public enum Kind {
        ATTEMPT, LOGICAL, REVALUATION
    }

    /** Observation provenance, separate from the monetary estimate. */
    public enum Origin {
        PROVIDER_REPORTED, GATEWAY_ESTIMATE, UNKNOWN
    }

    /** Observation completeness, including known zero as REPORTED with zero quantities. */
    public enum Quality {
        REPORTED, ESTIMATED, UNKNOWN, INVALID, UNSUPPORTED
    }

    /** Monetary coverage, never an invoice status. */
    public enum PriceStatus {
        ESTIMATED, UNKNOWN_USAGE, UNKNOWN_PRICE, UNSUPPORTED_CATEGORY, NOT_APPLICABLE
    }

    /**
     * Trusted bounded attribution. No wire credentials, arguments, prompts or outputs belong here.
     * @param namespace authoritative namespace
     * @param tenant authenticated tenant
     * @param subject authenticated subject
     * @param application authorized application
     * @param feature server-owned feature mapping
     * @param apiKeyId internal key identifier only
     * @param provider actual selected provider identifier
     * @param requestedModel original model when known
     * @param resolvedModel actual selected model when known
     * @param clientProtocol actual ingress protocol
     * @param nativeProtocol actual provider protocol when known
     * @param operation actual operation
     * @param configurationVersion node-local configuration generation, not a price revision
     */
    public record Facts(String namespace, String tenant, String subject, String application, String feature,
                         String apiKeyId, String provider, String requestedModel, String resolvedModel,
                         String clientProtocol, String nativeProtocol, String operation, Long configurationVersion) {
        public Facts {
            for (String value : new String[]{namespace, tenant, subject, application, feature, apiKeyId,
                provider, requestedModel, resolvedModel, clientProtocol, nativeProtocol, operation}) {
                optional(value);
            }
            if (Objects.nonNull(configurationVersion) && configurationVersion < 0) {
                throw new IllegalArgumentException("Invalid configuration generation");
            }
        }
    }

    /**
     * One final normalized attempt snapshot. Categories never become additional summed totals.
     * @param inputTokens reported input
     * @param outputTokens reported output
     * @param totalTokens reported total; do not derive unknown categories from this total
     * @param cachedInputTokens reported cache category
     * @param categories bounded original normalized categories
     * @param quality observation quality
     * @param origin quantity provenance
     */
    public record Usage(Long inputTokens, Long outputTokens, Long totalTokens, Long cachedInputTokens,
                         Map<String, Long> categories, Quality quality, Origin origin) {
        public Usage {
            Objects.requireNonNull(quality, "quality");
            Objects.requireNonNull(origin, "origin");
            if (quality == Quality.REPORTED && origin != Origin.PROVIDER_REPORTED
                    || quality == Quality.ESTIMATED && origin != Origin.GATEWAY_ESTIMATE
                    || quality != Quality.REPORTED && quality != Quality.ESTIMATED && origin != Origin.UNKNOWN) {
                throw new IllegalArgumentException("Usage origin disagrees with quality");
            }
            categories = Map.copyOf(categories);
            if (categories.size() > 32) {
                throw new IllegalArgumentException("Too many usage categories");
            }
            for (Long value : new Long[]{inputTokens, outputTokens, totalTokens, cachedInputTokens}) {
                if (Objects.nonNull(value) && value < 0) {
                    throw new IllegalArgumentException("Negative usage");
                }
            }
            categories.forEach((name, value) -> {
                required(name);
                if (Objects.isNull(value) || value < 0) {
                    throw new IllegalArgumentException("Invalid usage category");
                }
            });
            if (quality != Quality.REPORTED && quality != Quality.ESTIMATED
                    && (Objects.nonNull(inputTokens) || Objects.nonNull(outputTokens) || Objects.nonNull(totalTokens)
                    || Objects.nonNull(cachedInputTokens) || !categories.isEmpty())) {
                throw new IllegalArgumentException("Unavailable usage cannot carry invented quantities");
            }
        }

        /**
         * Explicitly unavailable observation.
         * @param quality bounded unavailable reason
         * @return empty quantities, never synthetic zero
         */
        public static Usage unavailable(final Quality quality) {
            return new Usage(null, null, null, null, Map.of(), quality, Origin.UNKNOWN);
        }
    }

    /**
     * Frozen operator-price provenance for an estimate.
     * @param status valuation coverage
     * @param amount nullable estimate, never a reserved debit
     * @param currency configured currency, no implicit FX
     * @param priceVersion immutable schedule revision
     * @param priceSource operator-owned provenance
     * @param effectiveFrom inclusive price effective time
     * @param effectiveUntil exclusive price end
     * @param asOf explicit lookup time
     * @param inputPerMillion frozen input unit price
     * @param outputPerMillion frozen output unit price
     */
    public record Estimate(PriceStatus status, BigDecimal amount, String currency, String priceVersion,
                            String priceSource, Instant effectiveFrom, Instant effectiveUntil, Instant asOf,
                            BigDecimal inputPerMillion, BigDecimal outputPerMillion) {
        public Estimate {
            Objects.requireNonNull(status, "status");
            Objects.requireNonNull(asOf, "asOf");
            if (status == PriceStatus.ESTIMATED && (Objects.isNull(amount) || amount.signum() < 0
                    || Objects.isNull(currency) || Objects.isNull(priceVersion) || Objects.isNull(priceSource) || Objects.isNull(effectiveFrom)
                    || Objects.isNull(inputPerMillion) || Objects.isNull(outputPerMillion))
                    || status != PriceStatus.ESTIMATED && Objects.nonNull(amount)) {
                throw new IllegalArgumentException("Invalid estimate coverage");
            }
        }

        /**
         * Unknown or inapplicable monetary value.
         * @param status missing valuation reason
         * @param asOf lookup time
         * @return a null amount
         */
        public static Estimate unavailable(final PriceStatus status, final Instant asOf) {
            return new Estimate(status, null, null, null, null, null, null, asOf, null, null);
        }
    }
}
