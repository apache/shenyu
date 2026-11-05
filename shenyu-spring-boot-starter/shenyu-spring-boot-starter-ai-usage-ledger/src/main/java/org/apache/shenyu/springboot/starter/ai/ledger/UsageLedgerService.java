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

package org.apache.shenyu.springboot.starter.ai.ledger;

import org.apache.shenyu.common.ledger.LedgerPriceBook;
import org.apache.shenyu.common.ledger.LedgerRecord;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.Clock;
import java.time.Instant;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/** Authorized bounded read/estimate operations. Does not debit or settle any reservation. */
public final class UsageLedgerService {

    private final UsageLedgerStore store;

    private final LedgerPriceBook prices;

    private final Clock clock;

    public UsageLedgerService(final UsageLedgerStore store, final LedgerPriceBook prices, final Clock clock) {
        this.store = Objects.requireNonNull(store, "store");
        this.prices = Objects.requireNonNull(prices, "prices");
        this.clock = Objects.requireNonNull(clock, "clock");
    }

    /**
     * Export a bounded retained view, explicitly indicating truncation.
     * @param scope application-authorized scope
     * @param limit maximum exported rows, at most 1000
     * @return records and exact retained match count for this bounded sink
     */
    public Export export(final UsageLedgerStore.Scope scope, final int limit) {
        if (limit < 1 || limit > 1000) {
            throw new IllegalArgumentException("Export limit must be between 1 and 1000");
        }
        List<LedgerRecord> rows = store.query(scope, 32768);
        boolean complete = rows.size() < 32768;
        return new Export(rows.stream().limit(limit).toList(), rows.size(), rows.size() > limit || !complete,
                complete, store.diagnostics().epoch(), summary(rows, complete));
    }

    /**
     * Create a separately auditable revaluation; the original attempt never changes.
     * The caller must authorize this administrative action, not merely a read.
     * @param scope application-authorized administrative scope
     * @param originalId existing attempt ID
     * @param valuationId unique operator revision
     * @param asOf explicit effective-time selection
     * @return the independently stored valuation
     */
    public LedgerRecord revalue(final UsageLedgerStore.Scope scope, final String originalId, final String valuationId, final Instant asOf) {
        LedgerRecord.required(valuationId);
        LedgerRecord original = store.find(scope, originalId);
        if (Objects.isNull(original) || original.kind() != LedgerRecord.Kind.ATTEMPT) {
            throw new SecurityException("Attempt is unavailable in the authorized scope");
        }
        LedgerRecord changed = new LedgerRecord(LedgerRecord.key("revalue", original.id(), valuationId), LedgerRecord.Kind.REVALUATION,
                original.id(), valuationId, original.invocationId(), original.attemptNumber(), original.phase(), original.observedAt(),
                clock.instant(), original.facts(), original.usage(), prices.estimate(original.facts(), original.usage(), asOf),
                original.outcome(), original.httpStatus(), 0, original.schemaVersion(), original.streaming());
        UsageLedgerStore.AppendResult result = store.append(changed);
        if (result != UsageLedgerStore.AppendResult.APPENDED && result != UsageLedgerStore.AppendResult.DUPLICATE) {
            throw new IllegalStateException("Revaluation could not be retained");
        }
        return store.find(scope, changed.id());
    }

    private Summary summary(final List<LedgerRecord> rows, final boolean completeRetainedView) {
        List<LedgerRecord> attempts = rows.stream().filter(row -> row.kind() == LedgerRecord.Kind.ATTEMPT).toList();
        List<LedgerRecord> logical = rows.stream().filter(row -> row.kind() == LedgerRecord.Kind.LOGICAL).toList();
        BigInteger inputs = BigInteger.ZERO;
        BigInteger outputs = BigInteger.ZERO;
        int unknown = 0;
        int estimated = 0;
        int partialOutcomes = 0;
        Map<String, Cost> costs = new LinkedHashMap<>();
        Map<Attribution, Cost> attributed = new LinkedHashMap<>();
        Map<String, Integer> actual = new LinkedHashMap<>();
        for (LedgerRecord row : attempts) {
            actual.merge(row.invocationId(), 1, Integer::sum);
            LedgerRecord.Usage usage = row.usage();
            if (usage.origin() == LedgerRecord.Origin.PROVIDER_REPORTED) {
                if (Objects.nonNull(usage.inputTokens())) {
                    inputs = inputs.add(BigInteger.valueOf(usage.inputTokens()));
                }
                if (Objects.nonNull(usage.outputTokens())) {
                    outputs = outputs.add(BigInteger.valueOf(usage.outputTokens()));
                }
            } else if (usage.origin() == LedgerRecord.Origin.GATEWAY_ESTIMATE) {
                estimated++;
            }
            if (Objects.isNull(usage.inputTokens()) || Objects.isNull(usage.outputTokens()) || usage.origin() != LedgerRecord.Origin.PROVIDER_REPORTED) {
                unknown++;
            }
            if (!"SUCCESS".equals(row.outcome()) || row.streaming()) {
                partialOutcomes++;
            }
            LedgerRecord.Estimate price = row.estimate();
            String currency = Objects.requireNonNullElse(price.currency(), "UNPRICED");
            costs.put(currency, addCost(costs.get(currency), row));
            LedgerRecord.Facts facts = row.facts();
            Attribution dimension = new Attribution(facts.namespace(), facts.tenant(), facts.application(), facts.feature(),
                    facts.provider(), facts.resolvedModel(), facts.clientProtocol(), currency);
            attributed.put(dimension, addCost(attributed.get(dimension), row));
        }
        Set<String> terminalIds = logical.stream().map(LedgerRecord::invocationId).collect(Collectors.toSet());
        long missingTerminals = actual.keySet().stream().filter(id -> !terminalIds.contains(id)).count();
        boolean coverage = completeRetainedView && missingTerminals == 0
                && logical.stream().allMatch(row -> actual.getOrDefault(row.invocationId(), 0) == row.expectedAttempts());
        long noOutbound = logical.stream().filter(row -> row.expectedAttempts() == 0).count();
        boolean interrupted = logical.stream().anyMatch(row -> row.expectedAttempts() > 0 && !"SUCCESS".equals(row.outcome()));
        return new Summary(logical.size(), attempts.size(), noOutbound, inputs, outputs, unknown, estimated,
                !coverage || unknown > 0 || partialOutcomes > 0 || interrupted, Map.copyOf(costs),
                attributed.entrySet().stream().map(entry -> new AttributedCost(entry.getKey(), entry.getValue())).toList());
    }

    private Cost addCost(final Cost previous, final LedgerRecord row) {
        Cost prior = Objects.nonNull(previous) ? previous : new Cost(BigDecimal.ZERO, BigDecimal.ZERO, 0, 0, 0);
        LedgerRecord.Estimate price = row.estimate();
        boolean priced = price.status() == LedgerRecord.PriceStatus.ESTIMATED;
        boolean fallback = "FALLBACK".equals(row.phase());
        return new Cost(priced ? prior.knownEstimatedSubtotal().add(price.amount()) : prior.knownEstimatedSubtotal(),
                priced && fallback ? prior.knownFallbackSubtotal().add(price.amount()) : prior.knownFallbackSubtotal(),
                prior.unknownAttempts() + (priced ? 0 : 1), prior.unknownFallbackAttempts() + (!priced && fallback ? 1 : 0), prior.attempts() + 1);
    }

    /**
     * Retained rows, not a claim of complete historical delivery.
     * @param rows bounded metadata records
     * @param matchedRetainedRecords total scoped retained records
     * @param truncated whether export rows omit other retained rows
     * @param retainedViewComplete whether the retained query reached no source cap; not a delivery guarantee
     * @param storeEpoch opaque lifetime marker for reset/crash visibility
     * @param summary original-attempt aggregation over retained rows
     */
    public record Export(List<LedgerRecord> rows, int matchedRetainedRecords, boolean truncated,
                          boolean retainedViewComplete, String storeEpoch, Summary summary) {
    }

    /**
     * Original-attempt quantities; terminal rollups and revaluations are deliberately excluded.
     * @param logicalRequests retained terminal count
     * @param actualAttempts distinct original attempts
     * @param noOutboundRequests no new provider work, not zero total task cost
     * @param knownProviderInput measured input subtotal; unknown rows are separately counted
     * @param knownProviderOutput measured output subtotal
     * @param unknownUsageAttempts quantities unavailable or not provider-reported
     * @param estimatedUsageAttempts gateway-estimated quantities, never relabeled as actual
     * @param possibleUnobservedConsumption missing coverage, unknown usage or non-success attempts
     * @param costs separate currency groups, without FX or invoice claims
     * @param attributedCosts bounded provider/model/application/feature groups from the same retained authorized view
     */
    public record Summary(int logicalRequests, int actualAttempts, long noOutboundRequests,
                           BigInteger knownProviderInput, BigInteger knownProviderOutput, int unknownUsageAttempts,
                           int estimatedUsageAttempts, boolean possibleUnobservedConsumption, Map<String, Cost> costs, List<AttributedCost> attributedCosts) {
    }

    /**
     * Known monetary subtotals with explicit unknown coverage; no unknown amount is converted to zero.
     * @param knownEstimatedSubtotal known estimate across all actual attempts, including failed primaries
     * @param knownFallbackSubtotal declared fallback subset
     * @param unknownAttempts unpriced or unmeasured attempts
     * @param unknownFallbackAttempts fallback subset with unavailable estimate
     * @param attempts original attempt count in this group
     */
    public record Cost(BigDecimal knownEstimatedSubtotal, BigDecimal knownFallbackSubtotal, int unknownAttempts, int unknownFallbackAttempts, int attempts) {
    }

    /**
     * Bounded query dimensions, never metric labels. Null attribution remains unknown.
     * @param namespace deployment or trusted request namespace
     * @param tenant authenticated tenant
     * @param application trusted application, nullable
     * @param feature trusted feature, nullable
     * @param provider actual provider
     * @param model resolved model
     * @param clientProtocol observed protocol
     * @param currency separate currency or UNPRICED
     */
    public record Attribution(String namespace, String tenant, String application, String feature,
                               String provider, String model, String clientProtocol, String currency) {
    }

    /**
     * Authorized retained cost breakdown.
     * @param attribution known dimensions
     * @param cost original-attempt subtotal and unknown coverage
     */
    public record AttributedCost(Attribution attribution, Cost cost) {
    }
}
