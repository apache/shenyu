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

package org.apache.shenyu.common.metrics;

import java.util.Objects;

/**
 * Immutable sampled plugin facts; no exchange, DTO, body, raw URL, exception or chain control is retained.
 * @param requestId local root-subscription identity
 * @param invocationId actual plugin construction invocation
 * @param subscriptionId publisher subscription identity, or zero for construction/matching
 * @param plugin bounded implementation label, never a request identifier
 * @param implementation bounded implementation provenance
 * @param traceId optional existing trace correlation, never a metric label
 * @param kind observation boundary
 * @param outcome terminal classification
 * @param elapsedNanos monotonic inclusive publisher or separate construction duration
 * @param matching optional actual matching summary
 */
public record PluginObservationEvent(long requestId, long invocationId, long subscriptionId, String plugin, String implementation,
                                     String traceId, Kind kind, Outcome outcome, long elapsedNanos, Matching matching) {

    public PluginObservationEvent {
        plugin = bounded(plugin, 64);
        implementation = bounded(implementation, 256);
        traceId = bounded(traceId, 128);
        Objects.requireNonNull(kind);
        Objects.requireNonNull(outcome);
        elapsedNanos = Math.max(0, elapsedNanos);
    }

    /**
     * Bound diagnostic text without retaining a larger supplied string.
     * @param value diagnostic identifier
     * @param maximum maximum characters
     * @return bounded identifier, never a payload
     */
    public static String bounded(final String value, final int maximum) {
        return Objects.isNull(value) ? "" : value.substring(0, Math.min(value.length(), maximum));
    }

    /** Construction, publisher and matching are distinct observation boundaries. */
    public enum Kind {
        SKIPPED, SKIP_ERROR, CONSTRUCTED, CONSTRUCTION_ERROR, SUBSCRIBED, TERMINAL, MATCHING, TRUNCATED
    }

    /** Publisher termination is not final HTTP status, upstream-call completion or authorization. */
    public enum Outcome {
        NONE, COMPLETE, ERROR, CANCELLED
    }

    /** Independent cache resolutions; a cached winner does not imply predicate evaluation. */
    public enum Cache {
        NOT_CONSULTED, DISABLED, MISS, POSITIVE_HIT, NEGATIVE_HIT
    }

    /** Actual base-plugin branches, not hypothetical request explanations. */
    public enum Decision {
        PLUGIN_MISSING, PLUGIN_DISABLED, NO_SELECTORS, SELECTOR_MISS, SELECTOR_NEGATIVE_HIT,
        SYNTHETIC_RULE, NO_RULES, NO_ENABLED_FULL_FLOW_RULE, FULL_FLOW_RULE, RULE_MISS, RULE_NEGATIVE_HIT, SELECTED
    }

    /**
     * Live-cache observation, not an atomic configuration revision. IDs are diagnostics, never metric labels.
     * @param plugin configured plugin identity
     * @param selector selected selector, when known
     * @param rule selected rule, when known
     * @param decision actual branch
     * @param selectorCache actual selector cache resolution
     * @param ruleCache actual rule cache resolution
     */
    public record Matching(String plugin, String selector, String rule, Decision decision, Cache selectorCache, Cache ruleCache) {
        public Matching {
            plugin = bounded(plugin, 64);
            selector = bounded(selector, 128);
            rule = bounded(rule, 128);
            Objects.requireNonNull(decision);
            Objects.requireNonNull(selectorCache);
            Objects.requireNonNull(ruleCache);
        }
    }
}
