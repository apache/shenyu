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

package org.apache.shenyu.plugin.ai.common.view;

import com.fasterxml.jackson.databind.JsonNode;
import org.apache.shenyu.common.ai.AiNormalizedRequestView;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Per-exchange state behind {@link AiRequestViewOperations}: the current view, the
 * original wire view and the bounded consumer inspection log.
 *
 * <p>Memory is bounded by construction — at most two views are retained (original
 * plus current, each already bounded by the contract), and at most
 * {@link AiNormalizedRequestView#MAX_INSPECTIONS} inspection entries. The holder is
 * stored as a server exchange attribute, so it is released with the exchange: there
 * is no global registry and no explicit cleanup path.</p>
 */
final class AiRequestViewHolder {

    private final Object lock = new Object();

    private AiRequestViewHolder.Versioned original;

    private AiRequestViewHolder.Versioned current;

    private long nextGeneration;

    private final Map<String, Long> inspections = new LinkedHashMap<>();

    AiRequestViewHolder() {
        this.nextGeneration = 1L;
    }

    /**
     * Publish the given body version with an already parsed payload and an
     * explicitly resolved protocol identity.
     *
     * @param body the body version the caller observed
     * @param parsedRoot the already parsed body, {@code null} lets the parser decode it
     * @param mutatorName component that produced the body, used for effective views
     * @param protocolId the resolved protocol identity, {@code null} or blank when the
     *                   request stayed unresolved; no hint is derived from the body
     * @return the view of the current body version
     */
    AiNormalizedRequestView ensurePublished(final String body, final JsonNode parsedRoot,
                                            final String mutatorName, final String protocolId) {
        long fingerprint = fingerprint(body);
        synchronized (lock) {
            // a different protocol identity is a different interpretation of the same body
            if (Objects.nonNull(current) && current.fingerprint == fingerprint
                    && Objects.equals(current.protocolId, protocolId)) {
                return current.view;
            }
            AiNormalizedRequestView.ViewKind kind = Objects.isNull(original)
                    ? AiNormalizedRequestView.ViewKind.WIRE : AiNormalizedRequestView.ViewKind.EFFECTIVE;
            AiNormalizedRequestView view = AiRequestViewParser.parse(body, parsedRoot, kind,
                    kind == AiNormalizedRequestView.ViewKind.EFFECTIVE ? mutatorName : null, nextGeneration, protocolId);
            Versioned versioned = new Versioned(view, fingerprint, protocolId);
            current = versioned;
            if (Objects.isNull(original)) {
                original = versioned;
            }
            nextGeneration++;
            return view;
        }
    }

    /**
     * The latest published view, or {@code null} when nothing was published.
     *
     * @return the current view or {@code null}
     */
    AiNormalizedRequestView snapshot() {
        synchronized (lock) {
            return Objects.isNull(current) ? null : current.view;
        }
    }

    /**
     * The wire view of the original body version, or {@code null} when nothing was
     * published.
     *
     * @return the original wire view or {@code null}
     */
    AiNormalizedRequestView wire() {
        synchronized (lock) {
            return Objects.isNull(original) ? null : original.view;
        }
    }

    /**
     * Record that a consumer inspected the current generation. A consumer re-inspecting
     * replaces its own entry; beyond the bounded size new distinct consumers are not
     * recorded.
     *
     * @param consumerName the consumer identity
     * @return the current view or {@code null} when nothing was published
     */
    AiNormalizedRequestView recordInspection(final String consumerName) {
        synchronized (lock) {
            if (Objects.isNull(current)) {
                return null;
            }
            if (!inspections.containsKey(consumerName) && inspections.size() >= AiNormalizedRequestView.MAX_INSPECTIONS) {
                return current.view;
            }
            inspections.put(consumerName, current.view.generation());
            return current.view;
        }
    }

    /**
     * The bounded inspection report: one entry per recorded consumer with whether the
     * inspected generation is still current. An outdated entry means the consumer's
     * interpretation was superseded by a later mutation and cannot be claimed as a
     * successful inspection of the effective request.
     *
     * @return the inspection report, oldest entry first
     */
    List<AiRequestViewOperations.Inspection> inspectionReport() {
        synchronized (lock) {
            List<AiRequestViewOperations.Inspection> report = new ArrayList<>(inspections.size());
            long currentGeneration = Objects.isNull(current) ? 0L : current.view.generation();
            inspections.forEach((consumer, generation) -> report.add(
                    new AiRequestViewOperations.Inspection(consumer, generation, generation == currentGeneration)));
            return report;
        }
    }

    /**
     * FNV-1a over the body characters; collisions only risk a missed regeneration,
     * the body is never retained to compare exactly.
     */
    private static long fingerprint(final String body) {
        long hash = 0xcbf29ce484222325L;
        if (Objects.isNull(body)) {
            return hash;
        }
        for (int i = 0; i < body.length(); i++) {
            hash ^= body.charAt(i);
            hash *= 0x100000001b3L;
        }
        return hash;
    }

    private record Versioned(AiNormalizedRequestView view, long fingerprint, String protocolId) {
    }
}
