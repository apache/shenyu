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
import org.apache.shenyu.plugin.ai.protocol.AiProtocolDetector;
import org.apache.shenyu.plugin.ai.protocol.AiProtocols;
import org.springframework.web.server.ServerWebExchange;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * Access to the bounded normalized request view of one exchange (issue #8255).
 *
 * <p>The view is published once per body version by the first component that holds
 * that version — the prompt plugin for the wire and its decorated body, the AI proxy
 * for the body it is about to decode — and every later reader consumes the published
 * view instead of re-parsing the body. A mutation publishes a new generation, so a
 * consumer that inspected an older generation is observable as superseded instead of
 * silently interpreting a request that no longer exists.</p>
 *
 * <p>State lives exclusively in a server exchange attribute, so it is bounded to the
 * request lifetime and released with the exchange; there is no global map and no
 * explicit cleanup. Request content never leaves the view contract: it must not be
 * placed in {@code AgentRequestContext}, trace or audit events.</p>
 */
public final class AiRequestViewOperations {

    /**
     * Exchange attribute holding the per-request view state.
     */
    public static final String EXCHANGE_ATTRIBUTE = "org.apache.shenyu.plugin.ai.common.view.normalizedRequestView";

    private AiRequestViewOperations() {
    }

    /**
     * Publish the view of the given body version unless it is already the published
     * one. The first publication of an exchange is the wire view; later publications
     * with a different body are effective views naming the component that produced
     * them. The body itself is never retained.
     *
     * @param exchange the current exchange
     * @param body the body version the caller observed
     * @param mutatorName component that produced the body, used for effective views
     * @return the view of the current body version
     */
    public static AiNormalizedRequestView ensurePublished(final ServerWebExchange exchange, final String body,
                                                          final String mutatorName) {
        String protocol = AiProtocolDetector.detect(exchange.getRequest().getPath().value(),
                exchange.getRequest().getHeaders(), body).orElse(AiProtocols.UNKNOWN);
        return holder(exchange).ensurePublished(body, null, mutatorName, protocol);
    }

    /**
     * Publish the view of a body version whose protocol identity is already resolved.
     *
     * @param exchange the current exchange
     * @param body the body version the caller observed
     * @param mutatorName component that produced the body, used for effective views
     * @param protocolId the resolved protocol identity, {@link AiProtocols#UNKNOWN}
     *                   when the request stayed unresolved
     * @return the view of the current body version
     */
    public static AiNormalizedRequestView ensurePublished(final ServerWebExchange exchange, final String body,
                                                          final String mutatorName, final String protocolId) {
        return holder(exchange).ensurePublished(body, null, mutatorName, protocolId);
    }

    /**
     * Publish the view of an already parsed body version with its resolved protocol
     * identity, so no consumer parses the same body twice.
     *
     * @param exchange the current exchange
     * @param body the body version the caller observed
     * @param parsedRoot the already parsed body
     * @param mutatorName component that produced the body, used for effective views
     * @param protocolId the resolved protocol identity, {@link AiProtocols#UNKNOWN}
     *                   when the request stayed unresolved
     * @return the view of the current body version
     */
    public static AiNormalizedRequestView ensurePublished(final ServerWebExchange exchange, final String body,
                                                          final JsonNode parsedRoot, final String mutatorName,
                                                          final String protocolId) {
        return holder(exchange).ensurePublished(body, parsedRoot, mutatorName, protocolId);
    }

    /**
     * The latest published view without recording an inspection.
     *
     * @param exchange the current exchange
     * @return the current view, empty when nothing was published
     */
    public static Optional<AiNormalizedRequestView> snapshot(final ServerWebExchange exchange) {
        return Optional.ofNullable(holder(exchange).snapshot());
    }

    /**
     * The wire view of the original body version, even after later mutations
     * published effective generations. This is how a consumer states that it needs
     * the original wire representation instead of the effective one.
     *
     * @param exchange the current exchange
     * @return the wire view, empty when nothing was published
     */
    public static Optional<AiNormalizedRequestView> wireSnapshot(final ServerWebExchange exchange) {
        return Optional.ofNullable(holder(exchange).wire());
    }

    /**
     * Read the latest published view and record the inspection under the consumer
     * name. A consumer that never calls this cannot claim a successful inspection;
     * when no view exists (for example no publisher ran before this consumer) the
     * call records nothing and returns empty, which keeps unsupported inspection
     * visible instead of simulated.
     *
     * @param exchange the current exchange
     * @param consumerName the consumer identity, e.g. the plugin name
     * @return the inspected view, empty when nothing was published
     */
    public static Optional<AiNormalizedRequestView> snapshotAndRecordInspection(final ServerWebExchange exchange,
                                                                                final String consumerName) {
        return Optional.ofNullable(holder(exchange).recordInspection(Objects.requireNonNull(consumerName, "consumerName")));
    }

    /**
     * The bounded inspection report of this exchange: which consumer inspected which
     * generation and whether that generation is still current.
     *
     * @param exchange the current exchange
     * @return the report, oldest entry first
     */
    public static List<Inspection> inspectionReport(final ServerWebExchange exchange) {
        return holder(exchange).inspectionReport();
    }

    /**
     * The latest published generation, or {@code 0} when nothing was published.
     *
     * @param exchange the current exchange
     * @return the current generation
     */
    public static long currentGeneration(final ServerWebExchange exchange) {
        return snapshot(exchange).map(AiNormalizedRequestView::generation).orElse(0L);
    }

    private static AiRequestViewHolder holder(final ServerWebExchange exchange) {
        return (AiRequestViewHolder) exchange.getAttributes().computeIfAbsent(EXCHANGE_ATTRIBUTE,
                key -> new AiRequestViewHolder());
    }

    /**
     * One recorded consumer inspection.
     *
     * @param consumer the consumer identity
     * @param generation the generation the consumer inspected
     * @param current whether that generation is still the published one; a superseded
     *                inspection cannot be claimed as a successful inspection of the
     *                effective request
     */
    public record Inspection(String consumer, long generation, boolean current) {
    }
}
