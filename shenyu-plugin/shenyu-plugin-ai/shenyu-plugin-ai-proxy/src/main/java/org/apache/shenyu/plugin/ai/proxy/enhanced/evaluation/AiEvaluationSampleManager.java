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

package org.apache.shenyu.plugin.ai.proxy.enhanced.evaluation;

import org.apache.shenyu.common.cache.WindowTinyLFUMap;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.AgentRequestContext;
import org.apache.shenyu.common.dto.convert.rule.AiEvaluationRouteConfig;
import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;
import org.apache.shenyu.common.metrics.AiCallObserver;
import org.apache.shenyu.common.metrics.AiCallOutcome;
import org.apache.shenyu.common.metrics.AiTerminalEvent;
import org.apache.shenyu.common.metrics.AiUsage;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.plugin.ai.common.evaluation.AiEvaluationContract;
import org.apache.shenyu.plugin.ai.common.evaluation.AiEvaluationExportRecord;
import org.apache.shenyu.plugin.ai.common.evaluation.AiEvaluationFeedback;
import org.apache.shenyu.plugin.ai.common.evaluation.AiEvaluationSample;
import org.apache.shenyu.plugin.ai.proxy.enhanced.handler.AiProxyPluginHandler;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The opt-in evaluation sample manager (issue #8256): observes terminal AI events
 * and, only for routes that explicitly opted in, mints an opaque sample reference,
 * exports a body-free metadata snapshot asynchronously, and authorizes feedback
 * joins against that reference.
 *
 * <p>Every entry point is O(1) and non-blocking — the request path only ever pays
 * a cached map lookup, a sampler check and a bounded-queue offer. Retained state
 * is bounded twice over: the exporter queue capacity and the join registry, whose
 * entries expire with the configured retention, so a sample reference that was
 * never joined simply disappears. The recorded tenant always stays inside the
 * registry for join authorization; it is exported only when the route allow-listed
 * identity fields. A request id is never an authority — only references the
 * gateway minted can be joined, and forged or expired ones are rejected without
 * creating state.</p>
 */
public final class AiEvaluationSampleManager implements AiCallObserver {

    private static final int MAX_LABEL_VALUE_CHARS = AiEvaluationContract.MAX_LABEL_VALUE_CHARS;

    private final AiProxyPluginHandler handler;

    private final AiEvaluationProperties properties;

    private final AiEvaluationExporter exporter;

    private final WindowTinyLFUMap<String, PendingSample> registry;

    private final Map<String, AiEvaluationSampler> samplers = new ConcurrentHashMap<>();

    public AiEvaluationSampleManager(final AiProxyPluginHandler handler, final AiEvaluationProperties properties,
                                     final AiEvaluationExporter exporter) {
        this.handler = handler;
        this.properties = properties;
        this.exporter = exporter;
        this.registry = new WindowTinyLFUMap<>(64, properties.getJoinRetentionMillis(),
                properties.getRegistryCapacity(), false);
    }

    @Override
    public void record(final AiTerminalEvent event) {
        try {
            AgentRequestContext context = event.context();
            AiEvaluationRouteConfig route = routeConfig(context);
            if (Objects.isNull(route) || !Boolean.TRUE.equals(route.getEnabled())) {
                return;
            }
            if (!samplerFor(context.getSelectorId(), route).isSampled()) {
                return;
            }
            boolean identityIncluded = Boolean.TRUE.equals(route.getIncludeIdentity());
            String sampleId = UUID.randomUUID().toString();
            AiEvaluationSample sample = new AiEvaluationSample(sampleId, AiEvaluationContract.SAMPLE_VERSION,
                    System.currentTimeMillis(), context.getRequestId(), context.getSelectorId(), context.getRuleId(),
                    context.getConfigurationVersion(), outcomeName(event.outcome()), event.httpStatus(),
                    event.elapsedMillis(), event.attemptCount(), usageComponent(event.usage(), AiUsage::promptTokens),
                    usageComponent(event.usage(), AiUsage::completionTokens), usageComponent(event.usage(), AiUsage::totalTokens),
                    usageStatus(event.usage()), identityIncluded,
                    identityIncluded ? context.getTenant() : null,
                    identityIncluded ? context.getSubject() : null,
                    identityIncluded ? context.getTarget() : null);
            // the recorded tenant authorizes joins but is exported only when allow-listed
            registry.put(sampleId, new PendingSample(context.getTenant()));
            exporter.offer(new AiEvaluationExportRecord(AiEvaluationExportRecord.Type.SAMPLE,
                    AiEvaluationContract.SAMPLE_VERSION, JsonUtils.toJson(sample)));
        } catch (RuntimeException ignored) {
            // evaluation must never decide the fate of the request
        }
    }

    /**
     * Offer one feedback join against a previously exported sample.
     *
     * @param feedback the join offer
     * @return the join outcome; accepted or updated joins are exported asynchronously
     */
    public AiFeedbackJoinResult joinFeedback(final AiEvaluationFeedback feedback) {
        if (Objects.isNull(feedback) || blank(feedback.sampleId()) || blank(feedback.judgeId())
                || Objects.isNull(feedback.labels()) || feedback.labels().size() > AiEvaluationContract.MAX_LABELS
                || !labelsBounded(feedback.labels())) {
            return new AiFeedbackJoinResult(AiFeedbackJoinResult.Status.REJECTED_INVALID,
                    Objects.isNull(feedback) ? null : feedback.sampleId(),
                    Objects.isNull(feedback) ? null : feedback.judgeId());
        }
        PendingSample pending = registry.get(feedback.sampleId());
        if (Objects.isNull(pending)) {
            return new AiFeedbackJoinResult(AiFeedbackJoinResult.Status.UNKNOWN_SAMPLE,
                    feedback.sampleId(), feedback.judgeId());
        }
        AiFeedbackJoinResult.Status status;
        synchronized (pending) {
            if (!Objects.equals(pending.tenant, feedback.tenant())) {
                return new AiFeedbackJoinResult(AiFeedbackJoinResult.Status.TENANT_MISMATCH,
                        feedback.sampleId(), feedback.judgeId());
            }
            PendingJoin existing = pending.joins.stream()
                    .filter(join -> Objects.equals(join.judgeId, feedback.judgeId()))
                    .findFirst().orElse(null);
            if (Objects.nonNull(existing)) {
                existing.labels = Map.copyOf(feedback.labels());
                status = AiFeedbackJoinResult.Status.UPDATED;
            } else if (pending.joins.size() >= properties.getMaxJoinsPerSample()) {
                return new AiFeedbackJoinResult(AiFeedbackJoinResult.Status.REJECTED_BOUND,
                        feedback.sampleId(), feedback.judgeId());
            } else {
                pending.joins.add(new PendingJoin(feedback.judgeId(), Map.copyOf(feedback.labels())));
                status = AiFeedbackJoinResult.Status.ACCEPTED;
            }
        }
        AiEvaluationFeedback exported = new AiEvaluationFeedback(feedback.sampleId(), feedback.tenant(),
                feedback.judgeId(), feedback.evalSetVersion(), feedback.judgeVersion(), feedback.labels(),
                feedback.submittedAtMillis());
        exporter.offer(new AiEvaluationExportRecord(AiEvaluationExportRecord.Type.FEEDBACK,
                AiEvaluationContract.FEEDBACK_VERSION, JsonUtils.toJson(exported)));
        return new AiFeedbackJoinResult(status, feedback.sampleId(), feedback.judgeId());
    }

    private AiEvaluationRouteConfig routeConfig(final AgentRequestContext context) {
        AiProxyHandle handle = handler.getSelectorCachedHandle()
                .obtainHandle(CacheKeyUtils.INST.getKey(context.getSelectorId(), Constants.DEFAULT_RULE));
        return Objects.isNull(handle) ? null : handle.getEvaluation();
    }

    private AiEvaluationSampler samplerFor(final String selectorId, final AiEvaluationRouteConfig route) {
        return samplers.computeIfAbsent(selectorId, key -> new AiEvaluationSampler(route.getSampleRate()));
    }

    private static String outcomeName(final AiCallOutcome outcome) {
        return Objects.isNull(outcome) ? null : outcome.name();
    }

    private static Long usageComponent(final AiUsage usage, final java.util.function.Function<AiUsage, Long> component) {
        return Objects.isNull(usage) ? null : component.apply(usage);
    }

    private static String usageStatus(final AiUsage usage) {
        return Objects.isNull(usage) || Objects.isNull(usage.status()) ? null : usage.status().name();
    }

    private static boolean labelsBounded(final Map<String, String> labels) {
        return labels.entrySet().stream().allMatch(entry ->
                !blank(entry.getKey()) && !blank(entry.getValue())
                        && entry.getValue().length() <= MAX_LABEL_VALUE_CHARS);
    }

    private static boolean blank(final String value) {
        return Objects.isNull(value) || value.isBlank();
    }

    private static final class PendingSample {

        private final String tenant;

        private final List<PendingJoin> joins = new ArrayList<>();

        private PendingSample(final String tenant) {
            this.tenant = tenant;
        }
    }

    private static final class PendingJoin {

        private final String judgeId;

        private Map<String, String> labels;

        private PendingJoin(final String judgeId, final Map<String, String> labels) {
            this.judgeId = judgeId;
            this.labels = labels;
        }
    }
}
