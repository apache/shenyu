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

package org.apache.shenyu.admin.scale.scaler;

import org.apache.shenyu.admin.model.entity.ScalePolicyDO;
import org.apache.shenyu.admin.scale.config.ScaleProperties;
import org.apache.shenyu.admin.scale.monitor.subject.MetricsMonitor;
import org.apache.shenyu.admin.scale.scaler.cache.ScalePolicyCache;
import org.apache.shenyu.admin.scale.scaler.dynamic.TaskSchedulerManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.Clock;
import java.time.Instant;
import java.util.Comparator;
import java.util.Date;
import java.util.Objects;


/**
 * Reconciles Kubernetes scaling state with the active scaling policy.
 *
 * <p>Policy {@code 1} sets a fixed replica count, policy {@code 2} applies during a time window,
 * and policy {@code 3} is metric-driven. Reconciliation runs at startup, after policy updates,
 * and from one-shot window-boundary tasks.</p>
 */
@Component
public class ScaleService {

    private static final String DYNAMIC_TASK_NAME = "DynamicScalingTask";

    private static final String SCALE_WINDOW_ACTIVATION_TASK = "ScaleWindowActivation";

    private static final String SCALE_WINDOW_DEACTIVATION_TASK = "ScaleWindowDeactivation";

    private static final Logger LOG = LoggerFactory.getLogger(ScaleService.class);

    private final TaskSchedulerManager schedulerManager;

    private final MetricsMonitor metricsMonitor;

    private final ScalePolicyCache scalePolicyCache;

    private final ScaleProperties scaleProperties;

    private final KubernetesScaler kubernetesScaler;

    private final Clock clock;

    private boolean active;

    private AppliedTarget appliedTarget;

    @Autowired
    public ScaleService(final MetricsMonitor metricsMonitor,
                        final TaskSchedulerManager schedulerManager,
                        final ScalePolicyCache scalePolicyCache,
                        final ScaleProperties scaleProperties,
                        final KubernetesScaler kubernetesScaler) {
        this(metricsMonitor, schedulerManager, scalePolicyCache, scaleProperties,
                kubernetesScaler, Clock.systemDefaultZone());
    }

    ScaleService(final MetricsMonitor metricsMonitor,
                 final TaskSchedulerManager schedulerManager,
                 final ScalePolicyCache scalePolicyCache,
                 final ScaleProperties scaleProperties,
                 final KubernetesScaler kubernetesScaler,
                 final Clock clock) {
        this.schedulerManager = schedulerManager;
        this.metricsMonitor = metricsMonitor;
        this.scalePolicyCache = scalePolicyCache;
        this.scaleProperties = scaleProperties;
        this.kubernetesScaler = kubernetesScaler;
        this.clock = clock;
    }

    /**
     * Activate scaling and reconcile the currently effective policy.
     */
    public synchronized void start() {
        active = true;
        executeScaling();
    }

    /**
     * Deactivate scaling and cancel all scaling tasks.
     */
    public synchronized void stop() {
        active = false;
        stopDynamicTask();
        cancelWindowTasks();
        appliedTarget = null;
    }

    /**
     * Reconcile the currently effective scaling policy.
     *
     * <p>This method is synchronized because startup, HTTP request, and scheduler threads may invoke it.</p>
     */
    public synchronized void executeScaling() {
        if (!active) {
            return;
        }
        cancelWindowTasks();
        Instant now = clock.instant();
        scheduleFutureWindowActivation(now);
        ScalePolicyDO activePolicy = getActivePolicy(now);
        if (Objects.nonNull(activePolicy)) {
            switch (activePolicy.getId()) {
                case "1":
                    stopDynamicTask();
                    applyTarget(activePolicy);
                    break;
                case "2":
                    stopDynamicTask();
                    Instant end = toInstant(activePolicy.getEndTime());
                    applyTarget(activePolicy);
                    if (Objects.nonNull(end)) {
                        schedulerManager.registerOnceTask(SCALE_WINDOW_DEACTIVATION_TASK, this::executeScaling, end);
                    }
                    break;
                case "3":
                    appliedTarget = null;
                    startDynamicTask(scaleProperties.getMonitorInterval());
                    break;
                default:
                    stopDynamicTask();
                    throw new IllegalStateException("Unknown scaling policy: " + activePolicy.getId());
            }
        } else {
            appliedTarget = null;
            stopDynamicTask();
        }
    }

    /**
     * Select the enabled policy that is eligible at the supplied instant and has the smallest sort value.
     * Time-window policies participate only while their window is open.
     *
     * @param now reconciliation instant
     * @return the active policy, or {@code null} when none is eligible
     */
    private ScalePolicyDO getActivePolicy(final Instant now) {
        return scalePolicyCache.getAllPolicies().stream()
                .filter(policy -> Integer.valueOf(1).equals(policy.getStatus()))
                .filter(policy -> isEligible(policy, now))
                .min(Comparator.comparingInt(ScalePolicyDO::getSort))
                .orElseGet(() -> {
                    LOG.warn("No active scaling policy found.");
                    return null;
                });
    }

    /**
     * Determine whether a policy can participate at the supplied instant.
     * Non-window policies are always eligible; a null window boundary is unbounded on that side.
     *
     * @param policy scaling policy
     * @param now reconciliation instant
     * @return whether the policy is eligible
     */
    private boolean isEligible(final ScalePolicyDO policy, final Instant now) {
        if (!"2".equals(policy.getId())) {
            return true;
        }
        Instant begin = toInstant(policy.getBeginTime());
        Instant end = toInstant(policy.getEndTime());
        return (Objects.isNull(begin) || !now.isBefore(begin))
                && (Objects.isNull(end) || now.isBefore(end));
    }

    /**
     * Register a future window boundary even when another policy currently wins, so priority is
     * re-evaluated when that window opens.
     *
     * @param now reconciliation instant
     */
    private void scheduleFutureWindowActivation(final Instant now) {
        scalePolicyCache.getAllPolicies().stream()
                .filter(policy -> "2".equals(policy.getId()))
                .filter(policy -> Integer.valueOf(1).equals(policy.getStatus()))
                .filter(policy -> Objects.nonNull(policy.getBeginTime()))
                .filter(policy -> now.isBefore(policy.getBeginTime().toInstant()))
                .filter(policy -> Objects.isNull(policy.getEndTime())
                        || policy.getBeginTime().toInstant().isBefore(policy.getEndTime().toInstant()))
                .min(Comparator.comparingInt(ScalePolicyDO::getSort))
                .ifPresent(policy -> schedulerManager.registerOnceTask(
                        SCALE_WINDOW_ACTIVATION_TASK, this::executeScaling, policy.getBeginTime().toInstant()));
    }

    /**
     * Apply a target idempotently and cache it only after Kubernetes reports success.
     * The Kubernetes call is skipped when the same policy and replica count were already applied.
     *
     * @param policy active scaling policy
     */
    private void applyTarget(final ScalePolicyDO policy) {
        AppliedTarget target = new AppliedTarget(policy.getId(), policy.getNum());
        if (!target.equals(appliedTarget) && kubernetesScaler.scaleByNum(policy.getNum())) {
            appliedTarget = target;
        }
    }

    /**
     * Cancel both window-boundary tasks before recomputing their schedule.
     */
    private void cancelWindowTasks() {
        schedulerManager.cancelMonitorTask(SCALE_WINDOW_ACTIVATION_TASK);
        schedulerManager.cancelMonitorTask(SCALE_WINDOW_DEACTIVATION_TASK);
    }

    /**
     * Convert a nullable date to an instant.
     *
     * @param date date to convert
     * @return the instant, or {@code null} when the date is null
     */
    private Instant toInstant(final Date date) {
        return Objects.isNull(date) ? null : date.toInstant();
    }

    /**
     * execute dynamic scaling.
     */
    private void executeDynamicScaling() {
        try {
            metricsMonitor.monitorMetrics();
        } catch (Exception e) {
            LOG.error("Failed to execute dynamic scale. cause: {} ", e.getMessage());
        }
    }

    /**
     * startDynamicTask.
     *
     * @param interval interval
     */
    public void startDynamicTask(final long interval) {
        schedulerManager.registerMonitorTask(DYNAMIC_TASK_NAME, this::executeDynamicScaling, interval);
    }

    /**
     * stopDynamicTask.
     */
    public void stopDynamicTask() {
        schedulerManager.cancelMonitorTask(DYNAMIC_TASK_NAME);
    }

    private record AppliedTarget(String policyId, int num) {
    }
}
