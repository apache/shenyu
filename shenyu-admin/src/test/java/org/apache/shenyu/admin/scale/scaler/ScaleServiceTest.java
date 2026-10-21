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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.InOrder;

import java.time.Clock;
import java.time.Instant;
import java.util.Date;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

/**
 * Tests for scaling policy transitions and monitoring task cancellation.
 */
public final class ScaleServiceTest {

    private TaskSchedulerManager schedulerManager;

    private KubernetesScaler kubernetesScaler;

    private ScalePolicyCache policyCache;

    private Clock clock;

    private ScaleService service;

    private Instant now;

    @BeforeEach
    public void setUp() {
        schedulerManager = mock(TaskSchedulerManager.class);
        kubernetesScaler = mock(KubernetesScaler.class);
        policyCache = new ScalePolicyCache();
        clock = mock(Clock.class);
        now = Instant.parse("2026-01-15T12:00:00Z");
        when(clock.instant()).thenAnswer(invocation -> now);
        when(kubernetesScaler.scaleByNum(anyInt())).thenReturn(true);

        final ScaleProperties properties = new ScaleProperties();
        properties.setMonitorInterval(1000L);

        service = new ScaleService(
                mock(MetricsMonitor.class), schedulerManager,
                policyCache, properties, kubernetesScaler, clock);
    }

    @Test
    public void testDisableLastPolicyStopsDynamicMonitoring() {
        policyCache.initialize(List.of(policy("3", 1, 1)));

        service.executeScaling();

        policyCache.updatePolicy(policy("3", 0, 1));
        service.executeScaling();
        service.executeScaling();

        verify(schedulerManager).registerMonitorTask(
                eq("DynamicScalingTask"), any(Runnable.class), eq(1000L));
        verify(schedulerManager, times(2)).cancelMonitorTask("DynamicScalingTask");
        verifyWindowTaskCancellations(3);
        verifyNoMoreInteractions(schedulerManager);

        verifyNoInteractions(kubernetesScaler);
    }

    @Test
    public void testEmptyPoliciesCancelDynamicMonitoring() {
        service.executeScaling();

        verify(schedulerManager).cancelMonitorTask("DynamicScalingTask");
        verifyWindowTaskCancellations(1);
        verifyNoMoreInteractions(schedulerManager);
        verifyNoInteractions(kubernetesScaler);
    }

    @Test
    public void testActiveDynamicPolicyRegistersMonitoring() {
        policyCache.initialize(List.of(
                policy("1", 0, 0),
                policy("3", 1, 1)));

        service.executeScaling();

        verify(schedulerManager).registerMonitorTask(
                eq("DynamicScalingTask"), any(Runnable.class), eq(1000L));
        verifyWindowTaskCancellations(1);
        verifyNoMoreInteractions(schedulerManager);
        verifyNoInteractions(kubernetesScaler);
    }

    @Test
    public void testSwitchToFixedPolicyCancelsDynamicMonitoring() {
        policyCache.initialize(List.of(policy("3", 1, 1)));
        service.executeScaling();

        final ScalePolicyDO fixedPolicy = policy("1", 1, 0);
        fixedPolicy.setNum(2);
        policyCache.updatePolicy(fixedPolicy);
        service.executeScaling();

        final InOrder order = inOrder(schedulerManager, kubernetesScaler);
        order.verify(schedulerManager).registerMonitorTask(
                eq("DynamicScalingTask"), any(Runnable.class), eq(1000L));
        order.verify(schedulerManager).cancelMonitorTask("DynamicScalingTask");
        order.verify(kubernetesScaler).scaleByNum(2);
        verifyWindowTaskCancellations(2);
    }

    @Test
    public void testFutureWindowSchedulesActivation() {
        final Instant begin = now.plusSeconds(60);
        policyCache.initialize(List.of(windowPolicy(3, begin, begin.plusSeconds(60))));

        service.executeScaling();

        verify(kubernetesScaler, never()).scaleByNum(anyInt());
        verify(schedulerManager).registerOnceTask(
                eq("ScaleWindowActivation"), any(Runnable.class), eq(begin));
    }

    @Test
    public void testActiveWindowAppliesTargetAndSchedulesDeactivation() {
        final Instant end = now.plusSeconds(60);
        policyCache.initialize(List.of(windowPolicy(3, now.minusSeconds(60), end)));

        service.executeScaling();

        verify(kubernetesScaler).scaleByNum(3);
        verify(schedulerManager).registerOnceTask(
                eq("ScaleWindowDeactivation"), any(Runnable.class), eq(end));
    }

    @Test
    public void testRepeatedReconciliationDoesNotReapplyTarget() {
        final ScalePolicyDO fixedPolicy = policy("1", 1, 1);
        fixedPolicy.setNum(3);
        policyCache.initialize(List.of(fixedPolicy));

        service.executeScaling();
        service.executeScaling();

        verify(kubernetesScaler).scaleByNum(3);
    }

    @Test
    public void testNewWindowReappliesTargetAfterPeriodWithNoActivePolicy() {
        final Instant firstEnd = now.plusSeconds(60);
        policyCache.initialize(List.of(windowPolicy(5, now.minusSeconds(60), firstEnd)));

        service.executeScaling();
        verify(kubernetesScaler).scaleByNum(5);

        now = firstEnd.plusSeconds(1);
        service.executeScaling();
        verify(kubernetesScaler).scaleByNum(5);

        policyCache.updatePolicy(windowPolicy(5, now.minusSeconds(1), now.plusSeconds(60)));
        service.executeScaling();

        verify(kubernetesScaler, times(2)).scaleByNum(5);
    }

    @Test
    public void testExpiredWindowFallsBackToFixedPolicy() {
        final ScalePolicyDO windowPolicy = windowPolicy(5, now.minusSeconds(120), now.minusSeconds(60));
        windowPolicy.setSort(2);
        final ScalePolicyDO fixedPolicy = policy("1", 1, 3);
        fixedPolicy.setNum(2);
        policyCache.initialize(List.of(windowPolicy, fixedPolicy));

        service.executeScaling();

        verify(kubernetesScaler).scaleByNum(2);
        verify(schedulerManager, never()).registerOnceTask(any(), any(), any());
    }

    @Test
    public void testUnboundedWindowAppliesTarget() {
        final ScalePolicyDO windowPolicy = policy("2", 1, 2);
        windowPolicy.setNum(4);
        policyCache.initialize(List.of(windowPolicy));

        service.executeScaling();

        verify(kubernetesScaler).scaleByNum(4);
        verify(schedulerManager, never()).registerOnceTask(any(), any(), any());
    }

    @Test
    public void testActivationCallbackAppliesTargetAtWindowStart() {
        final Instant begin = now.plusSeconds(60);
        policyCache.initialize(List.of(windowPolicy(3, begin, begin.plusSeconds(60))));
        final ArgumentCaptor<Runnable> callback = ArgumentCaptor.forClass(Runnable.class);

        service.executeScaling();
        verify(schedulerManager).registerOnceTask(eq("ScaleWindowActivation"), callback.capture(), eq(begin));

        now = begin;
        callback.getValue().run();

        verify(kubernetesScaler).scaleByNum(3);
    }

    private ScalePolicyDO windowPolicy(final int num, final Instant begin, final Instant end) {
        final ScalePolicyDO policy = policy("2", 1, 2);
        policy.setNum(num);
        policy.setBeginTime(Date.from(begin));
        policy.setEndTime(Date.from(end));
        return policy;
    }

    private void verifyWindowTaskCancellations(final int times) {
        verify(schedulerManager, times(times)).cancelMonitorTask("ScaleWindowActivation");
        verify(schedulerManager, times(times)).cancelMonitorTask("ScaleWindowDeactivation");
    }

    private ScalePolicyDO policy(final String id, final int status, final int sort) {
        final ScalePolicyDO policy = new ScalePolicyDO();
        policy.setId(id);
        policy.setStatus(status);
        policy.setSort(sort);
        return policy;
    }
}
