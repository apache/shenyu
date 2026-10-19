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
import org.mockito.InOrder;

import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

/**
 * Tests for scaling policy transitions and monitoring task cancellation.
 */
public final class ScaleServiceTest {

    private TaskSchedulerManager schedulerManager;

    private KubernetesScaler kubernetesScaler;

    private ScalePolicyCache policyCache;

    private ScaleService service;

    @BeforeEach
    public void setUp() {
        schedulerManager = mock(TaskSchedulerManager.class);
        kubernetesScaler = mock(KubernetesScaler.class);
        policyCache = new ScalePolicyCache();

        final ScaleProperties properties = new ScaleProperties();
        properties.setMonitorInterval(1000L);

        service = new ScaleService(
                mock(MetricsMonitor.class), schedulerManager,
                policyCache, properties, kubernetesScaler);
    }

    @Test
    public void testDisableLastPolicyStopsDynamicMonitoring() {
        policyCache.initialize(List.of(policy("3", 1, 1)));

        service.executeScaling();

        policyCache.updatePolicy(policy("3", 0, 1));
        service.executeScaling();
        service.executeScaling();

        final InOrder order = inOrder(schedulerManager);
        order.verify(schedulerManager).registerMonitorTask(
                eq("DynamicScalingTask"), any(Runnable.class), eq(1000L));
        order.verify(schedulerManager, times(2)).cancelMonitorTask("DynamicScalingTask");
        order.verifyNoMoreInteractions();

        verifyNoInteractions(kubernetesScaler);
    }

    @Test
    public void testEmptyPoliciesCancelDynamicMonitoring() {
        service.executeScaling();

        verify(schedulerManager).cancelMonitorTask("DynamicScalingTask");
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
        order.verifyNoMoreInteractions();
    }

    private ScalePolicyDO policy(final String id, final int status, final int sort) {
        final ScalePolicyDO policy = new ScalePolicyDO();
        policy.setId(id);
        policy.setStatus(status);
        policy.setSort(sort);
        return policy;
    }
}
