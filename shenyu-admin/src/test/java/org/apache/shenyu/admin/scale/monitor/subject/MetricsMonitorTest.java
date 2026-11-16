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

package org.apache.shenyu.admin.scale.monitor.subject;

import org.apache.shenyu.admin.model.entity.ScaleRuleDO;
import org.apache.shenyu.admin.scale.collector.PrometheusMetricsProvider;
import org.apache.shenyu.admin.scale.collector.provider.MetricData;
import org.apache.shenyu.admin.scale.monitor.observer.Observer;
import org.apache.shenyu.admin.scale.monitor.observer.ScaleObserver;
import org.apache.shenyu.admin.scale.monitor.subject.cache.ScaleRuleCache;
import org.apache.shenyu.admin.scale.scaler.KubernetesScaler;
import org.apache.shenyu.admin.scale.scaler.dynamic.ScaleAction;
import org.apache.shenyu.admin.scale.scaler.dynamic.ScaleRuleEvaluator;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

/**
 * Tests for metric monitoring and scaling observer registration.
 */
public final class MetricsMonitorTest {

    private static final String METRIC_NAME = "cpu_usage";

    private PrometheusMetricsProvider metricsProvider;

    private ScaleRuleCache scaleRuleCache;

    private KubernetesScaler kubernetesScaler;

    private MetricsMonitor metricsMonitor;

    @BeforeEach
    public void setUp() {
        metricsProvider = mock(PrometheusMetricsProvider.class);
        scaleRuleCache = mock(ScaleRuleCache.class);
        kubernetesScaler = mock(KubernetesScaler.class);
        final ScaleObserver scaleObserver = new ScaleObserver(kubernetesScaler, new ScaleRuleEvaluator());
        metricsMonitor = new MetricsMonitor(metricsProvider, scaleRuleCache, List.of(scaleObserver));
    }

    @Test
    public void testMetricAboveMaximumScalesUpOnce() throws Exception {
        prepareMetric(rule(1), 81.0);

        metricsMonitor.monitorMetrics();

        verify(kubernetesScaler).scaleByAction(new ScaleAction(ScaleAction.ActionType.SCALE_UP));
        verifyNoMoreInteractions(kubernetesScaler);
    }

    @Test
    public void testMetricBelowMinimumScalesDownOnce() throws Exception {
        prepareMetric(rule(1), 39.0);

        metricsMonitor.monitorMetrics();

        verify(kubernetesScaler).scaleByAction(new ScaleAction(ScaleAction.ActionType.SCALE_DOWN));
        verifyNoMoreInteractions(kubernetesScaler);
    }

    @Test
    public void testMetricWithinBoundsDoesNotScale() {
        prepareMetric(rule(1), 60.0);

        metricsMonitor.monitorMetrics();

        verifyNoInteractions(kubernetesScaler);
    }

    @Test
    public void testMetricAtMinimumDoesNotScale() {
        prepareMetric(rule(1), 40.0);

        metricsMonitor.monitorMetrics();

        verifyNoInteractions(kubernetesScaler);
    }

    @Test
    public void testMetricAtMaximumDoesNotScale() {
        prepareMetric(rule(1), 80.0);

        metricsMonitor.monitorMetrics();

        verifyNoInteractions(kubernetesScaler);
    }

    @Test
    public void testDisabledRuleIsNotEvaluated() {
        when(scaleRuleCache.getAllRules()).thenReturn(List.of(rule(0)));

        metricsMonitor.monitorMetrics();

        verifyNoInteractions(metricsProvider, kubernetesScaler);
    }

    @Test
    public void testConstructorObserverReceivesMetricUpdate() {
        final Observer observer = mock(Observer.class);
        final MetricsMonitor monitor = new MetricsMonitor(metricsProvider, scaleRuleCache, List.of(observer));
        final ScaleRuleDO rule = rule(1);
        final MetricData metricData = prepareMetric(rule, 60.0);

        monitor.monitorMetrics();

        verify(observer).update(metricData, rule);
        verifyNoMoreInteractions(observer);
    }

    private MetricData prepareMetric(final ScaleRuleDO rule, final double value) {
        final MetricData metricData = new MetricData(METRIC_NAME, value);
        when(scaleRuleCache.getAllRules()).thenReturn(List.of(rule));
        when(metricsProvider.getMetricData(METRIC_NAME)).thenReturn(metricData);
        return metricData;
    }

    private ScaleRuleDO rule(final int status) {
        return ScaleRuleDO.builder()
                .id("rule")
                .metricName(METRIC_NAME)
                .sort(1)
                .status(status)
                .minimum("40")
                .maximum("80")
                .build();
    }
}
