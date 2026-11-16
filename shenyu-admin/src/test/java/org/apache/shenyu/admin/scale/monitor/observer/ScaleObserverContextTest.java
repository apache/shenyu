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

package org.apache.shenyu.admin.scale.monitor.observer;

import org.apache.shenyu.admin.model.entity.ScaleRuleDO;
import org.apache.shenyu.admin.scale.collector.PrometheusMetricsProvider;
import org.apache.shenyu.admin.scale.collector.provider.MetricData;
import org.apache.shenyu.admin.scale.monitor.subject.MetricsMonitor;
import org.apache.shenyu.admin.scale.monitor.subject.cache.ScaleRuleCache;
import org.apache.shenyu.admin.scale.scaler.KubernetesScaler;
import org.apache.shenyu.admin.scale.scaler.dynamic.ScaleAction;
import org.apache.shenyu.admin.scale.scaler.dynamic.ScaleRuleEvaluator;
import org.junit.jupiter.api.Test;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Tests Spring context registration of the scaling observer.
 */
public final class ScaleObserverContextTest {

    private static final String METRIC_NAME = "cpu_usage";

    @Test
    public void testScaleObserverIsDiscoveredAndReceivesMetricUpdate() throws Exception {
        final KubernetesScaler kubernetesScaler = mock(KubernetesScaler.class);
        final PrometheusMetricsProvider metricsProvider = mock(PrometheusMetricsProvider.class);
        final ScaleRuleCache scaleRuleCache = mock(ScaleRuleCache.class);
        try (AnnotationConfigApplicationContext context = new AnnotationConfigApplicationContext()) {
            context.registerBean(KubernetesScaler.class, () -> kubernetesScaler);
            context.registerBean(PrometheusMetricsProvider.class, () -> metricsProvider);
            context.registerBean(ScaleRuleCache.class, () -> scaleRuleCache);
            context.registerBean(ScaleRuleEvaluator.class, ScaleRuleEvaluator::new);
            context.registerBean(MetricsMonitor.class);
            context.scan("org.apache.shenyu.admin.scale.monitor.observer");
            context.refresh();

            final Map<String, Observer> observerBeans = context.getBeansOfType(Observer.class);
            assertEquals(1, observerBeans.size());
            assertInstanceOf(ScaleObserver.class, observerBeans.values().iterator().next());

            final ScaleRuleDO rule = ScaleRuleDO.builder()
                    .id("rule")
                    .metricName(METRIC_NAME)
                    .sort(1)
                    .status(1)
                    .minimum("40")
                    .maximum("80")
                    .build();
            when(scaleRuleCache.getAllRules()).thenReturn(List.of(rule));
            when(metricsProvider.getMetricData(METRIC_NAME)).thenReturn(new MetricData(METRIC_NAME, 81.0));

            context.getBean(MetricsMonitor.class).monitorMetrics();

            verify(kubernetesScaler, times(1)).scaleByAction(new ScaleAction(ScaleAction.ActionType.SCALE_UP));
        }
    }
}
