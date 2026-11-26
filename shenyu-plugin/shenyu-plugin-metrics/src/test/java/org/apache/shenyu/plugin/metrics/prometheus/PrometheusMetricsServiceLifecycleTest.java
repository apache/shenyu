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

package org.apache.shenyu.plugin.metrics.prometheus;

import io.prometheus.client.CollectorRegistry;
import io.prometheus.client.Counter;
import io.prometheus.client.Gauge;
import org.apache.shenyu.common.config.ShenyuConfig.MetricsConfig;
import org.apache.shenyu.common.metrics.LoggingQueueMetrics;
import org.apache.shenyu.common.metrics.LoggingQueueMetrics.Mode;
import org.apache.shenyu.plugin.metrics.constant.LabelNames;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.lang.reflect.Field;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Tests the Prometheus exporter against the shared application registry.
 */
public final class PrometheusMetricsServiceLifecycleTest {

    @Test
    public void testRestartPreservesSharedRegistry() throws ReflectiveOperationException {
        CollectorRegistry registry = CollectorRegistry.defaultRegistry;
        String externalName = "external_prometheus_exporter_lifecycle_test";
        String requestName = "shenyu_prometheus_exporter_lifecycle_test";
        String[] queueLabels = {"collector", "mode"};
        String[] queueLabelValues = {getClass().getName(), "single"};
        PrometheusMetricsService service = new PrometheusMetricsService();
        PrometheusMetricsRegister metricsRegister = new PrometheusMetricsRegister();
        MetricsConfig config = loopbackConfig();
        Gauge externalGauge = Gauge.build().name(externalName).help("Unrelated application metric").register();
        LoggingQueueMetrics queue = new LoggingQueueMetrics(getClass(), Mode.SINGLE, () -> 2, () -> 10);
        try {
            externalGauge.set(7);
            queue.register();
            metricsRegister.registerCounter(requestName, null, "ShenYu request metric");
            service.start(config, metricsRegister);
            assertNotNull(service.getServer());
            metricsRegister.counterIncrement(requestName, null, 1);
            assertEquals(2.0, registry.getSampleValue(LabelNames.LOGGING_QUEUE_SIZE, queueLabels, queueLabelValues));

            service.stop();
            service.stop();
            assertEquals(7.0, registry.getSampleValue(externalName));
            assertNull(registry.getSampleValue(LabelNames.LOGGING_QUEUE_SIZE, queueLabels, queueLabelValues));

            service.start(config, metricsRegister);
            assertNotNull(service.getServer());
            metricsRegister.registerCounter(requestName, null, "ShenYu request metric");
            metricsRegister.counterIncrement(requestName, null, 1);
            assertEquals(2.0, registry.getSampleValue(requestName + "_total"));
            assertEquals(2.0, registry.getSampleValue(LabelNames.LOGGING_QUEUE_SIZE, queueLabels, queueLabelValues));
        } finally {
            service.stop();
            queue.unregister();
            if (Objects.nonNull(registry.getSampleValue(externalName))) {
                registry.unregister(externalGauge);
            }
            removeTestCounter(metricsRegister, requestName, registry);
        }
    }

    @Test
    public void testFailedBindReleasesOwnedCollectorsForRetry() throws IOException {
        PrometheusMetricsService service = new PrometheusMetricsService();
        PrometheusMetricsRegister metricsRegister = new PrometheusMetricsRegister();
        MetricsConfig config = loopbackConfig();
        try (ServerSocket occupied = new ServerSocket(0, 1, InetAddress.getByName(config.getHost()))) {
            config.setPort(occupied.getLocalPort());
            service.start(config, metricsRegister);
            assertNull(service.getServer());
            assertFalse(service.getRegistered().get());

            config.setPort(0);
            service.start(config, metricsRegister);
            assertNotNull(service.getServer());
        } finally {
            service.stop();
        }
    }

    @Test
    public void testFailedCollectorRegistrationReleasesEarlierOwnedCollectors() {
        CollectorRegistry registry = CollectorRegistry.defaultRegistry;
        PrometheusMetricsService service = new PrometheusMetricsService();
        PrometheusMetricsRegister metricsRegister = new PrometheusMetricsRegister();
        MetricsConfig config = loopbackConfig();
        AgentCatalogMetricsCollector external = new AgentCatalogMetricsCollector().register();
        try {
            assertThrows(IllegalArgumentException.class, () -> service.start(config, metricsRegister));
            assertFalse(service.getRegistered().get());
        } finally {
            registry.unregister(external);
            service.stop();
        }
        try {
            service.start(config, metricsRegister);
            assertNotNull(service.getServer());
        } finally {
            service.stop();
        }
    }

    @Test
    public void testInvalidPortDoesNotRegisterCollectors() {
        PrometheusMetricsService service = new PrometheusMetricsService();
        PrometheusMetricsRegister metricsRegister = new PrometheusMetricsRegister();
        MetricsConfig config = loopbackConfig();
        config.setPort(-1);
        try {
            assertThrows(IllegalArgumentException.class, () -> service.start(config, metricsRegister));
            assertFalse(service.getRegistered().get());
        } finally {
            service.stop();
        }
    }

    private MetricsConfig loopbackConfig() {
        MetricsConfig config = new MetricsConfig();
        config.setHost("127.0.0.1");
        config.setPort(0);
        config.setProps(new Properties());
        return config;
    }

    private void removeTestCounter(final PrometheusMetricsRegister metricsRegister, final String requestName, final CollectorRegistry registry)
            throws ReflectiveOperationException {
        Field field = PrometheusMetricsRegister.class.getDeclaredField("COUNTER_MAP");
        field.setAccessible(true);
        Counter counter = (Counter) ((Map<?, ?>) field.get(metricsRegister)).remove(requestName);
        if (Objects.nonNull(counter) && Objects.nonNull(registry.getSampleValue(requestName + "_total"))) {
            registry.unregister(counter);
        }
    }
}
