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

import io.prometheus.client.Collector;
import io.prometheus.client.CollectorRegistry;
import io.prometheus.client.Counter;
import io.prometheus.client.Gauge;
import io.prometheus.client.GaugeMetricFamily;
import io.prometheus.client.SummaryMetricFamily;
import io.prometheus.client.hotspot.ClassLoadingExports;
import org.apache.shenyu.common.config.ShenyuConfig.MetricsConfig;
import org.apache.shenyu.common.metrics.LoggingQueueMetrics;
import org.apache.shenyu.common.metrics.LoggingQueueMetrics.Mode;
import org.apache.shenyu.plugin.metrics.constant.LabelNames;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.io.IOException;
import java.lang.reflect.Field;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests the Prometheus exporter against the shared application registry.
 */
public final class PrometheusMetricsServiceLifecycleTest {

    private static final String JVM_ENABLED = "jvm_enabled";

    private static final String JVM_CLASSES_LOADED = "jvm_classes_currently_loaded";

    private static final String JVM_THREADS_CURRENT = "jvm_threads_current";

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
    public void testJvmCollectorsFollowExporterConfiguration() throws IOException, InterruptedException, ReflectiveOperationException {
        CollectorRegistry registry = CollectorRegistry.defaultRegistry;
        String externalName = "external_prometheus_exporter_jvm_test";
        String requestName = "shenyu_prometheus_exporter_jvm_test";
        PrometheusMetricsService service = new PrometheusMetricsService();
        PrometheusMetricsRegister metricsRegister = new PrometheusMetricsRegister();
        MetricsConfig config = loopbackConfig();
        Gauge externalGauge = Gauge.build().name(externalName).help("Unrelated application metric").register();
        try {
            externalGauge.set(7);
            metricsRegister.registerCounter(requestName, null, "ShenYu request metric");
            config.getProps().setProperty(JVM_ENABLED, "true");
            service.start(config, metricsRegister);
            metricsRegister.counterIncrement(requestName, null, 1);
            String enabled = scrape(service);
            assertTrue(hasSample(enabled, JVM_CLASSES_LOADED));
            assertTrue(hasSample(enabled, JVM_THREADS_CURRENT));
            assertTrue(enabled.contains(requestName + "_total 1.0"));

            service.stop();
            config.getProps().setProperty(JVM_ENABLED, "false");
            service.start(config, metricsRegister);
            metricsRegister.counterIncrement(requestName, null, 1);
            String disabled = scrape(service);
            assertFalse(hasSample(disabled, JVM_CLASSES_LOADED));
            assertFalse(hasSample(disabled, JVM_THREADS_CURRENT));
            assertTrue(disabled.contains(externalName + " 7.0"));
            assertTrue(disabled.contains(requestName + "_total 2.0"));

            service.stop();
            config.getProps().setProperty(JVM_ENABLED, "true");
            service.start(config, metricsRegister);
            assertTrue(hasSample(scrape(service), JVM_CLASSES_LOADED));
            assertNull(registry.getSampleValue(JVM_CLASSES_LOADED));
        } finally {
            service.stop();
            if (Objects.nonNull(registry.getSampleValue(externalName))) {
                registry.unregister(externalGauge);
            }
            removeTestCounter(metricsRegister, requestName, registry);
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    public void testRestartDoesNotRemoveExternallyRegisteredJvmCollector(final boolean registeredBeforeStart) throws IOException, InterruptedException {
        CollectorRegistry registry = CollectorRegistry.defaultRegistry;
        ClassLoadingExports external = null;
        PrometheusMetricsService service = new PrometheusMetricsService();
        PrometheusMetricsRegister metricsRegister = new PrometheusMetricsRegister();
        MetricsConfig config = loopbackConfig();
        try {
            if (registeredBeforeStart) {
                external = new ClassLoadingExports().register();
            }
            config.getProps().setProperty(JVM_ENABLED, "true");
            service.start(config, metricsRegister);
            if (!registeredBeforeStart) {
                external = new ClassLoadingExports().register();
            }
            String enabled = scrape(service);
            assertEquals(1L, countSamples(enabled, JVM_CLASSES_LOADED));
            assertTrue(hasSample(enabled, JVM_THREADS_CURRENT));
            String filtered = scrape(service, "?name%5B%5D=" + JVM_CLASSES_LOADED);
            assertEquals(1L, countSamples(filtered, JVM_CLASSES_LOADED));
            assertFalse(hasSample(filtered, JVM_THREADS_CURRENT));

            service.stop();
            config.getProps().setProperty(JVM_ENABLED, "false");
            service.start(config, metricsRegister);
            String disabled = scrape(service);
            assertTrue(hasSample(disabled, JVM_CLASSES_LOADED));
            assertFalse(hasSample(disabled, JVM_THREADS_CURRENT));
        } finally {
            service.stop();
            if (Objects.nonNull(external)) {
                registry.unregister(external);
            }
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    public void testFilteredScrapeKeepsSharedFamilyPrecedence(final boolean summary) throws IOException, InterruptedException {
        String appMetric = "process_cpu_seconds";
        String sharedSample = summary ? appMetric + "_sum" : appMetric;
        String jvmSample = appMetric + "_total";
        Collector external = externalProcessMetric(appMetric, summary).register();
        String unrelatedName = "external_prometheus_exporter_unselected_test";
        AtomicInteger unrelatedCollections = new AtomicInteger();
        Collector unrelated = new Collector() {
            @Override
            public List<MetricFamilySamples> collect() {
                return List.of(new GaugeMetricFamily(unrelatedName, "Unselected metric", unrelatedCollections.incrementAndGet()));
            }
        }.register();
        PrometheusMetricsService service = new PrometheusMetricsService();
        MetricsConfig config = loopbackConfig();
        try {
            config.getProps().setProperty(JVM_ENABLED, "true");
            service.start(config, new PrometheusMetricsRegister());
            String full = scrape(service);
            assertTrue(full.contains(sharedSample + " 234.0"));
            assertFalse(hasSample(full, jvmSample));
            int collected = unrelatedCollections.get();
            assertFalse(hasSample(scrape(service, "?name%5B%5D=" + jvmSample), jvmSample));
            assertEquals(collected, unrelatedCollections.get());
            assertTrue(scrape(service, "?name%5B%5D=" + sharedSample).contains(sharedSample + " 234.0"));
            assertFalse(hasSample(scrape(service, "?name%5B%5D=missing_metric"), unrelatedName));
            assertEquals(collected, unrelatedCollections.get());
        } finally {
            service.stop();
            CollectorRegistry.defaultRegistry.unregister(unrelated);
            CollectorRegistry.defaultRegistry.unregister(external);
        }
    }

    @Test
    public void testEmptySharedFamilyFallsBackToJvmForFullAndFilteredScrapes() throws IOException, InterruptedException {
        Gauge external = Gauge.build().name(JVM_CLASSES_LOADED).help("Application loaded classes").labelNames("source").register();
        PrometheusMetricsService service = new PrometheusMetricsService();
        MetricsConfig config = loopbackConfig();
        try {
            config.getProps().setProperty(JVM_ENABLED, "true");
            service.start(config, new PrometheusMetricsRegister());
            for (String query : List.of("", "?name%5B%5D=" + JVM_CLASSES_LOADED)) {
                String fallback = scrape(service, query);
                assertEquals(1L, countSamples(fallback, JVM_CLASSES_LOADED));
                assertFalse(fallback.contains("# HELP " + JVM_CLASSES_LOADED + " Application loaded classes"));
                assertEquals(1L, fallback.lines().filter(line -> line.startsWith("# TYPE " + JVM_CLASSES_LOADED + " ")).count());
            }

            external.labels("app").set(234);
            for (String query : List.of("", "?name%5B%5D=" + JVM_CLASSES_LOADED)) {
                String shared = scrape(service, query);
                assertEquals(1L, countSamples(shared, JVM_CLASSES_LOADED));
                assertTrue(shared.contains(JVM_CLASSES_LOADED + "{source=\"app\",} 234.0"));
                assertEquals(1L, shared.lines().filter(line -> line.startsWith("# TYPE " + JVM_CLASSES_LOADED + " ")).count());
            }
        } finally {
            service.stop();
            CollectorRegistry.defaultRegistry.unregister(external);
        }
    }

    @Test
    public void testFailedBindReleasesOwnedCollectorsForRetry() throws IOException, InterruptedException {
        PrometheusMetricsService service = new PrometheusMetricsService();
        PrometheusMetricsRegister metricsRegister = new PrometheusMetricsRegister();
        MetricsConfig config = loopbackConfig();
        try (ServerSocket occupied = new ServerSocket(0, 1, InetAddress.getByName(config.getHost()))) {
            config.setPort(occupied.getLocalPort());
            config.getProps().setProperty(JVM_ENABLED, "true");
            service.start(config, metricsRegister);
            assertNull(service.getServer());
            assertFalse(service.getRegistered().get());

            config.setPort(0);
            config.getProps().setProperty(JVM_ENABLED, "false");
            service.start(config, metricsRegister);
            assertFalse(hasSample(scrape(service), JVM_CLASSES_LOADED));
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

    private Collector externalProcessMetric(final String name, final boolean summary) {
        if (!summary) {
            Gauge gauge = Gauge.build().name(name).help("Application process time").create();
            gauge.set(234);
            return gauge;
        }
        return new Collector() {
            @Override
            public List<MetricFamilySamples> collect() {
                return List.of(new SummaryMetricFamily(name, "Application process time", 7, 234));
            }
        };
    }

    private String scrape(final PrometheusMetricsService service) throws IOException, InterruptedException {
        return scrape(service, "");
    }

    private String scrape(final PrometheusMetricsService service, final String query) throws IOException, InterruptedException {
        assertNotNull(service.getServer());
        URI uri = URI.create("http://127.0.0.1:" + service.getServer().getPort() + "/metrics" + query);
        HttpRequest request = HttpRequest.newBuilder(uri).timeout(Duration.ofSeconds(5)).build();
        HttpResponse<String> response = HttpClient.newHttpClient().send(request, HttpResponse.BodyHandlers.ofString());
        assertEquals(200, response.statusCode());
        return response.body();
    }

    private boolean hasSample(final String scrape, final String name) {
        return countSamples(scrape, name) > 0;
    }

    private long countSamples(final String scrape, final String name) {
        return scrape.lines().filter(line -> line.startsWith(name + " ") || line.startsWith(name + "{")).count();
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
