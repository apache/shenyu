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
import org.apache.shenyu.common.metrics.LoggingQueueMetrics;
import org.apache.shenyu.common.metrics.LoggingQueueMetrics.Mode;
import org.apache.shenyu.plugin.metrics.constant.LabelNames;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * Verify exported values, late registration and collector lifecycle with real scrapes.
 */
class LoggingQueueMetricsCollectorTest {

    private final AtomicLong size = new AtomicLong();

    private final AtomicLong capacity = new AtomicLong(10);

    private final LoggingQueueMetrics metrics = new LoggingQueueMetrics(getClass(), Mode.MULTI, size::get, capacity::get);

    private final CollectorRegistry registry = new CollectorRegistry();

    @AfterEach
    void tearDown() {
        metrics.unregister();
        registry.clear();
    }

    @Test
    void exportsExistingAdmissionsAndLiveQueueState() {
        metrics.register();
        metrics.recordAdmission(true);
        metrics.recordAdmission(false);
        size.set(1);
        new LoggingQueueMetricsCollector().register(registry);
        assertEquals(1.0, sample(LabelNames.LOGGING_QUEUE_ACCEPTED_TOTAL));
        assertEquals(1.0, sample(LabelNames.LOGGING_QUEUE_DROPPED_TOTAL));
        assertEquals(1.0, sample(LabelNames.LOGGING_QUEUE_SIZE));
        assertEquals(10.0, sample(LabelNames.LOGGING_QUEUE_CAPACITY));
        // No new admission: a scrape still observes draining and configuration changes.
        size.set(0);
        capacity.set(20);
        assertEquals(0.0, sample(LabelNames.LOGGING_QUEUE_SIZE));
        assertEquals(20.0, sample(LabelNames.LOGGING_QUEUE_CAPACITY));
        assertEquals(1.0, sample(LabelNames.LOGGING_QUEUE_DROPPED_TOTAL));
    }

    @Test
    void discoversCollectorsStartedAfterMetricsAndRemovesStoppedCollectors() {
        new LoggingQueueMetricsCollector().register(registry);
        assertNull(sample(LabelNames.LOGGING_QUEUE_SIZE));
        metrics.register();
        assertEquals(0.0, sample(LabelNames.LOGGING_QUEUE_DROPPED_TOTAL));
        assertEquals(10.0, sample(LabelNames.LOGGING_QUEUE_CAPACITY));
        metrics.recordAdmission(false);
        metrics.unregister();
        assertNull(sample(LabelNames.LOGGING_QUEUE_SIZE));
        assertNull(sample(LabelNames.LOGGING_QUEUE_DROPPED_TOTAL));
        metrics.register();
        assertEquals(1.0, sample(LabelNames.LOGGING_QUEUE_DROPPED_TOTAL));
    }

    @Test
    void exportsOnlyCollectorAndModeLabels() {
        metrics.register();
        new LoggingQueueMetricsCollector().register(registry);
        registry.metricFamilySamples().asIterator().forEachRemaining(family ->
                family.samples.forEach(sample -> assertEquals(List.of("collector", "mode"), sample.labelNames)));
    }

    private Double sample(final String name) {
        return registry.getSampleValue(name, new String[]{"collector", "mode"}, new String[]{getClass().getName(), "multi"});
    }
}
