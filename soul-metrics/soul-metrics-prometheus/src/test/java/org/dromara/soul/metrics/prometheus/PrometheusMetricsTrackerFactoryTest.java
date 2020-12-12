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

package org.dromara.soul.metrics.prometheus;

import org.dromara.soul.metrics.api.MetricsTracker;
import org.dromara.soul.metrics.api.MetricsTrackerFactory;
import org.dromara.soul.metrics.enums.MetricsLabelEnum;
import org.dromara.soul.metrics.enums.MetricsTypeEnum;
import org.junit.Test;
import java.util.Optional;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

/**
 * The Test Case For PrometheusMetricsTrackerFactory.
 *
 * @author dengliming
 **/
public final class PrometheusMetricsTrackerFactoryTest {

    private final MetricsTrackerFactory metricsTrackerFactory = new PrometheusMetricsTrackerFactory();

    @Test
    public void testCreateRequestTotalCounter() {
        Optional<MetricsTracker> actual = metricsTrackerFactory.create(MetricsTypeEnum.COUNTER.name(), MetricsLabelEnum.REQUEST_TOTAL.getName());
        assertTrue(actual.isPresent());
    }

    @Test
    public void testCreateHttpRequestCounter() {
        Optional<MetricsTracker> actual = metricsTrackerFactory.create(MetricsTypeEnum.COUNTER.name(), MetricsLabelEnum.HTTP_REQUEST_TOTAL.getName());
        assertTrue(actual.isPresent());
    }

    @Test
    public void testCreateRequestLatencyHistogram() {
        Optional<MetricsTracker> actual = metricsTrackerFactory.create(MetricsTypeEnum.HISTOGRAM.name(), MetricsLabelEnum.REQUEST_LATENCY.getName());
        assertTrue(actual.isPresent());
    }

    @Test
    public void testCreateRequestLatencySummary() {
        Optional<MetricsTracker> actual = metricsTrackerFactory.create(MetricsTypeEnum.SUMMARY.name(), MetricsLabelEnum.REQUEST_LATENCY.getName());
        assertTrue(actual.isPresent());
    }

    @Test
    public void testCreateNoneExistent() {
        Optional<MetricsTracker> actual = metricsTrackerFactory.create("none", "none");
        assertFalse(actual.isPresent());
    }
}
