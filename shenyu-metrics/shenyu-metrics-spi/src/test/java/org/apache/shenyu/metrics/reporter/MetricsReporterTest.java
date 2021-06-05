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

package org.apache.shenyu.metrics.reporter;

import org.apache.shenyu.metrics.entity.Metric;
import org.apache.shenyu.metrics.enums.MetricType;
import org.apache.shenyu.metrics.spi.MetricsRegister;
import org.junit.Test;

import java.util.Collection;
import java.util.Collections;

/**
 * MetricsReporterTest.
 */
public class MetricsReporterTest {

    private static MetricsRegister metricsRegister;

    @Test
    public void testRegister() {
        MetricsReporter.register(metricsRegister);
    }

    @Test(expected = RuntimeException.class)
    public void testRegisterMetrics() {
        Collection<Metric> metricsCounterType = Collections.singletonList(new Metric(MetricType.COUNTER,
                "name", "document", Collections.EMPTY_LIST));
        MetricsReporter.registerMetrics(metricsCounterType);
        Collection<Metric> metricsGaugeType = Collections.singletonList(new Metric(MetricType.GAUGE,
                "name", "document", Collections.EMPTY_LIST));
        MetricsReporter.registerMetrics(metricsGaugeType);
        Collection<Metric> metricsHistogramType = Collections.singletonList(new Metric(MetricType.HISTOGRAM,
                "name", "document", Collections.EMPTY_LIST));
        MetricsReporter.registerMetrics(metricsHistogramType);
        Collection<Metric> metricsNullType = Collections.singletonList(new Metric(null,
                "name", "document", Collections.EMPTY_LIST));
        MetricsReporter.registerMetrics(metricsNullType);
    }

    @Test
    public void testRegisterCounter() {
        MetricsReporter.registerCounter("name", new String[]{"labelNames"}, "document");
        MetricsReporter.registerCounter("name", "document");
    }

    @Test
    public void testRegisterGauge() {
        MetricsReporter.registerGauge("name", new String[]{"labelNames"}, "document");
        MetricsReporter.registerGauge("name", "document");
    }

    @Test
    public void testHistogram() {
        MetricsReporter.registerHistogram("name", new String[]{"labelNames"}, "document");
        MetricsReporter.registerHistogram("name", "document");
    }

    @Test
    public void testCounterIncrement() {
        MetricsReporter.counterIncrement("name", new String[]{"labelValues"});
        MetricsReporter.counterIncrement("name");
        MetricsReporter.counterIncrement("name", new String[]{"labelValues"}, 1);
    }

    @Test
    public void testGaugeIncrement() {
        MetricsReporter.gaugeIncrement("name", new String[]{"labelValues"});
        MetricsReporter.gaugeIncrement("name");
    }

    @Test
    public void testGaugeDecrement() {
        MetricsReporter.gaugeDecrement("name", new String[]{"labelValues"});
        MetricsReporter.gaugeDecrement("name");
    }

    @Test
    public void testRecordTime() {
        MetricsReporter.recordTime("name", new String[]{"labelValues"}, 1L);
        MetricsReporter.recordTime("name", 1L);
    }
}
