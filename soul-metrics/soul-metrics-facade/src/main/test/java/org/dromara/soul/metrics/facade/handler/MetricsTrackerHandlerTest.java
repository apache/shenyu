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

package org.dromara.soul.metrics.facade.handler;

import org.dromara.soul.metrics.api.HistogramMetricsTrackerDelegate;
import org.dromara.soul.metrics.api.SummaryMetricsTrackerDelegate;
import org.dromara.soul.metrics.enums.MetricsLabelEnum;
import org.dromara.soul.metrics.prometheus.PrometheusMetricsTrackerManager;
import org.junit.Before;
import org.junit.Test;

import java.util.Optional;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

/**
 * The Test For MetricsTrackerHandler.
 *
 * @author nuo-promise
 **/
public class MetricsTrackerHandlerTest {

    private static final String REQUEST_TOTAL_COUNT_NAME = "request_total";

    private static final String REQUEST_HISTOGRAM_NAME = "request_latency_histogram_millis";

    private static final String REQUEST_SUMMARY_NAME = "request_latency_summary_millis";

    private MetricsTrackerHandler metricsTrackerHandler;

    @Before
    public void setUp() {
        metricsTrackerHandler = new MetricsTrackerHandler();
        metricsTrackerHandler.init(true, 10, new PrometheusMetricsTrackerManager());
    }
    @Test
    public void getInstance() {
        assertNotNull(metricsTrackerHandler);
    }

    @Test
    public void init() {
        assertNotNull(metricsTrackerHandler.getMetricsTrackerManager());
    }

    @Test
    public void counterInc() {
        metricsTrackerHandler.counterInc(MetricsLabelEnum.REQUEST_TOTAL.getName(), REQUEST_TOTAL_COUNT_NAME);
    }

    @Test
    public void gaugeInc() {
        metricsTrackerHandler.gaugeInc(MetricsLabelEnum.REQUEST_TOTAL.getName(), REQUEST_TOTAL_COUNT_NAME);
    }

    @Test
    public void gaugeDec() {
        metricsTrackerHandler.gaugeDec(MetricsLabelEnum.REQUEST_TOTAL.getName(), REQUEST_TOTAL_COUNT_NAME);
    }

    @Test
    public void histogramStartTimer() {
        assertTrue(metricsTrackerHandler.histogramStartTimer(MetricsLabelEnum.REQUEST_LATENCY.getName(), REQUEST_HISTOGRAM_NAME).isPresent());

    }

    @Test
    public void histogramObserveDuration() {
        Optional<HistogramMetricsTrackerDelegate> histogramMetricsTrackerDelegateOptional = metricsTrackerHandler.histogramStartTimer(MetricsLabelEnum.REQUEST_LATENCY.getName(), REQUEST_HISTOGRAM_NAME);
        histogramMetricsTrackerDelegateOptional.ifPresent(histogramMetricsTrackerDelegate -> metricsTrackerHandler.histogramObserveDuration(histogramMetricsTrackerDelegate));
    }

    @Test
    public void summaryStartTimer() {
        assertTrue(metricsTrackerHandler.summaryStartTimer(MetricsLabelEnum.REQUEST_LATENCY.getName(), REQUEST_SUMMARY_NAME).isPresent());
    }

    @Test
    public void summaryObserveDuration() {
        Optional<SummaryMetricsTrackerDelegate> summaryMetricsTrackerDelegate = metricsTrackerHandler.summaryStartTimer(MetricsLabelEnum.REQUEST_LATENCY.getName(), REQUEST_SUMMARY_NAME);
        summaryMetricsTrackerDelegate.ifPresent(metricsTrackerDelegate -> metricsTrackerHandler.summaryObserveDuration(metricsTrackerDelegate));
    }

    @Test
    public void close() {
        metricsTrackerHandler.close();
        assertTrue(metricsTrackerHandler.getExecutorService().isShutdown());
    }

    @Test
    public void handlerGaugeDec() {
        metricsTrackerHandler.handlerGaugeDec(MetricsLabelEnum.REQUEST_TOTAL.getName(), REQUEST_TOTAL_COUNT_NAME);
    }

    @Test
    public void getMetricsTrackerManager() {
        assertNotNull(metricsTrackerHandler.getMetricsTrackerManager());
    }

    @Test
    public void getExecutorService() {
        assertNotNull(metricsTrackerHandler.getExecutorService());
    }
}
