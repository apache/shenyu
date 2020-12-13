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

package org.dromara.soul.metrics.facade;

import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.metrics.api.HistogramMetricsTrackerDelegate;
import org.dromara.soul.metrics.api.SummaryMetricsTrackerDelegate;
import org.dromara.soul.metrics.config.MetricsConfig;
import org.dromara.soul.metrics.enums.MetricsLabelEnum;
import org.junit.Before;
import org.junit.Test;

import java.util.Optional;
import java.util.Properties;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

/**
 * The Test For MetricsTrackerFacade.
 *
 * @author nuo-promise
 **/
@Slf4j
public class MetricsTrackerFacadeTest {

    private MetricsTrackerFacade metricsTrackerFacade;

    private static final String REQUEST_TOTAL_COUNT_NAME = "request_total";

    private static final String REQUEST_HISTOGRAM_NAME = "request_latency_histogram_millis";

    private static final String REQUEST_SUMMARY_NAME = "request_latency_summary_millis";

    @Before
    public void setUp() {
        metricsTrackerFacade = MetricsTrackerFacade.getInstance();
        start();
    }

    @Test
    public void getInstance() {
        assertNotNull(metricsTrackerFacade);
    }

    @Test
    public void start() {
        if (!metricsTrackerFacade.isStarted()) {
            MetricsConfig metricsConfig = new MetricsConfig("prometheus","localhost", 1234, Boolean.FALSE,
                    10, "{\"jmxUrl\":\"service:jmx:rmi:///jndi/rmi://127.0.0.1:1234/jmxrmi\"}",new Properties());
            metricsTrackerFacade.start(metricsConfig);
            assertTrue(metricsTrackerFacade.isStarted());
        } else {
            log.info("metrics tracker has started !");
        }

    }

    @Test
    public void counterInc() {
        metricsTrackerFacade.counterInc(MetricsLabelEnum.REQUEST_TOTAL.getName(), REQUEST_TOTAL_COUNT_NAME);
    }

    @Test
    public void gaugeInc() {
        metricsTrackerFacade.gaugeInc(MetricsLabelEnum.REQUEST_TOTAL.getName(), REQUEST_TOTAL_COUNT_NAME);
    }

    @Test
    public void gaugeDec() {
        metricsTrackerFacade.gaugeDec(MetricsLabelEnum.REQUEST_TOTAL.getName(), REQUEST_TOTAL_COUNT_NAME);

    }

    @Test
    public void histogramStartTimer() {
        assertTrue(metricsTrackerFacade.histogramStartTimer(MetricsLabelEnum.REQUEST_LATENCY.getName(), REQUEST_HISTOGRAM_NAME).isPresent());
    }

    @Test
    public void histogramObserveDuration() {
        Optional<HistogramMetricsTrackerDelegate> histogramMetricsTrackerDelegateOptional = metricsTrackerFacade.histogramStartTimer(MetricsLabelEnum.REQUEST_LATENCY.getName(), REQUEST_HISTOGRAM_NAME);
        histogramMetricsTrackerDelegateOptional.ifPresent(histogramMetricsTrackerDelegate -> metricsTrackerFacade.histogramObserveDuration(histogramMetricsTrackerDelegate));
        stop();
        Optional<HistogramMetricsTrackerDelegate> histogramMetricsTrackerDelegateOptional1 = metricsTrackerFacade.histogramStartTimer(MetricsLabelEnum.REQUEST_LATENCY.getName(), REQUEST_HISTOGRAM_NAME);
        histogramMetricsTrackerDelegateOptional1.ifPresent(histogramMetricsTrackerDelegate -> metricsTrackerFacade.histogramObserveDuration(histogramMetricsTrackerDelegate));
    }

    @Test
    public void summaryStartTimer() {
        assertTrue(metricsTrackerFacade.summaryStartTimer(MetricsLabelEnum.REQUEST_LATENCY.getName(), REQUEST_SUMMARY_NAME).isPresent());
    }

    @Test
    public void summaryObserveDuration() {
        Optional<SummaryMetricsTrackerDelegate> summaryMetricsTrackerDelegate = metricsTrackerFacade.summaryStartTimer(MetricsLabelEnum.REQUEST_LATENCY.getName(), REQUEST_SUMMARY_NAME);
        summaryMetricsTrackerDelegate.ifPresent(metricsTrackerDelegate -> metricsTrackerFacade.summaryObserveDuration(metricsTrackerDelegate));
        stop();
        Optional<SummaryMetricsTrackerDelegate> summaryMetricsTrackerDelegate1 = metricsTrackerFacade.summaryStartTimer(MetricsLabelEnum.REQUEST_LATENCY.getName(), REQUEST_SUMMARY_NAME);
        summaryMetricsTrackerDelegate1.ifPresent(metricsTrackerDelegate -> metricsTrackerFacade.summaryObserveDuration(metricsTrackerDelegate));
    }

    @Test
    public void stop() {
        assertNotNull(metricsTrackerFacade.getMetricsTrackerManager());
        metricsTrackerFacade.getMetricsTrackerManager().stop();
        metricsTrackerFacade.stop();
        assertFalse(metricsTrackerFacade.isStarted());
    }

    @Test
    public void isStarted() {
        assertTrue(metricsTrackerFacade.isStarted());
    }

    @Test
    public void getMetricsTrackerManager() {
        assertNotNull(metricsTrackerFacade.getMetricsTrackerManager());
    }
}
