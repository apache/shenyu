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

import com.google.common.base.Preconditions;
import lombok.Getter;
import org.dromara.soul.metrics.api.HistogramMetricsTrackerDelegate;
import org.dromara.soul.metrics.api.SummaryMetricsTrackerDelegate;
import org.dromara.soul.metrics.config.MetricsConfig;
import org.dromara.soul.metrics.enums.MetricsLabelEnum;
import org.dromara.soul.metrics.facade.handler.MetricsTrackerHandler;
import org.dromara.soul.metrics.spi.MetricsTrackerManager;
import org.dromara.soul.spi.ExtensionLoader;
import org.junit.Before;
import org.junit.Test;

import java.util.Optional;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.Assert.assertTrue;

/**
 * The Test Case For MetricsTrackerFacade.
 *
 * @author nuo-promise
 **/
public class MetricsTrackerFacadeTest {

    private static final String REQUEST_TOTAL_COUNT_NAME = "request_total";

    private static final String REQUEST_HISTOGRAM_NAME = "request_latency_histogram_millis";

    private static final String REQUEST_SUMMARY_NAME = "request_latency_summary_millis";

    @Getter
    private MetricsTrackerManager metricsTrackerManager;

    private final AtomicBoolean isStarted = new AtomicBoolean(false);

    @Before
    public void setUp() {
        if (this.isStarted.compareAndSet(false, true)) {
            MetricsConfig  metricsConfig = new MetricsConfig("facade","localhost", 1234, Boolean.FALSE,
                    10, "{\"jmxUrl\":\"service:jmx:rmi:///jndi/rmi://127.0.0.1:1234/jmxrmi\"}",new Properties());
            metricsTrackerManager = ExtensionLoader.getExtensionLoader(MetricsTrackerManager.class).getJoin(metricsConfig.getMetricsName());
            Preconditions.checkNotNull(metricsTrackerManager,
                    "Can not find metrics tracker manager with metrics name : %s in metrics configuration.", metricsConfig.getMetricsName());
            metricsTrackerManager.start(metricsConfig);
            Integer threadCount = Optional.ofNullable(metricsConfig.getThreadCount()).orElse(Runtime.getRuntime().availableProcessors());
            MetricsTrackerHandler.getInstance().init(metricsConfig.getAsync(), threadCount, metricsTrackerManager);
        } else {
            System.out.println("Test metrics tracker has started !");
        }

    }

    @Test
    public void counterInc() {
        if (isStarted.get()) {
            MetricsTrackerHandler.getInstance().counterInc(MetricsLabelEnum.REQUEST_TOTAL.getName(), REQUEST_TOTAL_COUNT_NAME);
        }
    }

    @Test
    public void gaugeInc() {
        if (isStarted.get()) {
            MetricsTrackerHandler.getInstance().gaugeInc(MetricsLabelEnum.REQUEST_TOTAL.getName(), REQUEST_TOTAL_COUNT_NAME);
        }
    }

    @Test
    public void gaugeDec() {
        if (isStarted.get()) {
            MetricsTrackerHandler.getInstance().gaugeDec(MetricsLabelEnum.REQUEST_TOTAL.getName(), REQUEST_TOTAL_COUNT_NAME);
        }
    }

    @Test
    public void histogramStartTimer() {
        MetricsTrackerHandler.getInstance().histogramStartTimer(MetricsLabelEnum.REQUEST_LATENCY.getName(), REQUEST_HISTOGRAM_NAME);
    }

    @Test
    public void histogramObserveDuration() {
        if (isStarted.get()) {
            Optional<HistogramMetricsTrackerDelegate> histogramMetricsTrackerDelegate = MetricsTrackerHandler.getInstance().histogramStartTimer(MetricsLabelEnum.REQUEST_LATENCY.getName(), REQUEST_HISTOGRAM_NAME);
            histogramMetricsTrackerDelegate.ifPresent(metricsTrackerDelegate -> MetricsTrackerHandler.getInstance().histogramObserveDuration(metricsTrackerDelegate));

        }
    }

    @Test
    public void summaryStartTimer() {
        MetricsTrackerHandler.getInstance().summaryStartTimer(MetricsLabelEnum.REQUEST_LATENCY.getName(), REQUEST_SUMMARY_NAME);
    }

    @Test
    public void summaryObserveDuration() {
        if (isStarted.get()) {
            Optional<SummaryMetricsTrackerDelegate> summaryMetricsTrackerDelegate = MetricsTrackerHandler.getInstance().summaryStartTimer(MetricsLabelEnum.REQUEST_LATENCY.getName(), REQUEST_SUMMARY_NAME);
            summaryMetricsTrackerDelegate.ifPresent(metricsTrackerDelegate -> MetricsTrackerHandler.getInstance().summaryObserveDuration(metricsTrackerDelegate));

        }
    }

    @Test
    public void isStarted() {
        assertTrue(isStarted.get());
    }
}