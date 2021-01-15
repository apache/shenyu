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

import org.dromara.soul.metrics.api.NoneHistogramMetricsTrackerDelegate;
import org.dromara.soul.metrics.api.NoneSummaryMetricsTrackerDelegate;
import org.dromara.soul.metrics.enums.MetricsLabelEnum;
import org.dromara.soul.metrics.enums.MetricsTypeEnum;
import org.dromara.soul.metrics.facade.executor.MetricsThreadPoolExecutor;
import org.dromara.soul.metrics.prometheus.PrometheusMetricsTrackerFactory;
import org.dromara.soul.metrics.prometheus.PrometheusMetricsTrackerManager;
import org.hamcrest.CoreMatchers;
import org.junit.Before;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.MethodSorters;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.concurrent.Callable;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import static org.hamcrest.Matchers.equalTo;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import static org.junit.Assert.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * The test cases for MetricsTrackerHandler.
 *
 * @author dengliming
 * @author Young Bean
 */
@RunWith(MockitoJUnitRunner.class)
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class MetricsTrackerHandlerTest {

    private MetricsTrackerHandler metricsTrackerHandler;

    private PrometheusMetricsTrackerFactory prometheusMetricsTrackerFactory;

    private PrometheusMetricsTrackerManager prometheusMetricsTrackerManager;

    private MetricsThreadPoolExecutor metricsThreadPoolExecutor;

    private NoneHistogramMetricsTrackerDelegate noneHistogramMetricsTrackerDelegate;

    private NoneSummaryMetricsTrackerDelegate noneSummaryMetricsTrackerDelegate;

    @Before
    public void setUp() {
        metricsTrackerHandler = spy(MetricsTrackerHandler.class);
        prometheusMetricsTrackerFactory = mock(PrometheusMetricsTrackerFactory.class);
        prometheusMetricsTrackerManager = mock(PrometheusMetricsTrackerManager.class);
        metricsThreadPoolExecutor = mock(MetricsThreadPoolExecutor.class);
        noneHistogramMetricsTrackerDelegate = mock(NoneHistogramMetricsTrackerDelegate.class);
        noneSummaryMetricsTrackerDelegate = mock(NoneSummaryMetricsTrackerDelegate.class);
        when(prometheusMetricsTrackerManager.getMetricsTrackerFactory()).thenReturn(prometheusMetricsTrackerFactory);
        ReflectionTestUtils.setField(metricsTrackerHandler, "executorService", metricsThreadPoolExecutor);
        ReflectionTestUtils.setField(metricsTrackerHandler, "metricsTrackerManager", prometheusMetricsTrackerManager);
        ReflectionTestUtils.setField(metricsTrackerHandler, "async", false);
    }

    @Test
    public void testClose() {
        ReflectionTestUtils.setField(metricsTrackerHandler, "async", true);
        metricsTrackerHandler.close();
        assertThat(false, equalTo((Boolean) ReflectionTestUtils.getField(metricsTrackerHandler, "async")));
    }

    @Test
    public void testInit() {
        metricsTrackerHandler.init(false, 1, prometheusMetricsTrackerManager);
        assertThat(prometheusMetricsTrackerManager,
                equalTo(ReflectionTestUtils.getField(metricsTrackerHandler, "metricsTrackerManager")));
    }

    @Test
    public void testGetInstance() {
        assertNotNull(MetricsTrackerHandler.getInstance());
    }

    @Test
    public void testCounterInc() {
        metricsTrackerHandler.counterInc(MetricsLabelEnum.REQUEST_TOTAL.getName());
        verify(prometheusMetricsTrackerFactory, times(1)).create(eq(MetricsTypeEnum.COUNTER.name()), eq(MetricsLabelEnum.REQUEST_TOTAL.getName()));

        // test async call
        ReflectionTestUtils.setField(metricsTrackerHandler, "async", true);
        metricsTrackerHandler.counterInc(MetricsLabelEnum.REQUEST_TOTAL.getName());
        verify(metricsThreadPoolExecutor, times(1)).execute(any(Runnable.class));
    }

    @Test
    public void testGaugeInc() {
        metricsTrackerHandler.gaugeInc(MetricsLabelEnum.REQUEST_TOTAL.getName());
        verify(prometheusMetricsTrackerFactory, times(1)).create(eq(MetricsTypeEnum.GAUGE.name()), eq(MetricsLabelEnum.REQUEST_TOTAL.getName()));

        // test async call
        ReflectionTestUtils.setField(metricsTrackerHandler, "async", true);
        metricsTrackerHandler.gaugeInc(MetricsLabelEnum.REQUEST_TOTAL.getName());
        verify(metricsThreadPoolExecutor, times(1)).execute(any(Runnable.class));
    }

    @Test
    public void testGaugeDec() {
        metricsTrackerHandler.gaugeDec(MetricsLabelEnum.REQUEST_TOTAL.getName());
        verify(prometheusMetricsTrackerFactory, times(1)).create(eq(MetricsTypeEnum.GAUGE.name()), eq(MetricsLabelEnum.REQUEST_TOTAL.getName()));

        // test async call
        ReflectionTestUtils.setField(metricsTrackerHandler, "async", true);
        metricsTrackerHandler.gaugeDec(MetricsLabelEnum.REQUEST_TOTAL.getName());
        verify(metricsThreadPoolExecutor, times(1)).execute(any(Runnable.class));
    }

    @Test
    public void testHistogramStartTimer() throws Exception {
        metricsTrackerHandler.histogramStartTimer(MetricsLabelEnum.REQUEST_LATENCY.getName());
        verify(prometheusMetricsTrackerFactory, times(1)).create(eq(MetricsTypeEnum.HISTOGRAM.name()), eq(MetricsLabelEnum.REQUEST_LATENCY.getName()));

        // test async call
        Future mockedFuture = mock(Future.class);
        when(metricsThreadPoolExecutor.submit(any(Callable.class))).thenReturn(mockedFuture);
        ReflectionTestUtils.setField(metricsTrackerHandler, "async", true);
        metricsTrackerHandler.histogramStartTimer(MetricsLabelEnum.REQUEST_LATENCY.getName());
        verify(metricsThreadPoolExecutor, times(1)).submit(any(Callable.class));

        // test throw exception
        when(mockedFuture.get(anyLong(), any(TimeUnit.class))).thenThrow(InterruptedException.class);
        try {
            metricsTrackerHandler.histogramStartTimer(MetricsLabelEnum.REQUEST_LATENCY.getName());
            fail();
        } catch (IllegalStateException expected) {
            assertThat(expected.getMessage(), CoreMatchers.containsString(
                    "Error while fetching histogram metric with metricsLabel"));
        }
    }

    @Test
    public void testHistogramObserveDuration() {
        metricsTrackerHandler.histogramObserveDuration(noneHistogramMetricsTrackerDelegate);
        verify(noneHistogramMetricsTrackerDelegate, times(1)).observeDuration();

        // test async call
        ReflectionTestUtils.setField(metricsTrackerHandler, "async", true);
        metricsTrackerHandler.histogramObserveDuration(noneHistogramMetricsTrackerDelegate);
        verify(metricsThreadPoolExecutor, times(1)).execute(any(Runnable.class));
    }

    @Test
    public void testSummaryStartTimer() throws Exception {
        metricsTrackerHandler.summaryStartTimer(MetricsLabelEnum.REQUEST_LATENCY.getName());
        verify(prometheusMetricsTrackerFactory, times(1)).create(eq(MetricsTypeEnum.SUMMARY.name()), eq(MetricsLabelEnum.REQUEST_LATENCY.getName()));

        // test async call
        Future mockedFuture = mock(Future.class);
        when(metricsThreadPoolExecutor.submit(any(Callable.class))).thenReturn(mockedFuture);
        ReflectionTestUtils.setField(metricsTrackerHandler, "async", true);
        metricsTrackerHandler.summaryStartTimer(MetricsLabelEnum.REQUEST_LATENCY.getName());
        verify(metricsThreadPoolExecutor, times(1)).submit(any(Callable.class));

        // test throw exception
        when(mockedFuture.get(anyLong(), any(TimeUnit.class))).thenThrow(InterruptedException.class);
        try {
            metricsTrackerHandler.summaryStartTimer(MetricsLabelEnum.REQUEST_LATENCY.getName());
            fail();
        } catch (IllegalStateException expected) {
            assertThat(expected.getMessage(), CoreMatchers.containsString(
                    "Error while fetching summary metric with metricsLabel"));
        }
    }

    @Test
    public void testSummaryObserveDuration() {
        metricsTrackerHandler.summaryObserveDuration(noneSummaryMetricsTrackerDelegate);
        verify(noneSummaryMetricsTrackerDelegate, times(1)).observeDuration();

        // test async call
        ReflectionTestUtils.setField(metricsTrackerHandler, "async", true);
        metricsTrackerHandler.summaryObserveDuration(noneSummaryMetricsTrackerDelegate);
        verify(metricsThreadPoolExecutor, times(1)).execute(any(Runnable.class));
    }
}
