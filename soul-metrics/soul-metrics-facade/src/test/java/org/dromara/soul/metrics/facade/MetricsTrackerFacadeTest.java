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

import org.dromara.soul.metrics.api.HistogramMetricsTrackerDelegate;
import org.dromara.soul.metrics.api.SummaryMetricsTrackerDelegate;
import org.dromara.soul.metrics.config.MetricsConfig;
import org.dromara.soul.metrics.facade.handler.MetricsTrackerHandler;
import org.dromara.soul.metrics.spi.MetricsTrackerManager;
import org.dromara.soul.spi.ExtensionLoader;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.MethodSorters;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.MockitoJUnitRunner;

import java.util.Properties;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * The test cases for MetricsTrackerFacade.
 *
 * @author dengliming
 */
@RunWith(MockitoJUnitRunner.class)
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class MetricsTrackerFacadeTest {

    private static final String METRIC_LABEL = "test_label";

    private static MockedStatic<MetricsTrackerHandler> mockedMetricsTrackerHandler;

    private static MockedStatic<ExtensionLoader> mockedExtensionLoader;

    private final MetricsTrackerFacade metricsTrackerFacade = MetricsTrackerFacade.getInstance();

    @Mock
    private MetricsTrackerHandler metricsTrackerHandler;

    @Mock
    private SummaryMetricsTrackerDelegate summaryMetricsTrackerDelegate;

    @Mock
    private HistogramMetricsTrackerDelegate delegate;

    @BeforeClass
    public static void setUp() {
        mockedMetricsTrackerHandler = mockStatic(MetricsTrackerHandler.class);
        mockedExtensionLoader = mockStatic(ExtensionLoader.class);
    }

    @Before
    public void before() {
        mockedMetricsTrackerHandler.when(MetricsTrackerHandler::getInstance).thenReturn(metricsTrackerHandler);

        MetricsConfig metricsConfig = new MetricsConfig("prometheus", "localhost", 1234, Boolean.FALSE,
                10, "{\"jmxUrl\":\"service:jmx:rmi:///jndi/rmi://127.0.0.1:1234/jmxrmi\"}", new Properties());

        ExtensionLoader extensionLoader = mock(ExtensionLoader.class);
        MetricsTrackerManager metricsTrackerManager = mock(MetricsTrackerManager.class);
        when(extensionLoader.getJoin(eq(metricsConfig.getMetricsName()))).thenReturn(metricsTrackerManager);
        mockedExtensionLoader.when(() -> ExtensionLoader.getExtensionLoader(eq(MetricsTrackerManager.class))).thenReturn(extensionLoader);
        metricsTrackerFacade.start(metricsConfig);
        assertTrue(metricsTrackerFacade.isStarted());
    }

    @After
    public void after() {
        metricsTrackerFacade.stop();
        assertFalse(metricsTrackerFacade.isStarted());
    }

    @Test
    public void testCounterInc() {
        metricsTrackerFacade.counterInc(METRIC_LABEL);
        verify(metricsTrackerHandler, times(1)).counterInc(eq(METRIC_LABEL));
    }

    @Test
    public void testGaugeInc() {
        metricsTrackerFacade.gaugeInc(METRIC_LABEL);
        verify(metricsTrackerHandler, times(1)).gaugeInc(eq(METRIC_LABEL));
    }

    @Test
    public void testGaugeDec() {
        metricsTrackerFacade.gaugeDec(METRIC_LABEL);
        verify(metricsTrackerHandler, times(1)).gaugeDec(eq(METRIC_LABEL));
    }

    @Test
    public void testSummaryStartTimer() {
        metricsTrackerFacade.summaryStartTimer(METRIC_LABEL);
        verify(metricsTrackerHandler, times(1)).summaryStartTimer(eq(METRIC_LABEL));
    }

    @Test
    public void testSummaryObserveDuration() {
        metricsTrackerFacade.summaryObserveDuration(summaryMetricsTrackerDelegate);
        verify(metricsTrackerHandler, times(1)).summaryObserveDuration(eq(summaryMetricsTrackerDelegate));
    }

    @Test
    public void testHistogramStartTimer() {
        metricsTrackerFacade.histogramStartTimer(METRIC_LABEL);
        verify(metricsTrackerHandler, times(1)).histogramStartTimer(eq(METRIC_LABEL));
    }

    @Test
    public void testHistogramObserveDuration() {
        metricsTrackerFacade.histogramObserveDuration(delegate);
        verify(metricsTrackerHandler, times(1)).histogramObserveDuration(eq(delegate));
    }
}
