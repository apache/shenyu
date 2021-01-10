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
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.metrics.api.HistogramMetricsTrackerDelegate;
import org.dromara.soul.metrics.api.SummaryMetricsTrackerDelegate;
import org.dromara.soul.metrics.config.MetricsConfig;
import org.dromara.soul.metrics.facade.handler.MetricsTrackerHandler;
import org.dromara.soul.metrics.spi.MetricsTrackerManager;
import org.dromara.soul.spi.ExtensionLoader;

/**
 * Metrics tracker facade.
 */
@Slf4j
public final class MetricsTrackerFacade {
    
    @Getter
    private MetricsTrackerManager metricsTrackerManager;
    
    private final AtomicBoolean isStarted = new AtomicBoolean(false);
    
    private MetricsTrackerFacade() {
    }
    
    /**
     * Get metrics tracker facade of lazy load singleton.
     *
     * @return metrics tracker facade
     */
    public static MetricsTrackerFacade getInstance() {
        return MetricsTrackerFacadeHolder.INSTANCE;
    }
    
    /**
     * Init for metrics tracker manager.
     *
     * @param metricsConfig metrics config
     */
    public void start(final MetricsConfig metricsConfig) {
        if (this.isStarted.compareAndSet(false, true)) {
            metricsTrackerManager = ExtensionLoader.getExtensionLoader(MetricsTrackerManager.class).getJoin(metricsConfig.getMetricsName());
            Preconditions.checkNotNull(metricsTrackerManager,
                    "Can not find metrics tracker manager with metrics name : %s in metrics configuration.", metricsConfig.getMetricsName());
            metricsTrackerManager.start(metricsConfig);
            Integer threadCount = Optional.ofNullable(metricsConfig.getThreadCount()).orElse(Runtime.getRuntime().availableProcessors());
            MetricsTrackerHandler.getInstance().init(metricsConfig.getAsync(), threadCount, metricsTrackerManager);
        } else {
            log.info("metrics tracker has started !");
        }
    }
    
    /**
     * Increment of counter metrics tracker.
     *
     * @param metricsLabel metrics label
     * @param labelValues  label values
     */
    public void counterInc(final String metricsLabel, final String... labelValues) {
        if (isStarted()) {
            MetricsTrackerHandler.getInstance().counterInc(metricsLabel, labelValues);
        }
    }
    
    /**
     * Increment of gauge metrics tracker.
     *
     * @param metricsLabel metrics label
     * @param labelValues  label values
     */
    public void gaugeInc(final String metricsLabel, final String... labelValues) {
        if (isStarted()) {
            MetricsTrackerHandler.getInstance().gaugeInc(metricsLabel, labelValues);
        }
    }
    
    /**
     * Decrement of gauge metrics tracker.
     *
     * @param metricsLabel metrics label
     * @param labelValues  label values
     */
    public void gaugeDec(final String metricsLabel, final String... labelValues) {
        if (isStarted()) {
            MetricsTrackerHandler.getInstance().gaugeDec(metricsLabel, labelValues);
        }
    }
    
    /**
     * Start timer of histogram metrics tracker.
     *
     * @param metricsLabel metrics label
     * @param labelValues  label values
     * @return histogram metrics tracker delegate
     */
    public Optional<HistogramMetricsTrackerDelegate> histogramStartTimer(final String metricsLabel, final String... labelValues) {
        if (!isStarted()) {
            return Optional.empty();
        }
        return MetricsTrackerHandler.getInstance().histogramStartTimer(metricsLabel, labelValues);
    }
    
    /**
     * Observe amount of time since start time with histogram metrics tracker.
     *
     * @param delegate histogram metrics tracker delegate
     */
    public void histogramObserveDuration(final HistogramMetricsTrackerDelegate delegate) {
        if (isStarted()) {
            MetricsTrackerHandler.getInstance().histogramObserveDuration(delegate);
        }
    }
    
    /**
     * Start timer of summary metrics tracker.
     *
     * @param metricsLabel metrics label
     * @param labelValues  label values
     * @return summary metrics tracker delegate
     */
    public Optional<SummaryMetricsTrackerDelegate> summaryStartTimer(final String metricsLabel, final String... labelValues) {
        if (!isStarted()) {
            return Optional.empty();
        }
        return MetricsTrackerHandler.getInstance().summaryStartTimer(metricsLabel, labelValues);
    }
    
    /**
     * Observe amount of time since start time with summary metrics tracker.
     *
     * @param delegate summary metrics tracker delegate
     */
    public void summaryObserveDuration(final SummaryMetricsTrackerDelegate delegate) {
        if (isStarted()) {
            MetricsTrackerHandler.getInstance().summaryObserveDuration(delegate);
        }
    }
    
    /**
     * Stop.
     */
    public void stop() {
        this.isStarted.compareAndSet(true, false);
        if (null != metricsTrackerManager) {
            metricsTrackerManager.stop();
        }
        MetricsTrackerHandler.getInstance().close();
    }
    
    /**
     * Check if start or not.
     *
     * @return true is stared, otherwise not.
     */
    public boolean isStarted() {
        return isStarted.get();
    }
    
    /**
     * Metrics tracker facade holder.
     */
    private static class MetricsTrackerFacadeHolder {
        private static final MetricsTrackerFacade INSTANCE = new MetricsTrackerFacade();
    }
}

