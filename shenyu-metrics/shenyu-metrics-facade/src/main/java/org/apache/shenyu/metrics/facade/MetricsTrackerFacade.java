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

package org.apache.shenyu.metrics.facade;

import com.google.common.base.Preconditions;
import org.apache.shenyu.metrics.config.MetricsConfig;
import org.apache.shenyu.metrics.spi.MetricsBootService;
import org.apache.shenyu.metrics.spi.MetricsRegister;
import org.apache.shenyu.spi.ExtensionLoader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Metrics tracker facade.
 */
public final class MetricsTrackerFacade implements AutoCloseable {

    private static final Logger LOG = LoggerFactory.getLogger(MetricsTrackerFacade.class);

    private MetricsBootService metricsBootService;
    
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
            metricsBootService = ExtensionLoader.getExtensionLoader(MetricsBootService.class).getJoin(metricsConfig.getMetricsName());
            Preconditions.checkNotNull(metricsBootService,
                    "Can not find metrics tracker manager with metrics name : %s in metrics configuration.", metricsConfig.getMetricsName());
            MetricsRegister metricsRegister = ExtensionLoader.getExtensionLoader(MetricsRegister.class).getJoin(metricsConfig.getMetricsName());
            Preconditions.checkNotNull(metricsRegister,
                    "Can not find metrics register with metrics name : %s in metrics configuration.", metricsConfig.getMetricsName());
            metricsBootService.start(metricsConfig, metricsRegister);
        } else {
            LOG.info("metrics tracker has started !");
        }
    }
    
    /**
     * Stop.
     */
    public void stop() {
        this.isStarted.compareAndSet(true, false);
        if (null != metricsBootService) {
            metricsBootService.stop();
        }
    }
    
    /**
     * Check if start or not.
     *
     * @return true is stared, otherwise not.
     */
    public boolean isStarted() {
        return isStarted.get();
    }
    
    @Override
    public void close() {
        stop();
    }
    
    /**
     * Metrics tracker facade holder.
     */
    private static class MetricsTrackerFacadeHolder {
        
        private static final MetricsTrackerFacade INSTANCE = new MetricsTrackerFacade();
    }
}

