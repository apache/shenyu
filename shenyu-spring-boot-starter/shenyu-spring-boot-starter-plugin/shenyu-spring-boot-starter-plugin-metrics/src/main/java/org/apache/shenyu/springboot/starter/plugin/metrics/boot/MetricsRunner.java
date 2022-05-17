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

package org.apache.shenyu.springboot.starter.plugin.metrics.boot;

import com.google.common.base.Preconditions;
import org.apache.shenyu.common.config.ShenyuConfig.MetricsConfig;
import org.apache.shenyu.plugin.metrics.reporter.MetricsReporter;
import org.apache.shenyu.plugin.metrics.spi.MetricsRegister;
import org.apache.shenyu.plugin.metrics.spi.MetricsService;
import org.apache.shenyu.spi.ExtensionLoader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.boot.CommandLineRunner;

import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * The type Metrics runner.
 */
public class MetricsRunner implements CommandLineRunner, DisposableBean {
    
    private static final Logger LOG = LoggerFactory.getLogger(MetricsRunner.class);
    
    private final AtomicBoolean isStarted = new AtomicBoolean(false);
    
    private final MetricsConfig config;
    
    private MetricsService metricsService;
    
    /**
     * Instantiates a new Metrics runner.
     *
     * @param config the config
     */
    public MetricsRunner(final MetricsConfig config) {
        this.config = config;
    }
    
    @Override
    public void run(final String... args) {
        if (this.isStarted.compareAndSet(false, true)) {
            metricsService = ExtensionLoader.getExtensionLoader(MetricsService.class).getJoin(config.getName());
            Preconditions.checkNotNull(metricsService,
                    "Can not find metrics service with metrics name : %s in metrics configuration.", config.getName());
            MetricsRegister metricsRegister = ExtensionLoader.getExtensionLoader(MetricsRegister.class).getJoin(config.getName());
            Preconditions.checkNotNull(metricsRegister,
                    "Can not find metrics register with metrics name : %s in metrics configuration.", config.getName());
            MetricsReporter.register(metricsRegister);
            metricsService.start(config, metricsRegister);
        } else {
            LOG.info("metrics service has started!");
        }
    }
    
    @Override
    public void destroy() {
        this.isStarted.compareAndSet(true, false);
        Optional.ofNullable(metricsService).ifPresent(MetricsService::stop);
    }
}
