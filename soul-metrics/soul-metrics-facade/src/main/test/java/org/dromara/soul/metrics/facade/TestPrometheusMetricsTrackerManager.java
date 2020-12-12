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

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.metrics.api.MetricsTrackerFactory;
import org.dromara.soul.metrics.config.MetricsConfig;
import org.dromara.soul.metrics.prometheus.PrometheusMetricsTrackerFactory;
import org.dromara.soul.metrics.spi.MetricsTrackerManager;
import org.dromara.soul.spi.Join;

/**
 * Test Prometheus metrics tracker manager.
 *
 * @author nuo-promise
 **/
@Getter
@Slf4j
@Join
public class TestPrometheusMetricsTrackerManager implements MetricsTrackerManager {

    private final MetricsTrackerFactory metricsTrackerFactory = new PrometheusMetricsTrackerFactory();

    @Override
    public void start(MetricsConfig metricsConfig) {
        System.out.println("TestPrometheusMetricsTrackerManger start : " + metricsConfig.toString());
    }

    @Override
    public void stop() {
        System.out.println("TestPrometheusMetricsTrackerManger stop");
    }
}
