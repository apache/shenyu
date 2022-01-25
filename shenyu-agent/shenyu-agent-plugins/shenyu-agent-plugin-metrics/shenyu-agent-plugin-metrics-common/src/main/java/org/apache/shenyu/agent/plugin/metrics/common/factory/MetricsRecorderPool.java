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

package org.apache.shenyu.agent.plugin.metrics.common.factory;

import org.apache.shenyu.agent.core.loader.SPILoader;
import org.apache.shenyu.agent.core.locator.ShenyuAgentLocator;
import org.apache.shenyu.agent.core.utils.ShenyuAgentConfigUtils;
import org.apache.shenyu.agent.core.yaml.ShenyuYamlEngine;
import org.apache.shenyu.agent.plugin.metrics.api.MetricsRecorder;
import org.apache.shenyu.agent.plugin.metrics.api.MetricsRecorderRegistry;
import org.apache.shenyu.agent.plugin.metrics.api.config.Metrics;
import org.apache.shenyu.agent.plugin.metrics.api.config.Metrics.Metric;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The type Metrics recorder pool.
 */
public class MetricsRecorderPool {
    
    private static final Logger LOG = LoggerFactory.getLogger(MetricsRecorderPool.class);
    
    private static final ConcurrentHashMap<String, Map<String, MetricsRecorder>> RECORDER_MAP = new ConcurrentHashMap<>();
    
    static {
        init();
    }
    
    private static void init() {
        Metrics metrics = null;
        try {
            metrics = ShenyuYamlEngine.unmarshal(ShenyuAgentLocator.locatorConf("metrics-meta.yaml"), Metrics.class);
        } catch (IOException e) {
            LOG.error("Exception loader metrics meta data config is", e);
        }
        if (Objects.nonNull(metrics) && !metrics.getMetrics().isEmpty()) {
            List<Metric> metricList = metrics.getMetrics();
            Collection<MetricsRecorderRegistry> registryList = SPILoader.loadList(MetricsRecorderRegistry.class);
            Set<String> supports = ShenyuAgentConfigUtils.getSupports();
            for (Metric metric : metricList) {
                for (MetricsRecorderRegistry registry : registryList) {
                    String name = registry.named();
                    if (!supports.contains(name)) {
                        break;
                    }
                    Optional<MetricsRecorder> metricsRecorder = registry.register(metric);
                    metricsRecorder.ifPresent(mr -> {
                        if (RECORDER_MAP.containsKey(name)) {
                            Map<String, MetricsRecorder> recorderMap = RECORDER_MAP.get(name);
                            recorderMap.put(metric.getName(), mr);
                        } else {
                            Map<String, MetricsRecorder> recorderMap = new HashMap<>();
                            recorderMap.put(metric.getName(), mr);
                            RECORDER_MAP.put(name, recorderMap);
                        }
                    });
                }
            }
        }
    }
    
    /**
     * Get optional.
     *
     * @param metricsName the metrics name
     * @param registryName the registry name
     * @return the optional
     */
    public static Optional<MetricsRecorder> get(final String metricsName, final String registryName) {
        return Optional.ofNullable(RECORDER_MAP.get(registryName).get(metricsName));
    }
}
