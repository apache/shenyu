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

package org.apache.shenyu.agent.plugin.metrics.prometheus.collector;

import io.prometheus.client.Collector;
import io.prometheus.client.GaugeMetricFamily;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Build info collector.
 */
public final class BuildInfoCollector extends Collector {
    
    private static final Logger LOG = LoggerFactory.getLogger(BuildInfoCollector.class);
    
    private static final String CLASS_NAME = "org.apache.shenyu.web.handler.ShenyuWebHandler";
    
    @Override
    public List<MetricFamilySamples> collect() {
        List<String> labels = new ArrayList<>();
        labels.add("version");
        labels.add("name");
        GaugeMetricFamily gaugeMetricFamily = new GaugeMetricFamily("build_info", "build information", labels);
        try {
            Package proxyPkg = Class.forName(CLASS_NAME).getPackage();
            final String proxyVersion = proxyPkg.getImplementationVersion();
            final String proxyName = proxyPkg.getImplementationTitle();
            gaugeMetricFamily.addMetric(Arrays.asList(null != proxyVersion ? proxyVersion : "unknown", null != proxyName ? proxyName : "unknown"), 1L);
        } catch (ClassNotFoundException ex) {
            LOG.error("No proxy class find :{}", CLASS_NAME);
        }
        return Collections.singletonList(gaugeMetricFamily);
    }
}
