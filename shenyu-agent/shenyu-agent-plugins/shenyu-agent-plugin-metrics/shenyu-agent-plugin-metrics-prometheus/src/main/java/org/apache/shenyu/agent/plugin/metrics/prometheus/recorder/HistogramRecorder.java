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

package org.apache.shenyu.agent.plugin.metrics.prometheus.recorder;

import io.prometheus.client.Histogram;
import org.apache.shenyu.agent.plugin.metrics.api.MetricsRecorder;
import org.apache.shenyu.agent.plugin.metrics.api.config.Metrics.Metric;

import java.util.Objects;

/**
 * The type Histogram recorder.
 */
public final class HistogramRecorder implements MetricsRecorder {
    
    private final Histogram histogram;
    
    /**
     * Instantiates a new Histogram recorder.
     *
     * @param metric the metric
     */
    public HistogramRecorder(final Metric metric) {
        Histogram.Builder builder = Histogram.build()
                .name(metric.getName())
                .help(metric.getHelp());
        if (Objects.nonNull(metric.getLabelNames()) && !metric.getLabelNames().isEmpty()) {
            builder.labelNames(metric.getLabelNames().toArray(new String[0]));
        }
        this.histogram = builder.register();
    }
    
    @Override
    public void observe(final double value) {        
        histogram.observe(value);
    }
}
