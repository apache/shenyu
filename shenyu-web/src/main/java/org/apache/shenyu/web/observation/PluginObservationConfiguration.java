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

package org.apache.shenyu.web.observation;

import org.apache.shenyu.common.metrics.PluginObservationSink;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/** Startup-only opt-in; no exporter or worker exists in the default deployment. */
@Configuration
@ConditionalOnProperty(name = "shenyu.plugin-observation.enabled", havingValue = "true")
public class PluginObservationConfiguration {

    /**
     * Create one bounded asynchronous owner, independent of a telemetry backend.
     * @param sinks optional existing backend adapter
     * @param capacity queued events
     * @param sampleEvery sampling interval
     * @param records per-root record budget
     * @param bytes conservative per-root serialized-byte allowance
     * @return lifecycle-owned observer
     */
    @Bean(destroyMethod = "close")
    public PluginObservationManager pluginObservationManager(final ObjectProvider<PluginObservationSink> sinks,
            @Value("${shenyu.plugin-observation.queue-capacity:1024}") final int capacity,
            @Value("${shenyu.plugin-observation.sample-every:100}") final int sampleEvery,
            @Value("${shenyu.plugin-observation.max-records:128}") final int records,
            @Value("${shenyu.plugin-observation.max-bytes:262144}") final int bytes) {
        return new PluginObservationManager(capacity, sampleEvery, records, bytes, sinks.getIfAvailable(() -> PluginObservationSink.NOOP));
    }
}
