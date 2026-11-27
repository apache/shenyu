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

package org.apache.shenyu.plugin.metrics.reporter;

import org.apache.shenyu.common.metrics.PluginObservationEvent;
import org.apache.shenyu.common.metrics.PluginObservationSink;

import java.util.Locale;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

/** Reuses the existing metrics registry; correlation/configuration IDs never become labels. */
public final class PluginLifecycleMetricsSink implements PluginObservationSink {

    public static final String EVENTS = "shenyu_plugin_observation_total";

    public static final String INCLUSIVE = "shenyu_plugin_publisher_inclusive_millis";

    public static final String MATCHES = "shenyu_plugin_match_decisions_total";

    @Override
    public void accept(final PluginObservationEvent event) {
        MetricsReporter.counterIncrement(EVENTS,
                new String[]{event.plugin(), event.kind().name().toLowerCase(Locale.ROOT), event.outcome().name().toLowerCase(Locale.ROOT)}, 1);
        if (event.kind() == PluginObservationEvent.Kind.TERMINAL) {
            MetricsReporter.recordTime(INCLUSIVE, new String[]{event.plugin(), event.outcome().name().toLowerCase(Locale.ROOT)},
                    TimeUnit.NANOSECONDS.toMillis(event.elapsedNanos()));
        }
        if (event.kind() == PluginObservationEvent.Kind.MATCHING && Objects.nonNull(event.matching())) {
            var matching = event.matching();
            MetricsReporter.counterIncrement(MATCHES, new String[]{event.plugin(), matching.decision().name(),
                matching.selectorCache().name(), matching.ruleCache().name()}, 1);
        }
    }
}
