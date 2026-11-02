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
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import static org.mockito.Mockito.mockStatic;

class PluginLifecycleMetricsSinkTest {

    @Test
    void exportsInclusiveTimingWithoutCorrelationOrConfigurationIdsAsLabels() {
        PluginObservationEvent event = new PluginObservationEvent(999, 123, 456, "Fixture", "external.Plugin", "SECRET_TRACE",
                PluginObservationEvent.Kind.TERMINAL, PluginObservationEvent.Outcome.COMPLETE, 42000000, null);
        try (MockedStatic<MetricsReporter> metrics = mockStatic(MetricsReporter.class)) {
            new PluginLifecycleMetricsSink().accept(event);
            metrics.verify(() -> MetricsReporter.counterIncrement(PluginLifecycleMetricsSink.EVENTS,
                    new String[]{"Fixture", "terminal", "complete"}, 1));
            metrics.verify(() -> MetricsReporter.recordTime(PluginLifecycleMetricsSink.INCLUSIVE, new String[]{"Fixture", "complete"}, 42));
            metrics.verifyNoMoreInteractions();
        }
    }
}
