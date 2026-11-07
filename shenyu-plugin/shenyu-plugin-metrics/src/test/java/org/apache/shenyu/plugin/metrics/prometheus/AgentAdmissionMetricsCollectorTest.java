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

package org.apache.shenyu.plugin.metrics.prometheus;

import org.apache.shenyu.common.metrics.AgentAdmissionMetrics;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

class AgentAdmissionMetricsCollectorTest {

    @Test
    void shouldAggregateFixedScopesWithoutIdentityLabels() {
        AgentAdmissionMetrics.Snapshot snapshot = new AgentAdmissionMetrics.Snapshot(
                AgentAdmissionMetrics.Scope.REMOTE, 2, 3, 4, 5, 6);
        try (AgentAdmissionMetrics ignored = AgentAdmissionMetrics.register(() -> List.of(snapshot))) {
            var families = new AgentAdmissionMetricsCollector().collect();
            List<String> names = families.stream()
                    .map(family -> family.samples.stream().map(sample -> sample.name).toList()).flatMap(List::stream).toList();
            assertEquals(15, names.size());
            assertEquals(5, names.stream().filter(name -> name.startsWith("shenyu_agent_admission_")).distinct().count());
            assertEquals(6.0, families.stream().flatMap(family -> family.samples.stream())
                    .filter(sample -> sample.name.equals("shenyu_agent_admission_rejections_total") && sample.labelValues.equals(List.of("remote")))
                    .findFirst().orElseThrow().value);
        }
    }
}
