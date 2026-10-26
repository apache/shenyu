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

package org.apache.shenyu.plugin.metrics.spi;

import org.junit.jupiter.api.Test;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

class MetricsRegisterCompatibilityTest {

    @Test
    void existingProvidersRetainRegistrationWithoutTruncatingFractionalObservations() {
        MetricsRegister legacy = mock(MetricsRegister.class, CALLS_REAL_METHODS);
        String[] labels = {"selector", "rule"};
        legacy.registerHistogram("decision_seconds", labels, "decision duration", new double[]{0.0001, 0.001});
        verify(legacy).registerHistogram("decision_seconds", labels, "decision duration");
        legacy.observe("decision_seconds", new String[]{"selector-a", "rule-a"}, 0.00008);
        verify(legacy, never()).recordTime(anyString(), any(), anyLong());
    }
}
