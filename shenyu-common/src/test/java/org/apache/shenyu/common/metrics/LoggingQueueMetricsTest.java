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

package org.apache.shenyu.common.metrics;

import org.apache.shenyu.common.metrics.LoggingQueueMetrics.Mode;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Verify replacement collectors cannot be removed by an older instance closing.
 */
class LoggingQueueMetricsTest {

    @Test
    void unregisterOnlyRemovesTheOwningCollector() {
        final LoggingQueueMetrics previous = new LoggingQueueMetrics(getClass(), Mode.SINGLE, () -> 0, () -> 1);
        final LoggingQueueMetrics current = new LoggingQueueMetrics(getClass(), Mode.SINGLE, () -> 0, () -> 2);
        try {
            previous.register();
            current.register();
            previous.unregister();
            assertFalse(LoggingQueueMetrics.registered().contains(previous));
            assertTrue(LoggingQueueMetrics.registered().contains(current));
        } finally {
            previous.unregister();
            current.unregister();
        }
        assertFalse(LoggingQueueMetrics.registered().contains(current));
    }
}
