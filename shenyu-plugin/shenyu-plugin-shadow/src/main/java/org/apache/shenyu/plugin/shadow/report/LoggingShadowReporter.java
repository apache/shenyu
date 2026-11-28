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

package org.apache.shenyu.plugin.shadow.report;

import org.apache.shenyu.plugin.shadow.compare.ShadowComparison;
import org.apache.shenyu.plugin.shadow.compare.ShadowObservation;
import org.apache.shenyu.plugin.shadow.compare.ShadowOutcome;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Writes bounded comparison summaries to the shadow report logger.
 *
 * <p>Only outcomes, rule identifiers, statuses, latencies and the redacted
 * comparison detail are written. Response bodies, header values and
 * credentials never reach the log.</p>
 */
public class LoggingShadowReporter implements ShadowReporter {

    private static final Logger LOG = LoggerFactory.getLogger(LoggingShadowReporter.class);

    private static final int NANOS_PER_MICRO = 1000;

    @Override
    public void report(final String ruleId, final ShadowComparison comparison, final ShadowObservation primary,
                       final ShadowObservation shadow) {
        if (ShadowOutcome.MATCH.equals(comparison.getOutcome())) {
            LOG.debug("shadow match rule={} primary_us={} shadow_us={}", ruleId,
                    primary.getDurationNanos() / NANOS_PER_MICRO, shadow.getDurationNanos() / NANOS_PER_MICRO);
            return;
        }
        LOG.info("shadow {} rule={} detail={} primary_status={} shadow_status={} primary_us={} shadow_us={}",
                comparison.getOutcome(), ruleId, comparison.getDetail(), primary.getStatus(), shadow.getStatus(),
                primary.getDurationNanos() / NANOS_PER_MICRO, shadow.getDurationNanos() / NANOS_PER_MICRO);
    }

    @Override
    public void failure(final String ruleId, final Throwable error) {
        LOG.warn("shadow call for rule={} failed: {}", ruleId, error.toString());
    }
}
