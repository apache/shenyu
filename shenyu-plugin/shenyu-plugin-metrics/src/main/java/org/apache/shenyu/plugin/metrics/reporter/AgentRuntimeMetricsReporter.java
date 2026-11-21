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

import org.apache.shenyu.common.metrics.AgentRuntimeMetrics;
import org.apache.shenyu.common.metrics.AiAttemptEvent;
import org.apache.shenyu.plugin.metrics.constant.LabelNames;
import org.apache.shenyu.plugin.metrics.spi.MetricsRegister;

import java.util.Locale;

/** Binds each request to the registry it started with, not a later reloaded registry. */
final class AgentRuntimeMetricsReporter implements AgentRuntimeMetrics {

    private final MetricsRegister register;

    private boolean active = true;

    AgentRuntimeMetricsReporter(final MetricsRegister register) {
        this.register = register;
    }

    @Override
    public synchronized void started(final Traffic traffic) {
        if (active) {
            register.gaugeIncrement(LabelNames.AGENT_REQUESTS_INFLIGHT, new String[]{label(traffic)});
        }
    }

    @Override
    public synchronized void finished(final Traffic traffic, final Outcome outcome, final Reason reason, final long millis) {
        if (!active) {
            return;
        }
        String[] labels = {label(traffic), label(outcome), label(reason)};
        try {
            register.counterIncrement(LabelNames.AGENT_REQUESTS_TOTAL, labels, 1);
            register.recordTime(LabelNames.AGENT_REQUEST_LATENCY, new String[]{label(traffic), label(outcome)}, millis);
        } finally {
            register.gaugeDecrement(LabelNames.AGENT_REQUESTS_INFLIGHT, new String[]{label(traffic)});
        }
    }

    @Override
    public synchronized void attempt(final AiAttemptEvent.Kind kind, final Outcome outcome, final long millis) {
        if (!active) {
            return;
        }
        String[] labels = {label(kind), label(outcome)};
        register.counterIncrement(LabelNames.AGENT_AI_ATTEMPTS_TOTAL, labels, 1);
        register.recordTime(LabelNames.AGENT_AI_ATTEMPT_LATENCY, labels, millis);
    }

    @Override
    public synchronized void catalog(final boolean available) {
        if (active) {
            register.counterIncrement(LabelNames.AGENT_CATALOG_OBSERVATIONS_TOTAL, new String[]{available ? "available" : "unavailable"}, 1);
        }
    }

    synchronized void deactivate() {
        active = false;
    }

    @Override
    public synchronized void authenticationRejected(final AuthenticationSource source, final AuthenticationReason reason) {
        if (active) {
            register.counterIncrement(LabelNames.AUTHENTICATION_REJECTIONS_TOTAL, new String[]{label(source), label(reason)}, 1);
        }
    }

    private static String label(final Enum<?> value) {
        return value.name().toLowerCase(Locale.ROOT);
    }
}
