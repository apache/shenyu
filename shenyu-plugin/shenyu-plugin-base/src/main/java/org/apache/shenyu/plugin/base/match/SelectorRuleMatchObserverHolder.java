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

package org.apache.shenyu.plugin.base.match;

import java.util.Objects;

/**
 * Opt-in static registration point for the {@link SelectorRuleMatchObserver}.
 *
 * <p>The holder follows the same pattern as the diagnostics and metrics registration
 * points: exactly one observer is registered at startup by whichever consumer (metrics,
 * diagnostics, agent) wants the decision stream. With no observer registered the matching
 * path performs a single null read and never allocates an observation, so instrumentation
 * enabled/disabled is behaviourally and cost-invisible at the request path.</p>
 */
public final class SelectorRuleMatchObserverHolder {

    private static volatile SelectorRuleMatchObserver observer;

    private SelectorRuleMatchObserverHolder() {
    }

    /**
     * Register the process-wide observer, replacing any previous one.
     *
     * @param newObserver the observer to register, must not be null
     */
    public static void register(final SelectorRuleMatchObserver newObserver) {
        observer = Objects.requireNonNull(newObserver, "observer");
    }

    /**
     * Remove the current observer; matching returns to the no-op fast path.
     */
    public static void unregister() {
        observer = null;
    }

    /**
     * The currently registered observer, or null when instrumentation is off.
     *
     * @return the observer or null
     */
    public static SelectorRuleMatchObserver get() {
        return observer;
    }

    /**
     * Whether an observer is registered.
     *
     * @return true when decisions are being reported
     */
    public static boolean isInstrumented() {
        return Objects.nonNull(observer);
    }
}
