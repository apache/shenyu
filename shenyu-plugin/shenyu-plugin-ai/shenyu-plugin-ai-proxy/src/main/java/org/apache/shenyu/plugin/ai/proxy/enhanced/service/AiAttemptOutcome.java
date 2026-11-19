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

package org.apache.shenyu.plugin.ai.proxy.enhanced.service;

import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Explicit result of one current upstream attempt. A transition is applied only
 * after the current attempt's error boundary, so a later attempt cannot be
 * recovered by an earlier attempt.
 */
sealed interface AiAttemptOutcome {

    /**
     * Successfully completed current attempt.
     */
    record Completed() implements AiAttemptOutcome {
    }

    /**
     * Policy transition produced by current attempt.
     *
     * @param decision retry, fallback or error propagation decision
     * @param failure classified failure that caused the transition
     * @param responseHeaders upstream response headers, or an empty map
     */
    record Transition(AiProxyRetryPolicy.Decision decision, Throwable failure,
                      Map<String, List<String>> responseHeaders) implements AiAttemptOutcome {

        public Transition {
            Objects.requireNonNull(decision, "decision must not be null");
            Objects.requireNonNull(failure, "failure must not be null");
            Objects.requireNonNull(responseHeaders, "responseHeaders must not be null");
            if (decision == AiProxyRetryPolicy.Decision.FORWARD_RESPONSE) {
                throw new IllegalArgumentException("forward responses must be consumed by the current attempt");
            }
        }
    }
}
