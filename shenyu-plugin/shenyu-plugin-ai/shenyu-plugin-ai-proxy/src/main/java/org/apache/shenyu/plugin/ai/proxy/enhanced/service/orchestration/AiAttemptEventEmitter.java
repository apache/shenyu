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

package org.apache.shenyu.plugin.ai.proxy.enhanced.service.orchestration;

import org.apache.shenyu.plugin.ai.proxy.enhanced.service.event.AiProxyAttemptEvent;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.event.AiProxyAttemptListener;

import java.time.Duration;
import java.util.Objects;

/**
 * Emits bounded attempt lifecycle events without allowing observers to change request outcomes.
 */
public final class AiAttemptEventEmitter {

    private final AiProxyAttemptListener listener;

    public AiAttemptEventEmitter(final AiProxyAttemptListener listener) {
        this.listener = Objects.requireNonNull(listener, "listener must not be null");
    }

    public void emit(final String invocationId, final int attemptIndex, final AiProxyAttemptEvent.Phase phase,
              final AiProxyAttemptEvent.Reason reason, final Duration retryAfter) {
        try {
            listener.onAttempt(new AiProxyAttemptEvent(invocationId, attemptIndex, phase, reason, retryAfter));
        } catch (RuntimeException ignored) {
            // Observation must never change the AI request outcome.
        }
    }
}
