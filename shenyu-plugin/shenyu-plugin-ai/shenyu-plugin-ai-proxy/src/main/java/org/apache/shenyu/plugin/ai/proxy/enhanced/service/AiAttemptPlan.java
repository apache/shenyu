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

import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.event.AiProxyAttemptEvent;

import java.util.Optional;

/**
 * Stable orchestration facts shared while one primary or fallback attempt sequence is evaluated.
 *
 * @param execution logical request execution state
 * @param prepared provider-ready exchange for this sequence
 * @param fallback optional fallback configuration and its replay capability
 * @param retryBudget configured and remaining retry allowance
 * @param phase primary or fallback execution phase
 */
record AiAttemptPlan(
        AiExecutionState execution,
        AiPreparedExchange prepared,
        Optional<FallbackPlan> fallback,
        RetryBudget retryBudget,
        AiProxyAttemptEvent.Phase phase) {

    AiAttemptPlan nextRetry() {
        return new AiAttemptPlan(execution, prepared, fallback, retryBudget.consume(), phase);
    }

    AiAttemptPlan forFallback(final AiPreparedExchange fallbackExchange) {
        return new AiAttemptPlan(execution, fallbackExchange, Optional.empty(),
                new RetryBudget(0, 0), AiProxyAttemptEvent.Phase.FALLBACK);
    }

    /**
     * Fallback configuration coupled with the provider capability used by replay policy.
     *
     * @param config fallback configuration
     * @param providerDeclaredReplaySafe whether the fallback provider declares replay safety
     */
    record FallbackPlan(AiCommonConfig config, boolean providerDeclaredReplaySafe) {
    }

    /**
     * Retry allowance for the current execution phase.
     *
     * @param configured originally configured retry count
     * @param remaining retries remaining before the current attempt
     */
    record RetryBudget(int configured, int remaining) {

        int retryIndex() {
            return configured - remaining;
        }

        RetryBudget consume() {
            return new RetryBudget(configured, remaining - 1);
        }
    }
}
