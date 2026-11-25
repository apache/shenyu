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

package org.apache.shenyu.plugin.ai.proxy.enhanced.service.structured;

import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiStructuredOutputPolicy;

import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Structured output policy and bounded regeneration budget for one logical request.
 */
public final class AiStructuredOutputSession {

    private final AiStructuredOutputPolicy policy;

    private final AtomicInteger repairsRemaining;

    public AiStructuredOutputSession(final AiStructuredOutputPolicy policy) {
        this.policy = Objects.requireNonNull(policy, "policy must not be null");
        repairsRemaining = new AtomicInteger(policy.repairMaxAttempts());
    }

    public AiStructuredOutputPolicy policy() {
        return policy;
    }

    public int repairsRemaining() {
        return repairsRemaining.get();
    }

    public int consumeRepair() {
        return repairsRemaining.decrementAndGet();
    }
}
