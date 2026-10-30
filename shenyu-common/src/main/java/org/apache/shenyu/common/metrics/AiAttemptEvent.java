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

import org.apache.shenyu.common.dto.AgentRequestContext;

/**
 * Body-free terminal fact for one upstream attempt. Status is assigned, not proof of delivery.
 * @param context immutable trusted request snapshot
 * @param attemptNumber one-based sequence within a logical request
 * @param kind primary, retry or fallback
 * @param provider selected provider identifier
 * @param protocol selected client protocol
 * @param streaming selected response mode
 * @param outcome terminal classification
 * @param httpStatus received upstream status, null before headers
 * @param elapsedMillis monotonic attempt duration
 * @param usage last explicit usage snapshot for this attempt
 */
public record AiAttemptEvent(AgentRequestContext context, int attemptNumber, Kind kind, String provider, String protocol, boolean streaming,
                             AiCallOutcome outcome, Integer httpStatus, long elapsedMillis, AiUsage usage) {

    /**
     * How this actual network attempt was selected.
     */
    public enum Kind {
        PRIMARY, RETRY, FALLBACK
    }
}
