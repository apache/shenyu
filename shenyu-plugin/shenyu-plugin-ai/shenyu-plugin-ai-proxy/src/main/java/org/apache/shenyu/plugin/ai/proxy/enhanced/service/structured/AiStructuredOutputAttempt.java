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

import java.util.Objects;

/**
 * Domain facts required to evaluate one structured-output response.
 *
 * @param session request-scoped policy and repair budget
 * @param originalRequestBody original client request used for repair generation
 * @param phase structured-output evaluation phase
 */
public record AiStructuredOutputAttempt(
        AiStructuredOutputSession session,
        String originalRequestBody,
        Phase phase) {

    public AiStructuredOutputAttempt {
        Objects.requireNonNull(session, "session must not be null");
        Objects.requireNonNull(originalRequestBody, "originalRequestBody must not be null");
        Objects.requireNonNull(phase, "phase must not be null");
    }

    /**
     * Structured-output generation phase.
     */
    public enum Phase {
        INITIAL,
        REPAIR
    }
}
