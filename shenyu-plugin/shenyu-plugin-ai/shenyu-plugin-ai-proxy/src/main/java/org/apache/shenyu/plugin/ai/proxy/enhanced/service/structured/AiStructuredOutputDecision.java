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

import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiContentCompliance;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;

import java.util.Objects;

/**
 * Domain decision produced by structured-output evaluation.
 */
public sealed interface AiStructuredOutputDecision {

    /**
     * Response accepted for downstream consumption.
     *
     * @param response materialized response with replayable body
     * @param compliance classified content compliance
     */
    record Accepted(AiUpstreamResponse response, AiContentCompliance compliance)
            implements AiStructuredOutputDecision {

        public Accepted {
            Objects.requireNonNull(response, "response must not be null");
            Objects.requireNonNull(compliance, "compliance must not be null");
        }
    }

    /**
     * Another generation is required with the supplied repair request.
     *
     * @param requestBody complete repair request body
     * @param compliance classified content compliance
     */
    record RepairRequired(String requestBody, AiContentCompliance compliance)
            implements AiStructuredOutputDecision {

        public RepairRequired {
            Objects.requireNonNull(requestBody, "requestBody must not be null");
            Objects.requireNonNull(compliance, "compliance must not be null");
        }
    }

    /**
     * Content rejected without another repair attempt.
     *
     * @param failure classified content failure
     * @param compliance classified content compliance
     */
    record Rejected(AiProxyException failure, AiContentCompliance compliance)
            implements AiStructuredOutputDecision {

        public Rejected {
            Objects.requireNonNull(failure, "failure must not be null");
            Objects.requireNonNull(compliance, "compliance must not be null");
        }
    }
}
