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

package org.apache.shenyu.plugin.ai.proxy.enhanced.conversion;

import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;

import java.util.Objects;

/**
 * Request-scoped protocol conversion binding a target request to its reverse response adapter.
 *
 * @param request converted target protocol request
 * @param responseAdapter reverse response adapter for this conversion
 */
public record AiProtocolConversion(
        ShenyuAiRequest request,
        AiResponseAdapter responseAdapter) {

    /**
     * Reject incomplete conversion results.
     */
    public AiProtocolConversion {
        Objects.requireNonNull(request, "Converted AI request is required");
        Objects.requireNonNull(responseAdapter, "AI response adapter is required");
    }

    /**
     * Create a conversion that leaves both request and response unchanged.
     *
     * @param request request to retain
     * @return identity protocol conversion
     */
    public static AiProtocolConversion identity(final ShenyuAiRequest request) {
        return new AiProtocolConversion(request, AiResponseAdapter.identity());
    }
}
