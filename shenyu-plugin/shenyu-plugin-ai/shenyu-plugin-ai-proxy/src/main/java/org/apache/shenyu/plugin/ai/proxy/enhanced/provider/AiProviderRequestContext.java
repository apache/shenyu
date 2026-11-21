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

package org.apache.shenyu.plugin.ai.proxy.enhanced.provider;

import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;

import java.util.Objects;

/**
 * Immutable context for a provider-and-protocol request transformation.
 *
 * @param provider provider identifier
 * @param targetProtocol native protocol selected for the provider
 * @param upstreamModel authoritative mapped model, or null
 * @param request normalized provider request
 * @param config resolved request configuration
 */
public record AiProviderRequestContext(String provider, String targetProtocol, String upstreamModel,
                                       ShenyuAiRequest request, AiCommonConfig config) {

    /**
     * Validate required transformation context.
     */
    public AiProviderRequestContext {
        Objects.requireNonNull(provider, "AI provider identifier is required");
        Objects.requireNonNull(targetProtocol, "AI target protocol is required");
        Objects.requireNonNull(request, "AI normalized request is required");
        Objects.requireNonNull(config, "AI request configuration is required");
    }
}
