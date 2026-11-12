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
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.PreparedAiRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;

import java.util.Objects;

/**
 * Contract for adapting a ShenYu AI request to a provider request.
 *
 * <p>The engine calls {@link #prepareRequest(ShenyuAiRequest, AiCommonConfig)}
 * after protocol routing and outgoing protocol preparation. Providers own native
 * endpoints, authentication and vendor adaptation. They construct request data;
 * the engine owns HTTP execution, retries, fallback and downstream writing.</p>
 *
 * <p>Use {@link OpenAi} for complete request preparation, or
 * {@link OpenAiCompatible} with {@link OpenAiProviderOptions} for a named profile
 * that reuses the existing request construction.</p>
 */
public interface AiProxyProvider {

    /**
     * Provider identifier.
     *
     * @return provider identifier
     */
    String name();

    /**
     * Get provider feature and operation declarations.
     * Native protocol bindings declare the supported protocols and their provider behavior.
     *
     * @return provider capabilities
     */
    AiProviderCapabilities capabilities();

    /**
     * Check whether the provider natively accepts an upstream protocol.
     *
     * @param protocol upstream protocol identifier
     * @return whether supported
     */
    default boolean supports(final String protocol) {
        return Objects.nonNull(protocol) && capabilities().protocols().contains(protocol);
    }

    /**
     * Prepare an upstream request and the response conversion bound to it.
     *
     * <p>This is the engine entry point. Override it when response adaptation
     * needs state from request preparation. Allocate that state per preparation
     * and return it through the response adapter, keeping body transformations
     * lazy and preserving upstream cancellation.</p>
     *
     * @param request decoded AI request
     * @param config provider configuration
     * @return prepared upstream request and request-scoped response adapter
     */
    PreparedAiRequest prepareRequest(ShenyuAiRequest request, AiCommonConfig config);

    /**
     * Check whether one native protocol is compatible with exact response caching.
     *
     * @param protocol native target protocol
     * @return whether exact response caching is supported
     */
    default boolean isExactResponseCacheCompatible(final String protocol) {
        return false;
    }
}
