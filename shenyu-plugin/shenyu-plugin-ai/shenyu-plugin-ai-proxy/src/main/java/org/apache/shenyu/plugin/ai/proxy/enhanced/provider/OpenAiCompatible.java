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
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;

import java.util.Objects;

/**
 * Named OpenAI-compatible profile sharing endpoint and request adaptation.
 */
public final class OpenAiCompatible implements AiProxyProvider {

    private final String name;

    private final OpenAi delegate;

    public OpenAiCompatible(final String name) {
        this(name, Objects.nonNull(name) && "DeepSeek".equalsIgnoreCase(name.trim().replace("-", "").replace("_", "")) ? OpenAiProviderOptions.deepSeek() : OpenAiProviderOptions.generic(null));
    }

    /**
     * Register another compatible service without changing Engine, Protocol or Transport.
     *
     * @param name provider identifier
     * @param options explicitly selected provider options
     */
    public OpenAiCompatible(final String name, final OpenAiProviderOptions options) {
        if (Objects.isNull(name) || name.isBlank()) {
            throw new IllegalArgumentException("AI provider name must not be blank");
        }
        this.name = name;
        this.delegate = OpenAi.withOptions(options);
    }

    @Override
    public AiProviderCapabilities capabilities() {
        return delegate.capabilities();
    }

    @Override
    public String name() {
        return name;
    }

    @Override
    public boolean supports(final String protocol) {
        return delegate.supports(protocol);
    }

    @Override
    public AiUpstreamRequest buildRequest(final ShenyuAiRequest request, final AiCommonConfig config) {
        return delegate.buildRequest(request, config);
    }
}
