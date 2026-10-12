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

import org.apache.shenyu.common.exception.ShenyuException;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;

/**
 * Registry for AI provider implementations.
 */
public final class AiProxyProviderFactory {

    private final Map<String, AiProxyProvider> aiProxyProviderMap;

    /**
     * Create a provider registry.
     *
     * @param providers provider implementations
     */
    public AiProxyProviderFactory(final Collection<AiProxyProvider> providers) {
        this.aiProxyProviderMap = new LinkedHashMap<>();
        for (AiProxyProvider provider : providers) {
            final String name = normalize(provider.name());
            final AiProxyProvider previous = aiProxyProviderMap.put(name, provider);
            if (Objects.nonNull(previous)) {
                throw new IllegalArgumentException("Duplicate AI provider: " + provider.name());
            }
        }
    }

    /**
     * Get a provider by identifier.
     *
     * @param provider provider identifier
     * @return provider implementation
     */
    public AiProxyProvider getProvider(final String provider) {
        final AiProxyProvider result = aiProxyProviderMap.get(normalize(provider));
        if (Objects.isNull(result)) {
            throw new ShenyuException("Unsupported AI provider: " + provider);
        }
        return result;
    }

    private String normalize(final String provider) {
        if (Objects.isNull(provider)) {
            return "";
        }
        return provider.trim().toLowerCase(Locale.ROOT).replace("_", "").replace("-", "");
    }
}
