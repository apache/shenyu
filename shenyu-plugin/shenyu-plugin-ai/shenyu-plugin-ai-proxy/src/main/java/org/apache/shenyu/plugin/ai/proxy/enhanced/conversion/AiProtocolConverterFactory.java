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

import org.apache.shenyu.common.exception.ShenyuException;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;

/**
 * Registry for directional AI protocol converters.
 */
public final class AiProtocolConverterFactory {

    private final Map<ConversionKey, AiProtocolConverter> converters;

    /**
     * Create a converter registry and reject duplicate directional routes.
     *
     * @param converters protocol converters
     */
    public AiProtocolConverterFactory(final Collection<AiProtocolConverter> converters) {
        this.converters = new LinkedHashMap<>();
        for (AiProtocolConverter converter : converters) {
            Objects.requireNonNull(converter, "AI protocol converter is required");
            ConversionKey key = new ConversionKey(converter.sourceProtocol(), converter.targetProtocol());
            AiProtocolConverter previous = this.converters.putIfAbsent(key, converter);
            if (Objects.nonNull(previous)) {
                throw new IllegalArgumentException("Duplicate AI protocol converter: "
                        + key.sourceProtocol() + " -> " + key.targetProtocol());
            }
        }
    }

    /**
     * Get a converter for an exact directional route.
     *
     * @param sourceProtocol source protocol identifier
     * @param targetProtocol target protocol identifier
     * @return resolved protocol converter
     */
    public AiProtocolConverter getConverter(final String sourceProtocol, final String targetProtocol) {
        ConversionKey key = new ConversionKey(sourceProtocol, targetProtocol);
        if (sourceProtocol.equals(targetProtocol)) {
            return new IdentityAiProtocolConverter(sourceProtocol);
        }
        AiProtocolConverter converter = converters.get(key);
        if (Objects.isNull(converter)) {
            throw new ShenyuException("Unsupported AI protocol conversion: "
                    + sourceProtocol + " -> " + targetProtocol);
        }
        return converter;
    }

    /**
     * Resolve a converter from a client protocol to one of the protocols natively accepted by a provider.
     * Native support takes precedence over registered conversion routes. When multiple routes are available,
     * converter registration order determines the selected target.
     *
     * @param sourceProtocol client protocol identifier
     * @param targetProtocols protocols natively accepted by the provider
     * @return identity or directional protocol converter
     */
    public AiProtocolConverter resolveConverter(final String sourceProtocol, final Collection<String> targetProtocols) {
        Objects.requireNonNull(sourceProtocol, "Source AI protocol is required");
        Objects.requireNonNull(targetProtocols, "Target AI protocols are required");
        if (sourceProtocol.isBlank()) {
            throw new IllegalArgumentException("Source AI protocol must not be blank");
        }
        for (String targetProtocol : targetProtocols) {
            Objects.requireNonNull(targetProtocol, "Target AI protocol is required");
            if (targetProtocol.isBlank()) {
                throw new IllegalArgumentException("Target AI protocol must not be blank");
            }
        }
        if (targetProtocols.contains(sourceProtocol)) {
            return new IdentityAiProtocolConverter(sourceProtocol);
        }
        for (Map.Entry<ConversionKey, AiProtocolConverter> entry : converters.entrySet()) {
            if (entry.getKey().sourceProtocol().equals(sourceProtocol)
                    && targetProtocols.contains(entry.getKey().targetProtocol())) {
                return entry.getValue();
            }
        }
        throw new ShenyuException("Unsupported AI protocol conversion from " + sourceProtocol
                + " to provider protocols " + targetProtocols);
    }

    /**
     * Determine whether an identity or registered directional route is supported.
     *
     * @param sourceProtocol source protocol identifier
     * @param targetProtocol target protocol identifier
     * @return whether the route is supported
     */
    public boolean supports(final String sourceProtocol, final String targetProtocol) {
        if (Objects.isNull(sourceProtocol) || Objects.isNull(targetProtocol)
                || sourceProtocol.isBlank() || targetProtocol.isBlank()) {
            return false;
        }
        return sourceProtocol.equals(targetProtocol)
                || converters.containsKey(new ConversionKey(sourceProtocol, targetProtocol));
    }

    private record ConversionKey(String sourceProtocol, String targetProtocol) {

        private ConversionKey {
            Objects.requireNonNull(sourceProtocol, "Source AI protocol is required");
            Objects.requireNonNull(targetProtocol, "Target AI protocol is required");
            if (sourceProtocol.isBlank() || targetProtocol.isBlank()) {
                throw new IllegalArgumentException("AI protocol identifiers must not be blank");
            }
        }
    }
}
