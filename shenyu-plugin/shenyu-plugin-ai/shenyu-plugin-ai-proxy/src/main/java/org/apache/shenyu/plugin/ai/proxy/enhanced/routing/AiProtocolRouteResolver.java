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

package org.apache.shenyu.plugin.ai.proxy.enhanced.routing;

import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.conversion.AiProtocolConversion;
import org.apache.shenyu.plugin.ai.proxy.enhanced.conversion.AiProtocolConverter;
import org.apache.shenyu.plugin.ai.proxy.enhanced.conversion.AiProtocolConverterFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;

import java.util.Collection;
import java.util.Objects;

/**
 * Resolves a client protocol to one Provider-native protocol, preferring native passthrough.
 */
public final class AiProtocolRouteResolver {

    private final AiProtocolConverterFactory converterFactory;

    /**
     * Create a route resolver backed by the directional converter registry.
     *
     * @param converterFactory converter registry
     */
    public AiProtocolRouteResolver(final AiProtocolConverterFactory converterFactory) {
        this.converterFactory = Objects.requireNonNull(converterFactory, "converterFactory must not be null");
    }

    /**
     * Resolve and execute the request side of one request-scoped protocol route.
     *
     * @param request decoded client request
     * @param nativeProtocols protocols accepted natively by the Provider
     * @param stream effective streaming mode
     * @return selected route and its bidirectional conversion
     */
    public AiProtocolRoute resolve(final ShenyuAiRequest request, final Collection<String> nativeProtocols,
                                   final boolean stream) {
        Objects.requireNonNull(request, "AI request is required");
        Objects.requireNonNull(nativeProtocols, "Provider-native AI protocols are required");
        String clientProtocol = request.getProtocol();
        if (Objects.isNull(clientProtocol) || clientProtocol.isBlank()) {
            throw new IllegalArgumentException("Client AI protocol is required");
        }
        for (String nativeProtocol : nativeProtocols) {
            Objects.requireNonNull(nativeProtocol, "Provider-native AI protocol is required");
            if (nativeProtocol.isBlank()) {
                throw new IllegalArgumentException("Provider-native AI protocol must not be blank");
            }
        }

        if (nativeProtocols.contains(clientProtocol)) {
            return new AiProtocolRoute(clientProtocol, clientProtocol, AiProtocolConversion.identity(request));
        }

        AiProtocolConverter converter = converterFactory.findConverter(clientProtocol, nativeProtocols)
                .orElseThrow(() -> new ShenyuException("Unsupported AI protocol route from " + clientProtocol
                        + " to provider protocols " + nativeProtocols));
        AiProtocolConversion conversion = converter.convert(request, stream);

        return new AiProtocolRoute(clientProtocol, converter.targetProtocol(), conversion);
    }
}
