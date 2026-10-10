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

package org.apache.shenyu.plugin.ai.proxy.enhanced.protocol;

import com.fasterxml.jackson.databind.JsonNode;
import org.apache.shenyu.common.exception.ShenyuException;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;

/**
 * Registry for AI protocol implementations.
 */
public final class AiProxyProtocolFactory {

    private final Map<String, AiProxyProtocol> aiProxyProtocolMap;

    /**
     * Create a protocol registry.
     *
     * @param protocols protocol implementations
     */
    public AiProxyProtocolFactory(final Collection<AiProxyProtocol> protocols) {
        this.aiProxyProtocolMap = new LinkedHashMap<>();
        for (AiProxyProtocol protocol : protocols) {
            final AiProxyProtocol previous = aiProxyProtocolMap.put(protocol.name(), protocol);
            if (Objects.nonNull(previous)) {
                throw new IllegalArgumentException("Duplicate AI protocol: " + protocol.name());
            }
        }
    }

    /**
     * Get a protocol by its stable identifier.
     *
     * @param protocol protocol identifier
     * @return protocol implementation
     */
    public AiProxyProtocol getProtocol(final String protocol) {
        final AiProxyProtocol result = aiProxyProtocolMap.get(protocol);
        if (Objects.isNull(result)) {
            throw new ShenyuException("Unsupported AI protocol: " + protocol);
        }
        return result;
    }

    /**
     * Detect a protocol from the parsed request payload.
     *
     * @param payload parsed request payload
     * @return matching protocol
     */
    public AiProxyProtocol detect(final JsonNode payload) {
        return aiProxyProtocolMap.values().stream()
                .filter(protocol -> protocol.matches(payload))
                .findFirst()
                .orElseThrow(() -> new ShenyuException("No AI protocol matches the request body"));
    }
}
