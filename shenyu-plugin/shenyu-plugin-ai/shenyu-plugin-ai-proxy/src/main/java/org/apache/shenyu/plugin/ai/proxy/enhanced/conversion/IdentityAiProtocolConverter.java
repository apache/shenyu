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

import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;

import java.util.Objects;

/**
 * Identity conversion for requests whose source and target protocols are the same.
 */
public final class IdentityAiProtocolConverter implements AiProtocolConverter {

    private final String protocol;

    /**
     * Create an identity converter for one protocol.
     *
     * @param protocol protocol identifier
     */
    public IdentityAiProtocolConverter(final String protocol) {
        this.protocol = Objects.requireNonNull(protocol, "AI protocol is required");
        if (protocol.isBlank()) {
            throw new IllegalArgumentException("AI protocol must not be blank");
        }
    }

    @Override
    public String sourceProtocol() {
        return protocol;
    }

    @Override
    public String targetProtocol() {
        return protocol;
    }

    @Override
    public AiProtocolConversion convert(final ShenyuAiRequest request, final boolean stream) {
        Objects.requireNonNull(request, "AI request is required");
        if (!protocol.equals(request.getProtocol())) {
            throw new AiProxyException(AiProxyException.Kind.REQUEST,
                    "Expected AI protocol: " + protocol, null);
        }
        return AiProtocolConversion.identity(request);
    }
}
