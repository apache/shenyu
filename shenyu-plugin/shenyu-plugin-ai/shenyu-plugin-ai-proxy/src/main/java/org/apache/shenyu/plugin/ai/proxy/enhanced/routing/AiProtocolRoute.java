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

import org.apache.shenyu.plugin.ai.proxy.enhanced.conversion.AiProtocolConversion;

import java.util.Objects;

/**
 * Request-scoped route from a client protocol to one Provider-native target protocol.
 *
 * @param clientProtocol protocol accepted from the client
 * @param targetProtocol protocol sent to the Provider
 * @param conversion converted request and reverse response conversion
 */
public record AiProtocolRoute(
        String clientProtocol,
        String targetProtocol,
        AiProtocolConversion conversion) {

    /**
     * Reject incomplete or inconsistent routes.
     */
    public AiProtocolRoute {
        Objects.requireNonNull(clientProtocol, "Client AI protocol is required");
        Objects.requireNonNull(targetProtocol, "Target AI protocol is required");
        Objects.requireNonNull(conversion, "AI protocol conversion is required");
        if (clientProtocol.isBlank() || targetProtocol.isBlank()) {
            throw new IllegalArgumentException("AI route protocol identifiers must not be blank");
        }
        if (!targetProtocol.equals(conversion.request().getProtocol())) {
            throw new IllegalArgumentException("Converted AI request protocol does not match route target: "
                    + conversion.request().getProtocol() + " != " + targetProtocol);
        }
    }
}
