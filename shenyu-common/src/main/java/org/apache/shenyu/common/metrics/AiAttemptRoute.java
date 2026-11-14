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

package org.apache.shenyu.common.metrics;

import java.util.Objects;

/**
 * Immutable attribution for one actual upstream attempt. Client protocol is the
 * accepted wire contract; target protocol is the provider-native contract after routing.
 *
 * @param provider selected provider identifier
 * @param clientProtocol protocol accepted from the client
 * @param targetProtocol protocol sent to the provider
 * @param requestedModel model requested by the client, when known
 * @param resolvedModel model in the constructed upstream request, when known
 */
public record AiAttemptRoute(String provider, String clientProtocol, String targetProtocol,
                             String requestedModel, String resolvedModel) {

    public AiAttemptRoute {
        requireIdentifier(provider, "AI provider");
        requireIdentifier(clientProtocol, "client AI protocol");
        if (Objects.nonNull(targetProtocol)) {
            requireIdentifier(targetProtocol, "target AI protocol");
        }
        requireModel(requestedModel, "requested AI model");
        requireModel(resolvedModel, "resolved AI model");
    }

    private static void requireIdentifier(final String value, final String name) {
        Objects.requireNonNull(value, name + " is required");
        if (value.isBlank() || value.length() > 128 || !value.equals(value.strip())
                || value.chars().anyMatch(Character::isISOControl)) {
            throw new IllegalArgumentException(name + " must be bounded nonblank text");
        }
    }

    private static void requireModel(final String value, final String name) {
        if (Objects.nonNull(value) && (value.isBlank() || value.length() > 256 || !value.equals(value.strip())
                || value.chars().anyMatch(Character::isISOControl))) {
            throw new IllegalArgumentException(name + " must be bounded nonblank text");
        }
    }
}
