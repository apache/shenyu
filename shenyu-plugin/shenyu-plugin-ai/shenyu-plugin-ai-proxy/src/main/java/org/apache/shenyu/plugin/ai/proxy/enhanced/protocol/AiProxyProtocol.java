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
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;

import java.util.Objects;

/**
 * Contract for decoding a client AI protocol into ShenYu-owned models.
 */
public interface AiProxyProtocol {

    /**
     * Protocol identifier.
     *
     * @return protocol identifier
     */
    String name();

    /**
     * Determine whether the JSON body uses this protocol.
     *
     * @param payload request JSON
     * @return whether it matches
     */
    boolean matches(JsonNode payload);

    /**
     * Decode client request into ShenYu internal request.
     *
     * @param rawBody original request body
     * @param payload parsed request JSON
     * @return internal AI request
     */
    ShenyuAiRequest decodeRequest(
            String rawBody,
            ObjectNode payload);

    /**
     * Prepare a request payload for this target protocol.
     *
     * <p>The engine supplies a request-scoped copy, so implementations may modify and return it.
     * Provider authentication, endpoint selection and provider-specific dialect adaptation do not belong here.</p>
     *
     * @param payload target protocol request payload
     * @param overrides protocol-level request overrides
     * @return prepared request payload
     */
    default ObjectNode prepareOutgoingRequest(final ObjectNode payload, final AiProtocolRequestOverrides overrides) {
        Objects.requireNonNull(overrides, "AI protocol request overrides are required");
        return Objects.requireNonNull(payload, "AI protocol payload is required");
    }

    /**
     * Encode provider-adapted response bytes for the client protocol.
     *
     * @param response provider-adapted response
     * @param stream whether the client requested streaming
     * @return client response
     */
    default AiUpstreamResponse encodeResponse(final AiUpstreamResponse response, final boolean stream) {
        return response;
    }
}
