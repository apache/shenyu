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
 * Protocol-specific request decoding, outgoing preparation and response encoding.
 *
 * <p>The engine uses the client protocol to decode requests and encode responses.
 * After routing and optional conversion, the target protocol prepares the outgoing
 * payload and inspects the raw upstream response. These can be different protocols
 * in one exchange. Provider authentication and endpoints belong to the provider;
 * HTTP execution, retries, fallback and caching belong to the engine.</p>
 *
 * <p>Use {@link OpenAiResponses} as the native request and response-passthrough
 * reference, and {@link OpenAiChat} for SSE encoding and upstream inspection.</p>
 */
public interface AiProxyProtocol {

    /**
     * Protocol identifier.
     *
     * @return protocol identifier
     */
    String name();

    /**
     * Get precedence used when multiple protocols match one payload.
     * Higher values are detected first.
     *
     * @return detection precedence
     */
    default int detectionOrder() {
        return 0;
    }

    /**
     * Determine whether the JSON body uses this protocol without modifying it.
     * The engine checks this even when the protocol was selected explicitly.
     *
     * @param payload request JSON
     * @return whether it matches
     */
    boolean matches(JsonNode payload);

    /**
     * Decode client request into ShenYu internal request.
     *
     * <p>Validate the protocol envelope and set the protocol, operation, raw body
     * and a request-owned payload. Populate decoded model, stream and token fields
     * when supplied by this protocol. An absent stream value allows the engine to
     * use the configured default. Keep raw body and payload consistent.</p>
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
     * This runs after optional protocol conversion and before provider adaptation.
     * Override it to map gateway options to this protocol's outgoing fields; the
     * default leaves the payload unchanged.
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
     * <p>Provider response adaptation and optional reverse protocol conversion
     * have already run. The default preserves the response for native passthrough.
     * Override when client-facing framing or encoding is needed, keeping body
     * consumption lazy so backpressure and cancellation reach the upstream.</p>
     *
     * @param response provider-adapted response
     * @param stream whether the client requested streaming
     * @return client response
     */
    default AiUpstreamResponse encodeResponse(final AiUpstreamResponse response, final boolean stream) {
        return response;
    }

    /**
     * Get the raw upstream response inspector owned by this protocol.
     *
     * <p>This target-protocol hook runs before response adaptation. The default
     * extracts no usage or completion evidence. Inspectors must not subscribe
     * independently or modify response bytes.</p>
     *
     * @return response inspector
     */
    default AiProtocolResponseInspector responseInspector() {
        return AiProtocolResponseInspector.identity();
    }

    /**
     * Check whether this protocol is compatible with exact response caching.
     *
     * @return whether exact response caching is supported
     */
    default boolean isExactResponseCacheCompatible() {
        return false;
    }
}
