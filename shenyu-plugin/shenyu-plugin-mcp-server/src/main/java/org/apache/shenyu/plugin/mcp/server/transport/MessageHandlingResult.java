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

package org.apache.shenyu.plugin.mcp.server.transport;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Result object for message handling operations.
 * This class encapsulates the result of processing a Streamable HTTP message,
 * including the HTTP status code, response body, and session correlation information.
 */
public class MessageHandlingResult {

    private static final Logger LOGGER = LoggerFactory.getLogger(MessageHandlingResult.class);

    private final int statusCode;

    private final Object responseBody;

    private final String sessionId;

    /**
     * Creates a new message handling result.
     *
     * @param statusCode   the HTTP status code for the response
     * @param responseBody the response body object
     * @param sessionId    the session identifier for correlation (nullable)
     */
    public MessageHandlingResult(final int statusCode, final Object responseBody, final String sessionId) {
        this.statusCode = statusCode;
        this.responseBody = responseBody;
        this.sessionId = sessionId;
    }

    /**
     * Gets the HTTP status code for this result.
     *
     * @return the status code
     */
    public int getStatusCode() {
        return statusCode;
    }

    /**
     * Gets the response body object.
     *
     * @return the response body
     */
    public Object getResponseBody() {
        return responseBody;
    }

    /**
     * Gets the session identifier associated with this result.
     *
     * @return the session ID, or null if not available
     */
    public String getSessionId() {
        return sessionId;
    }

    /**
     * Converts the response body to a JSON string for HTTP response transmission.
     * If the response body is already a string, returns it as-is. Otherwise,
     * attempts to serialize it to JSON using ObjectMapper. On serialization failure,
     * returns a standard JSON-RPC error response.
     *
     * @return the response body as JSON string
     */
    public String getResponseBodyAsJson() {
        if (responseBody instanceof String) {
            return (String) responseBody;
        }

        try {
            return new ObjectMapper().writeValueAsString(responseBody);
        } catch (Exception e) {
            LOGGER.error("Failed to serialize response body to JSON: {}", e.getMessage());
            return "{\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32603,\"message\":\"Internal error\"}}";
        }
    }
}