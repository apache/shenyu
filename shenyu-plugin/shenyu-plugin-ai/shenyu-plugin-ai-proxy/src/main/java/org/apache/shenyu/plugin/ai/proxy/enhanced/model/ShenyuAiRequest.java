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

package org.apache.shenyu.plugin.ai.proxy.enhanced.model;

import com.fasterxml.jackson.databind.node.ObjectNode;

import java.util.List;
import java.util.Objects;

/**
 * Protocol-neutral AI request with the original request data retained.
 */
public final class ShenyuAiRequest {

    private String rawBody;

    private ObjectNode rawPayload;

    private String protocol;

    private String model;

    private Integer maxTokens;

    private List<ShenyuAiMessage> messages;

    private Boolean stream;

    /**
     * Get original request body.
     *
     * @return original request body
     */
    public String getRawBody() {
        return rawBody;
    }

    /**
     * Get complete parsed request payload.
     *
     * @return complete parsed request payload
     */
    public ObjectNode getRawPayload() {
        return rawPayload;
    }

    /**
     * Get protocol identifier.
     *
     * @return protocol identifier
     */
    public String getProtocol() {
        return protocol;
    }

    /**
     * Get requested model.
     *
     * @return requested model
     */
    public String getModel() {
        return model;
    }

    /**
     * Determine whether streaming is requested.
     *
     * @return whether streaming is requested
     */
    public boolean isStream() {
        return Boolean.TRUE.equals(stream);
    }

    /**
     * Determine whether the client supplied the stream field.
     *
     * @return whether the client supplied the stream field
     */
    public boolean hasStream() {
        return Objects.nonNull(stream);
    }

    /**
     * Set original request body.
     *
     * @param rawBody original request body
     */
    public void setRawBody(final String rawBody) {
        this.rawBody = rawBody;
    }

    /**
     * Set complete parsed request payload.
     *
     * @param rawPayload complete parsed request payload
     */
    public void setRawPayload(final ObjectNode rawPayload) {
        this.rawPayload = rawPayload;
    }

    /**
     * Set protocol identifier.
     *
     * @param protocol protocol identifier
     */
    public void setProtocol(final String protocol) {
        this.protocol = protocol;
    }

    /**
     * Set requested model.
     *
     * @param model requested model
     */
    public void setModel(final String model) {
        this.model = model;
    }

    /**
     * Set whether streaming is requested.
     *
     * @param stream whether streaming is requested
     */
    public void setStream(final Boolean stream) {
        this.stream = stream;
    }

    /**
     * Get normalized maximum output tokens.
     *
     * @return normalized maximum output tokens
     */
    public Integer getMaxTokens() {
        return maxTokens;
    }

    /**
     * Set normalized maximum output tokens.
     *
     * @param maxTokens normalized maximum output tokens
     */
    public void setMaxTokens(final Integer maxTokens) {
        this.maxTokens = maxTokens;
    }

    /**
     * Get normalized messages.
     *
     * @return normalized messages
     */
    public List<ShenyuAiMessage> getMessages() {
        return messages;
    }

    /**
     * Set normalized messages.
     *
     * @param messages normalized messages
     */
    public void setMessages(final List<ShenyuAiMessage> messages) {
        this.messages = messages;
    }
}
