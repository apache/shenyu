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

/**
 * Normalized AI message with its original protocol payload.
 */
public final class ShenyuAiMessage {
    private String role;

    private String content;

    private ObjectNode rawPayload;

    /**
     * Get message role.
     *
     * @return message role
     */
    public String getRole() {
        return role;
    }

    /**
     * Set message role.
     *
     * @param role message role
     */
    public void setRole(final String role) {
        this.role = role;
    }

    /**
     * Get textual content when the message contains plain text.
     *
     * @return textual content, or null for structured content
     */
    public String getContent() {
        return content;
    }

    /**
     * Set textual content.
     *
     * @param content textual content
     */
    public void setContent(final String content) {
        this.content = content;
    }

    /**
     * Get the complete message payload.
     *
     * @return complete message payload
     */
    public ObjectNode getRawPayload() {
        return rawPayload;
    }

    /**
     * Set the complete message payload.
     *
     * @param rawPayload complete message payload
     */
    public void setRawPayload(final ObjectNode rawPayload) {
        this.rawPayload = rawPayload;
    }
}
