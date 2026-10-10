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

package org.apache.shenyu.plugin.agent.gateway.protocol;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;

import java.util.Objects;

/**
 * A request-local protocol failure with a sanitized wire response.
 */
public final class AgentMcpProtocolException extends RuntimeException {

    private static final long serialVersionUID = 1L;

    private final int httpStatus;

    private final int code;

    private final JsonNode id;

    private final ObjectNode data;

    AgentMcpProtocolException(final int httpStatus, final int code, final String message, final JsonNode id, final ObjectNode data) {
        super(message);
        this.httpStatus = httpStatus;
        this.code = code;
        this.id = Objects.isNull(id) ? JsonNodeFactory.instance.nullNode() : id.deepCopy();
        this.data = Objects.isNull(data) ? null : data.deepCopy();
    }

    public int getHttpStatus() {
        return httpStatus;
    }

    public int getCode() {
        return code;
    }

    /**
     * Build an independent error envelope without body, header or exception details.
     *
     * @return the JSON-RPC error response
     */
    public ObjectNode toResponse() {
        ObjectNode response = JsonNodeFactory.instance.objectNode();
        response.put("jsonrpc", "2.0");
        response.set("id", id.deepCopy());
        ObjectNode error = response.putObject("error");
        error.put("code", code);
        error.put("message", getMessage());
        if (Objects.nonNull(data)) {
            error.set("data", data.deepCopy());
        }
        return response;
    }
}
