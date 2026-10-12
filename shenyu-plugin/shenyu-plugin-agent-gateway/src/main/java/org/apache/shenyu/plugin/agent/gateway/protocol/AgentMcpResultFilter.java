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
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.common.dto.AgentGatewayResultFilterConfig;

import java.util.Objects;

/**
 * One request's configured deletion and unchanged-schema validation.
 */
final class AgentMcpResultFilter {

    private final AgentMcpStructuredContentFilter filter;

    private final AgentMcpOutputSchemaValidator validator;

    AgentMcpResultFilter(final AgentGatewayResultFilterConfig config, final String name, final JsonNode outputSchema) {
        if (config.getPlan(name).isEmpty()) {
            filter = null;
            validator = null;
        } else {
            filter = new AgentMcpStructuredContentFilter(config, name);
            validator = Objects.isNull(outputSchema) || outputSchema.isNull() ? null : new AgentMcpOutputSchemaValidator(outputSchema);
        }
    }

    ObjectNode apply(final ObjectNode result) {
        if (Objects.isNull(filter)) {
            return result;
        }
        ObjectNode filtered = filter.filter(result);
        if (Objects.nonNull(validator) && (filtered.has("structuredContent") || !filtered.path("isError").asBoolean(false))) {
            validator.validate(filtered.get("structuredContent"));
        }
        new AgentMcpResultCopiesFilter().apply(result, filtered);
        return filtered;
    }

    boolean isActive() {
        return Objects.nonNull(filter);
    }
}
