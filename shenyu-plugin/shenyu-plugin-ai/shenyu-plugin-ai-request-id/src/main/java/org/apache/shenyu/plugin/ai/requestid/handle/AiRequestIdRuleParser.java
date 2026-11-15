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

package org.apache.shenyu.plugin.ai.requestid.handle;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.JsonParser;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.convert.rule.AiRequestIdHandle;
import org.apache.shenyu.common.utils.GsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;
import java.util.Set;

/**
 * Parser for AI request id rule handles.
 */
public final class AiRequestIdRuleParser {

    private static final Logger LOG = LoggerFactory.getLogger(AiRequestIdRuleParser.class);

    private static final String RESPONSE_HEADER_ENABLED = "responseHeaderEnabled";

    private static final Set<String> SUPPORTED_FIELDS = Set.of(RESPONSE_HEADER_ENABLED);

    /**
     * Parse one rule handle. Unknown fields are ignored for forward compatibility.
     *
     * @param rawHandle raw rule handle
     * @return parsed rule or an invalid rule with a stable reason
     */
    public AiRequestIdRule parse(final String rawHandle) {
        if (StringUtils.isBlank(rawHandle)) {
            return AiRequestIdRule.invalid(rawHandle, "rule handle must not be empty");
        }
        try {
            final JsonElement element = JsonParser.parseString(rawHandle);
            if (!element.isJsonObject()) {
                return AiRequestIdRule.invalid(rawHandle, "rule handle must be a JSON object");
            }
            final JsonObject object = element.getAsJsonObject();
            for (String field : object.keySet()) {
                if (!SUPPORTED_FIELDS.contains(field)) {
                    LOG.warn("Ignoring unknown AI request id rule handle field: {}", field);
                }
            }
            if (object.has(RESPONSE_HEADER_ENABLED)) {
                final JsonElement value = object.get(RESPONSE_HEADER_ENABLED);
                if (!value.isJsonPrimitive() || !value.getAsJsonPrimitive().isBoolean()) {
                    return AiRequestIdRule.invalid(rawHandle, "responseHeaderEnabled must be a boolean");
                }
            }
            final AiRequestIdHandle handle = GsonUtils.getInstance().fromJson(rawHandle, AiRequestIdHandle.class);
            final boolean responseHeaderEnabled = Objects.isNull(handle) || Objects.isNull(handle.getResponseHeaderEnabled())
                    || handle.getResponseHeaderEnabled();
            return AiRequestIdRule.valid(rawHandle, responseHeaderEnabled);
        } catch (JsonParseException | IllegalStateException | UnsupportedOperationException ex) {
            return AiRequestIdRule.invalid(rawHandle, "rule handle is not valid JSON");
        }
    }
}
