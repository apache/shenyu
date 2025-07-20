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

package org.apache.shenyu.plugin.mcp.server.request;

import com.google.gson.JsonElement;
import com.google.gson.JsonPrimitive;
import org.apache.shenyu.common.utils.GsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.StringUtils;

import java.util.Objects;

/**
 * Simple parameter formatting utilities.
 */
public class ParameterFormatter {

    private static final Logger LOG = LoggerFactory.getLogger(ParameterFormatter.class);

    /**
     * Try to parse a JSON string into JsonElement.
     * If parsing fails, returns the original string as JsonPrimitive.
     *
     * @param jsonString the JSON string to parse
     * @return the parsed JsonElement or original string
     */
    public static JsonElement tryParseJsonString(final String jsonString) {
        if (!StringUtils.hasText(jsonString)) {
            return new JsonPrimitive(Objects.isNull(jsonString) ? "" : jsonString);
        }

        String trimmed = jsonString.trim();
        if (trimmed.startsWith("[") && trimmed.endsWith("]")
                || trimmed.startsWith("{") && trimmed.endsWith("}")) {
            try {
                return GsonUtils.getInstance().fromJson(jsonString, JsonElement.class);
            } catch (Exception e) {
                LOG.debug("Failed to parse JSON string, keeping as string: {}", e.getMessage());
            }
        }

        return new JsonPrimitive(jsonString);
    }
} 