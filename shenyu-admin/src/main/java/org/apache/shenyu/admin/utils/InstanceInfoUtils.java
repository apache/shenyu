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

package org.apache.shenyu.admin.utils;

import com.google.gson.JsonElement;
import com.google.gson.JsonParseException;
import com.google.gson.JsonParser;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Canonical representation of optional instance information.
 */
public final class InstanceInfoUtils {

    private static final Logger LOG = LoggerFactory.getLogger(InstanceInfoUtils.class);

    private InstanceInfoUtils() {
    }

    /**
     * Normalize optional instance information to JSON object text.
     *
     * @param information reported or persisted instance information
     * @return JSON object text that consumers can parse once
     */
    public static String normalize(final String information) {
        if (StringUtils.isBlank(information)) {
            return "{}";
        }
        try {
            JsonElement element = JsonParser.parseString(information);
            // Older reporters may wrap the object text in one JSON string.
            if (element.isJsonPrimitive() && element.getAsJsonPrimitive().isString()) {
                element = JsonParser.parseString(element.getAsString());
            }
            if (element.isJsonObject()) {
                // Compact whitespace without decoding nested string values or sorting keys.
                return element.toString();
            }
        } catch (JsonParseException | IllegalStateException ex) {
            LOG.debug("Ignoring malformed optional instance information");
            return "{}";
        }
        LOG.debug("Ignoring optional instance information that is not a JSON object");
        return "{}";
    }
}
