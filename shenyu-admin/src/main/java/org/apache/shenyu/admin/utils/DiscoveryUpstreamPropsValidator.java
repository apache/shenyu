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
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.common.dto.convert.rule.canary.CanaryConfigValidator;
import org.apache.shenyu.common.utils.GsonUtils;

import java.util.Objects;

/**
 * Validation of submitted discovery upstream properties without changing replacement semantics.
 */
public final class DiscoveryUpstreamPropsValidator {

    private DiscoveryUpstreamPropsValidator() {
    }

    /**
     * Validate the submitted properties without merging or rewriting them.
     * Omitted labels and an empty labels object are both valid; null labels are invalid.
     * Empty properties retain the existing API behavior for optional properties.
     *
     * @param props the complete submitted properties
     */
    public static void validate(final String props) {
        if (StringUtils.isBlank(props)) {
            return;
        }
        try {
            JsonElement parsed = GsonUtils.getInstance().fromJson(props, JsonElement.class);
            if (Objects.isNull(parsed) || !parsed.isJsonObject()) {
                throw new IllegalArgumentException("props must be a JSON object");
            }
            JsonObject object = parsed.getAsJsonObject();
            if (object.has("labels")) {
                CanaryConfigValidator.validateLabels(object.get("labels"), "props.labels", true);
            }
        } catch (IllegalArgumentException | JsonParseException ex) {
            throw new ShenyuAdminException("Invalid upstream props: " + ex.getMessage(), ex);
        }
    }
}
