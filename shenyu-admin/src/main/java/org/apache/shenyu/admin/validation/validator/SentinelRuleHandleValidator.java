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

package org.apache.shenyu.admin.validation.validator;

import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.common.dto.convert.rule.SentinelHandle;
import org.apache.shenyu.common.utils.GsonUtils;

import java.util.Objects;

/**
 * Validates opt-in Sentinel parameter flow handles before storage.
 */
public final class SentinelRuleHandleValidator {

    private SentinelRuleHandleValidator() {
    }

    /**
     * Reject unsupported parameter flow settings before synchronization.
     *
     * @param handle the rule handle from Admin
     */
    public static void validate(final String handle) {
        if (StringUtils.isBlank(handle)) {
            return;
        }
        try {
            final JsonObject json = GsonUtils.getInstance().fromJson(handle, JsonObject.class);
            if (Objects.nonNull(json) && json.has("paramFlowEnable")) {
                GsonUtils.getInstance().fromJson(handle, SentinelHandle.class).validateParamFlow();
            }
        } catch (JsonParseException ex) {
            throw new ShenyuAdminException("sentinel rule handle must be a valid JSON object");
        } catch (IllegalArgumentException ex) {
            throw new ShenyuAdminException(ex.getMessage());
        }
    }
}
