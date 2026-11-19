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

import com.google.gson.JsonSyntaxException;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.common.dto.convert.rule.impl.WebSocketRuleHandle;
import org.apache.shenyu.common.utils.GsonUtils;

import java.util.Objects;

/**
 * Validates websocket rule handles before Admin stores them.
 */
public final class WebSocketRuleHandleValidator {

    private WebSocketRuleHandleValidator() {
    }

    /**
     * Validate a websocket rule handle, rejecting negative timeout values before the
     * rule is persisted, because the gateway treats them as disabled deadlines.
     *
     * @param handle the rule handle supplied to Admin
     */
    public static void validate(final String handle) {
        if (StringUtils.isBlank(handle)) {
            return;
        }
        final WebSocketRuleHandle webSocketHandle;
        try {
            webSocketHandle = GsonUtils.getInstance().fromJson(handle, WebSocketRuleHandle.class);
        } catch (JsonSyntaxException ex) {
            throw new ShenyuAdminException("websocket rule handle must be a valid JSON object");
        }
        if (Objects.isNull(webSocketHandle)) {
            throw new ShenyuAdminException("websocket rule handle must be a valid JSON object");
        }
        if (webSocketHandle.getTimeout() < 0) {
            throw new ShenyuAdminException("websocket rule handle timeout must not be negative");
        }
        if (webSocketHandle.getIdleTimeout() < 0) {
            throw new ShenyuAdminException("websocket rule handle idleTimeout must not be negative");
        }
    }
}
