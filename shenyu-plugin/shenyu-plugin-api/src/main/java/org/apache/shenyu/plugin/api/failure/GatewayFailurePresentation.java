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

package org.apache.shenyu.plugin.api.failure;

import java.util.Objects;

/**
 * Bounded HTTP error policy; no writer, exchange, cause, retry or chain access.
 * @param status public HTTP error status
 * @param code public application error code
 * @param message deliberate safe public message, never an exception message by default
 * @param emptyBody whether to retain an empty HTTP error body
 */
public record GatewayFailurePresentation(int status, int code, String message, boolean emptyBody) {

    public GatewayFailurePresentation {
        if (status < 400 || status > 599 || code < 0 || Objects.isNull(message) || message.length() > 256
                || message.chars().anyMatch(Character::isISOControl)) {
            throw new IllegalArgumentException("Invalid gateway HTTP failure presentation");
        }
    }
}
