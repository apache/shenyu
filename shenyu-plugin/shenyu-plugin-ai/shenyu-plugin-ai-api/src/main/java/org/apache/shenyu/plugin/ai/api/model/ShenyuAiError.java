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

package org.apache.shenyu.plugin.ai.api.model;

import com.fasterxml.jackson.databind.JsonNode;

/**
 * Normalized AI error with the original payload retained for protocol-specific details.
 *
 * @param type normalized error type
 * @param message human-readable error message
 * @param code provider or protocol error code, when present
 * @param retryable whether retrying the request may succeed
 * @param payload original error payload
 */
public record ShenyuAiError(String type, String message, String code, boolean retryable, JsonNode payload) {
}
