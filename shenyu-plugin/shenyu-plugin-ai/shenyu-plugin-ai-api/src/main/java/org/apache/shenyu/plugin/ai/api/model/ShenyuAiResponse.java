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

import java.util.List;
import java.util.Map;

/**
 * Protocol-neutral view of a completed AI response.
 *
 * @param statusCode upstream HTTP status code
 * @param headers upstream response headers
 * @param model model reported by the provider, when present
 * @param usage normalized token usage, when reported
 * @param finishReason normalized finish reason, when reported
 * @param error normalized provider or protocol error, when present
 * @param payload original provider response for fields unknown to ShenYu
 */
public record ShenyuAiResponse(int statusCode, Map<String, List<String>> headers, String model,
                               ShenyuAiTokenUsage usage, ShenyuAiFinishReason finishReason,
                               ShenyuAiError error, JsonNode payload) {
}
