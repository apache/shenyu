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
 * One normalized event from an AI streaming response.
 *
 * @param type event type, such as a content delta or completion
 * @param model model reported by the provider, when present
 * @param content incremental text content, when present
 * @param usage normalized token usage, when reported
 * @param finishReason normalized finish reason, when reported
 * @param payload original event for fields unknown to ShenYu
 */
public record ShenyuAiStreamEvent(String type, String model, String content, ShenyuAiTokenUsage usage,
                                  ShenyuAiFinishReason finishReason, JsonNode payload) {
}
