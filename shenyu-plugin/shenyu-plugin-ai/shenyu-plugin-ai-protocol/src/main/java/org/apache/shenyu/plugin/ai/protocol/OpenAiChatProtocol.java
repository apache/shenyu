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

package org.apache.shenyu.plugin.ai.protocol;

import com.fasterxml.jackson.databind.JsonNode;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * The OpenAI Chat Completions protocol evidence.
 */
public final class OpenAiChatProtocol implements AiProtocolDescriptor {

    private static final String FIELD_MESSAGES = "messages";

    @Override
    public String id() {
        return AiProtocols.OPENAI_CHAT;
    }

    @Override
    public List<String> paths() {
        return List.of("/v1/chat/completions", "/chat/completions");
    }

    @Override
    public Map<String, String> headerHints() {
        return Collections.emptyMap();
    }

    @Override
    public boolean bodyMatches(final JsonNode payload) {
        return Objects.nonNull(payload)
                && payload.isObject()
                && payload.has(FIELD_MESSAGES)
                && payload.get(FIELD_MESSAGES).isArray();
    }
}
