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

package org.apache.shenyu.plugin.agent.gateway.protocol;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.StreamReadConstraints;
import com.fasterxml.jackson.core.StreamReadFeature;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

import java.io.IOException;
import java.util.Objects;
import java.util.Set;

/**
 * Reject opaque result channels; rebuild only verified complete JSON copies.
 * This is a declared-field policy, not arbitrary sensitive-value detection.
 */
final class AgentMcpResultCopiesFilter {

    private static final Set<String> RESULT_FIELDS = Set.of("structuredContent", "content", "isError", "_meta", "resultType");

    private static final Set<String> TEXT_FIELDS = Set.of("type", "text");

    private static final ObjectMapper MAPPER = new ObjectMapper(JsonFactory.builder()
            .enable(StreamReadFeature.STRICT_DUPLICATE_DETECTION)
            .streamReadConstraints(StreamReadConstraints.builder().maxNestingDepth(32).maxStringLength(2097152).build()).build())
            .enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

    void apply(final ObjectNode original, final ObjectNode filtered) {
        JsonNode structured = original.get("structuredContent");
        if (Objects.isNull(structured) || !structured.isObject()) {
            throw invalid();
        }
        original.fieldNames().forEachRemaining(name -> {
            if (!RESULT_FIELDS.contains(name)) {
                throw invalid();
            }
        });
        if (original.has("_meta") && (!original.get("_meta").isObject() || !original.get("_meta").isEmpty())) {
            throw invalid();
        }
        if (original.has("isError") && !original.get("isError").isBoolean()) {
            throw invalid();
        }
        if (original.has("resultType") && !original.get("resultType").isTextual()) {
            throw invalid();
        }
        if (original.has("resultType") && !"complete".equals(original.get("resultType").textValue())) {
            throw invalid();
        }
        JsonNode content = original.get("content");
        if (Objects.isNull(content) || !content.isArray() || content.size() > 32) {
            throw invalid();
        }
        for (int index = 0; index < content.size(); index++) {
            JsonNode block = content.get(index);
            if (!block.isObject() || !"text".equals(block.path("type").asText()) || !block.path("text").isTextual()) {
                throw invalid();
            }
            block.fieldNames().forEachRemaining(name -> {
                if (!TEXT_FIELDS.contains(name)) {
                    throw invalid();
                }
            });
            if (!readCopy(block.get("text").textValue()).equals(structured)) {
                throw invalid();
            }
            ((ObjectNode) filtered.get("content").get(index)).put("text", filtered.get("structuredContent").toString());
        }
    }

    private JsonNode readCopy(final String text) {
        try (JsonParser parser = MAPPER.createParser(text)) {
            // Bound streaming tokens before allocating a tree, including field names.
            int tokens = 0;
            while (Objects.nonNull(parser.nextToken())) {
                if (++tokens > 49152) {
                    throw invalid();
                }
            }
            JsonNode copy = MAPPER.readTree(text);
            if (Objects.isNull(copy) || !copy.isObject()) {
                throw invalid();
            }
            return copy;
        } catch (IOException error) {
            // Do not retain parser exceptions containing untrusted text.
            throw invalid();
        }
    }

    private IllegalArgumentException invalid() {
        return new IllegalArgumentException("Tool result contains unsupported or unverifiable copies");
    }
}
