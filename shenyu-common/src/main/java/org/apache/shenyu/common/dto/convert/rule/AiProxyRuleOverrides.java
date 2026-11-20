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

package org.apache.shenyu.common.dto.convert.rule;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.StreamReadConstraints;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.IOException;
import java.util.EnumSet;
import java.util.Objects;
import java.util.Set;

/**
 * Opt-in generation defaults, parsed once at Rule publication; no target or credential fields.
 * @param specified fields present in the block, including explicit null clearing
 * @param model configured model, null restores the request model
 * @param temperature generation temperature, null restores the request/default
 * @param maxTokens token limit, null restores the request/default
 * @param stream default response mode, explicit client stream still takes precedence
 */
public record AiProxyRuleOverrides(Set<Setting> specified, String model, Double temperature, Integer maxTokens, Boolean stream) {

    public static final AiProxyRuleOverrides NONE = new AiProxyRuleOverrides(Set.of(), null, null, null, null);

    private static final ObjectMapper MAPPER = mapper();

    public AiProxyRuleOverrides {
        specified = Set.copyOf(specified);
    }

    /**
     * Parse the new block, leaving legacy valid root fields inert.
     * @param source synchronized Rule handle
     * @return immutable values with explicit presence
     */
    public static AiProxyRuleOverrides parse(final String source) {
        if (Objects.isNull(source) || source.isBlank()) {
            return NONE;
        }
        if (source.length() > 65536) {
            throw invalid();
        }
        final JsonNode root;
        try {
            root = MAPPER.readTree(source);
        } catch (IOException error) {
            throw invalid();
        }
        if (Objects.isNull(root) || root.isNull()) {
            return NONE;
        }
        if (!root.isObject()) {
            throw invalid();
        }
        JsonNode block = root.get("generationOverrides");
        if (Objects.isNull(block) || block.isNull()) {
            return NONE;
        }
        if (!block.isObject() || block.size() > 4) {
            throw invalid();
        }
        EnumSet<Setting> present = EnumSet.noneOf(Setting.class);
        block.fieldNames().forEachRemaining(name -> {
            Setting setting = switch (name) {
                case "model" -> Setting.MODEL;
                case "temperature" -> Setting.TEMPERATURE;
                case "maxTokens" -> Setting.MAX_TOKENS;
                case "stream" -> Setting.STREAM;
                default -> throw invalid();
            };
            present.add(setting);
        });
        String model = model(block.get("model"));
        Double temperature = temperature(block.get("temperature"));
        Integer maxTokens = maxTokens(block.get("maxTokens"));
        JsonNode stream = block.get("stream");
        if (value(stream) && !stream.isBoolean()) {
            throw invalid();
        }
        return new AiProxyRuleOverrides(present, model, temperature, maxTokens, value(stream) ? stream.booleanValue() : null);
    }

    private static String model(final JsonNode value) {
        if (!value(value)) {
            return null;
        }
        if (!value.isTextual() || value.textValue().isBlank() || value.textValue().length() > 256
                || !value.textValue().equals(value.textValue().trim()) || value.textValue().chars().anyMatch(Character::isISOControl)) {
            throw invalid();
        }
        return value.textValue();
    }

    private static Double temperature(final JsonNode value) {
        if (!value(value)) {
            return null;
        }
        if (!value.isNumber() || !Double.isFinite(value.doubleValue()) || value.doubleValue() < 0 || value.doubleValue() > 2) {
            throw invalid();
        }
        return value.doubleValue();
    }

    private static Integer maxTokens(final JsonNode value) {
        if (!value(value)) {
            return null;
        }
        if (!value.isIntegralNumber() || !value.canConvertToInt() || value.intValue() <= 0) {
            throw invalid();
        }
        return value.intValue();
    }

    private static boolean value(final JsonNode node) {
        return Objects.nonNull(node) && !node.isNull();
    }

    private static IllegalArgumentException invalid() {
        return new IllegalArgumentException("Invalid AI Rule generation overrides");
    }

    private static ObjectMapper mapper() {
        ObjectMapper mapper = new ObjectMapper().enable(JsonParser.Feature.STRICT_DUPLICATE_DETECTION)
                .enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);
        mapper.getFactory().setStreamReadConstraints(StreamReadConstraints.builder().maxNestingDepth(32).maxStringLength(65536).maxNumberLength(1000).build());
        return mapper;
    }

    /** Explicit presence keeps clearing distinct from inheritance. */
    public enum Setting {
        MODEL, TEMPERATURE, MAX_TOKENS, STREAM
    }
}
