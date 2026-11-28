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

package org.apache.shenyu.plugin.ai.proxy.enhanced.service.structured;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiContentCompliance;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiStructuredOutputPolicy;
import org.apache.shenyu.plugin.base.schema.BoundedJsonSchemaValidator;

import java.util.Objects;

/**
 * Pure classifier for one materialized non-streaming chat completion.
 */
final class AiGeneratedContentInspector {

    private static final int MAX_PREVIOUS_OUTPUT_CHARS = 2048;

    private static final int MAX_SUMMARY_CHARS = 512;

    private static final ObjectMapper MAPPER = new ObjectMapper().enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

    private AiGeneratedContentInspector() {
    }

    static AiContentInspection classify(final AiStructuredOutputPolicy policy, final String envelope) {
        if (envelope.isBlank()) {
            return new AiContentInspection(AiContentCompliance.MALFORMED_ENVELOPE, "empty response envelope", null);
        }
        final JsonNode root;
        try {
            root = MAPPER.readTree(envelope);
        } catch (JsonProcessingException error) {
            return new AiContentInspection(AiContentCompliance.MALFORMED_ENVELOPE,
                    bounded("response envelope is not valid JSON: " + error.getOriginalMessage()), null);
        }
        if (!root.isObject() || !root.path("choices").isArray() || root.path("choices").isEmpty()) {
            return new AiContentInspection(AiContentCompliance.MALFORMED_ENVELOPE,
                    "response envelope has no chat completion choices", null);
        }
        JsonNode first = root.path("choices").get(0);
        JsonNode message = first.path("message");
        String finishReason = first.path("finish_reason").asText(null);
        if ("tool_calls".equals(finishReason) || message.has("tool_calls")) {
            return new AiContentInspection(AiContentCompliance.TOOL_CALLS_PRESENT, null, null);
        }
        if ("content_filter".equals(finishReason)) {
            return new AiContentInspection(AiContentCompliance.REFUSAL, "upstream declared content_filter", null);
        }
        if ("length".equals(finishReason)) {
            return new AiContentInspection(AiContentCompliance.TRUNCATED, "upstream declared finish_reason length", null);
        }
        JsonNode content = message.path("content");
        if (content.isMissingNode() || content.isNull() || content.isTextual() && content.asText().isBlank()) {
            return new AiContentInspection(AiContentCompliance.EMPTY, "generated content is absent or blank", null);
        }
        if (!content.isTextual()) {
            return new AiContentInspection(AiContentCompliance.SCHEMA_MISMATCH,
                    "generated content is not a text field, structured-output contracts apply to text", null);
        }
        String generated = content.asText();
        final JsonNode generatedJson;
        try {
            generatedJson = MAPPER.readTree(generated);
        } catch (JsonProcessingException error) {
            return new AiContentInspection(AiContentCompliance.INVALID_JSON,
                    bounded("generated content is not valid JSON: " + error.getOriginalMessage()),
                    boundedOutput(generated));
        }
        try {
            policy.validator().validateValue(generatedJson);
        } catch (BoundedJsonSchemaValidator.SchemaBudgetExceededException error) {
            return new AiContentInspection(AiContentCompliance.SCHEMA_BUDGET_EXCEEDED,
                    bounded("generated content exceeds the bounded validation budget: " + error.getMessage()),
                    boundedOutput(generated));
        } catch (IllegalArgumentException error) {
            return new AiContentInspection(AiContentCompliance.SCHEMA_MISMATCH,
                    bounded("generated content violates the operator schema: " + error.getMessage()),
                    boundedOutput(generated));
        }
        return new AiContentInspection(AiContentCompliance.VALID, null, null);
    }

    private static String boundedOutput(final String generated) {
        if (Objects.isNull(generated) || generated.length() <= MAX_PREVIOUS_OUTPUT_CHARS) {
            return generated;
        }
        return generated.substring(0, MAX_PREVIOUS_OUTPUT_CHARS) + "...(truncated)";
    }

    private static String bounded(final String summary) {
        if (Objects.isNull(summary) || summary.length() <= MAX_SUMMARY_CHARS) {
            return summary;
        }
        return summary.substring(0, MAX_SUMMARY_CHARS) + "...(truncated)";
    }

    /**
     * Bounded generated-content classification.
     *
     * @param compliance classified outcome
     * @param summary bounded diagnostic evidence
     * @param previousOutput bounded prior generated content
     */
    record AiContentInspection(AiContentCompliance compliance, String summary, String previousOutput) {
    }
}
