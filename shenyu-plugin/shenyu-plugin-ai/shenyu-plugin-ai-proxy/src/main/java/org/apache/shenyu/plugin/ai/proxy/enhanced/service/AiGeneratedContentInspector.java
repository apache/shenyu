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

package org.apache.shenyu.plugin.ai.proxy.enhanced.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiContentCompliance;
import org.apache.shenyu.plugin.base.schema.BoundedJsonSchemaValidator;

import java.util.Objects;

/**
 * Classifies the generated content of one materialized non-streaming chat
 * completion (issue #8257). Every classification is derived from protocol fields
 * ({@code finish_reason}, {@code tool_calls}, content presence) or from the
 * bounded schema validator — never from a semantic judgement — and the evidence
 * it carries for repair composition is bounded. Inspection is pure local work and
 * is never recorded as an upstream attempt.
 */
public final class AiGeneratedContentInspector {

    /**
     * Maximum characters of prior model output carried into a repair request as
     * data; longer outputs are represented by their prefix.
     */
    private static final int MAX_PREVIOUS_OUTPUT_CHARS = 2048;

    /**
     * Maximum characters of the classification summary carried into a repair
     * request and failure messages.
     */
    private static final int MAX_SUMMARY_CHARS = 512;

    private static final ObjectMapper MAPPER = new ObjectMapper().enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

    private AiGeneratedContentInspector() {
    }

    /**
     * Classify one materialized chat completion envelope.
     *
     * @param policy the active structured output policy
     * @param envelope the materialized response body
     * @return the classification
     */
    public static AiContentInspection classify(final AiStructuredOutputPolicy policy, final String envelope) {
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
        if (content.isMissingNode() || content.isNull() || (content.isTextual() && content.asText().isBlank())) {
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
                    bounded("generated content is not valid JSON: " + error.getOriginalMessage()), boundedOutput(generated));
        }
        try {
            policy.validator().validateValue(generatedJson);
        } catch (BoundedJsonSchemaValidator.SchemaBudgetExceededException error) {
            // an oversized document may be perfectly valid: rejecting it is fail-closed,
            // but regenerating it can only waste billable work
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

    /**
     * Bounded reference of the prior output for repair composition.
     *
     * @param generated the prior generated content
     * @return the bounded prefix
     */
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
     * One classification result.
     *
     * @param compliance the classified outcome
     * @param summary bounded human-readable evidence, {@code null} when valid
     * @param previousOutput bounded prior generated content, {@code null} when absent
     */
    public record AiContentInspection(AiContentCompliance compliance, String summary, String previousOutput) {
    }
}
