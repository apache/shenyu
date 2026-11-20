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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.common.dto.convert.rule.AiStructuredOutputConfig;
import org.apache.shenyu.common.cache.WindowTinyLFUMap;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiContentCompliance;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.base.schema.BoundedJsonSchemaValidator;

import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * The opt-in structured output policy of one request (issue #8257), built from the
 * selector's operator-managed configuration.
 *
 * <p>Construction is fail-fast: a schema the bounded validator cannot accept, a
 * repair enabled without an explicit prompt, or a repair-attempt count outside the
 * configured bounds reject the configuration instead of silently degrading. A
 * policy without a schema is disabled and the response passes through unchanged.</p>
 */
public final class AiStructuredOutputPolicy {

    /**
     * Content classification outcomes that a configured repair may regenerate.
     * Truncation and refusal are never repairable, and tool-call responses are
     * never replayed.
     */
    private static final Set<AiContentCompliance> REPAIRABLE = Set.of(
            AiContentCompliance.INVALID_JSON, AiContentCompliance.SCHEMA_MISMATCH);

    private static final ObjectMapper MAPPER = new ObjectMapper();

    /**
     * Bounded cache of compiled validators keyed by the schema text: compiling a
     * schema parses it and builds a registry, which must never run per request on
     * the event loop. Failures are not cached, so an operator fixing a schema gets
     * a fresh attempt; the cache is hard-capped so churned schemas cannot grow the
     * state without bound.
     */
    private static final Map<String, BoundedJsonSchemaValidator> VALIDATORS = new WindowTinyLFUMap<>(4, 32, false);

    private final BoundedJsonSchemaValidator validator;

    private final String schemaText;

    private final boolean repairEnabled;

    private final String repairPrompt;

    private final int repairMaxAttempts;

    private final Integer repairMaxTokens;

    private AiStructuredOutputPolicy(final BoundedJsonSchemaValidator validator, final String schemaText,
                                     final boolean repairEnabled, final String repairPrompt,
                                     final int repairMaxAttempts, final Integer repairMaxTokens) {
        this.validator = validator;
        this.schemaText = schemaText;
        this.repairEnabled = repairEnabled;
        this.repairPrompt = repairPrompt;
        this.repairMaxAttempts = repairMaxAttempts;
        this.repairMaxTokens = repairMaxTokens;
    }

    /**
     * Build the policy from operator configuration.
     *
     * @param config the structured output configuration, may be {@code null}
     * @return the policy; a disabled policy when the config is absent or schema-less
     */
    public static AiStructuredOutputPolicy of(final AiStructuredOutputConfig config) {
        if (Objects.isNull(config) || Objects.isNull(config.getSchema()) || config.getSchema().isBlank()) {
            return new AiStructuredOutputPolicy(null, null, false, null, 0, null);
        }
        JsonNode schemaNode;
        try {
            schemaNode = MAPPER.readTree(config.getSchema());
        } catch (Exception error) {
            throw new AiProxyException(AiProxyException.Kind.PROVIDER,
                    "structured output schema is not valid JSON", error);
        }
        BoundedJsonSchemaValidator validator = VALIDATORS.get(config.getSchema());
        if (Objects.isNull(validator)) {
            try {
                validator = new BoundedJsonSchemaValidator(schemaNode);
            } catch (IllegalArgumentException error) {
                throw new AiProxyException(AiProxyException.Kind.PROVIDER,
                        "structured output schema is unsupported by the bounded validator", error);
            }
            VALIDATORS.put(config.getSchema(), validator);
        }
        boolean repairEnabled = Boolean.TRUE.equals(config.getRepairEnabled());
        String repairPrompt = config.getRepairPrompt();
        if (repairEnabled && (Objects.isNull(repairPrompt) || repairPrompt.isBlank())) {
            throw new AiProxyException(AiProxyException.Kind.PROVIDER,
                    "structured output repair requires an explicit operator prompt", null);
        }
        int maxAttempts = Objects.isNull(config.getRepairMaxAttempts()) ? 1 : config.getRepairMaxAttempts();
        if (repairEnabled && (maxAttempts < 1 || maxAttempts > AiStructuredOutputConfig.MAX_REPAIR_ATTEMPTS_LIMIT)) {
            throw new AiProxyException(AiProxyException.Kind.PROVIDER,
                    "structured output repairMaxAttempts must be between 1 and "
                            + AiStructuredOutputConfig.MAX_REPAIR_ATTEMPTS_LIMIT, null);
        }
        return new AiStructuredOutputPolicy(validator, config.getSchema(), repairEnabled, repairPrompt,
                repairEnabled ? maxAttempts : 0, config.getRepairMaxTokens());
    }

    /**
     * Whether generated content is validated against an operator schema.
     *
     * @return true when validation is active
     */
    public boolean isActive() {
        return Objects.nonNull(validator);
    }

    /**
     * The bounded schema validator; only meaningful while {@link #isActive()}.
     *
     * @return the validator
     */
    public BoundedJsonSchemaValidator validator() {
        return validator;
    }

    /**
     * The operator-managed schema in its configured textual form, embedded verbatim
     * into repair prompts as the authoritative contract.
     *
     * @return the schema text
     */
    public String schemaText() {
        return schemaText;
    }

    /**
     * Whether the classified outcome may be repaired, and a repair is configured.
     *
     * @param compliance the classified outcome
     * @return true when a bounded regeneration may be attempted
     */
    public boolean canRepair(final AiContentCompliance compliance) {
        return repairEnabled && REPAIRABLE.contains(compliance);
    }

    /**
     * The operator-configured repair instruction.
     *
     * @return the repair prompt
     */
    public String repairPrompt() {
        return repairPrompt;
    }

    /**
     * Bounded regeneration budget; zero when repair is disabled.
     *
     * @return the maximum number of repair generations
     */
    public int repairMaxAttempts() {
        return repairMaxAttempts;
    }

    /**
     * Optional upper bound applied to {@code max_tokens} of a repair generation.
     *
     * @return the token bound, or {@code null}
     */
    public Integer repairMaxTokens() {
        return repairMaxTokens;
    }
}
