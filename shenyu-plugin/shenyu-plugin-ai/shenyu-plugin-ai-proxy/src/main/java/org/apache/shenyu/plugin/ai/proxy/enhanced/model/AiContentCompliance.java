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

package org.apache.shenyu.plugin.ai.proxy.enhanced.model;

/**
 * Classification of the generated content of one non-streaming chat completion
 * (issue #8257). Every value is derived from protocol fields or the bounded schema
 * validator — never from a semantic judgement — and maps to a distinct documented
 * gateway outcome.
 */
public enum AiContentCompliance {

    /**
     * The generated content parsed and satisfied the operator schema.
     */
    VALID,

    /**
     * The outer provider envelope was not a parseable chat completion.
     */
    MALFORMED_ENVELOPE,

    /**
     * The generated content field was present but not valid JSON.
     */
    INVALID_JSON,

    /**
     * The generated content was valid JSON but violated the operator schema.
     */
    SCHEMA_MISMATCH,

    /**
     * The generated content exceeded the bounded validator's size or complexity
     * budget; it may be perfectly valid, just too large to validate, so it is
     * rejected without a repair regeneration.
     */
    SCHEMA_BUDGET_EXCEEDED,

    /**
     * The generated content field was absent or blank.
     */
    EMPTY,

    /**
     * The upstream declared truncation ({@code finish_reason: length}); a repaired
     * regeneration of a truncated answer is out of scope.
     */
    TRUNCATED,

    /**
     * The upstream declared a refusal ({@code finish_reason: content_filter});
     * a refusal is never a formatting error to repair.
     */
    REFUSAL,

    /**
     * The response carries tool calls; validation is skipped and repair is
     * forbidden — no effectful operation is ever replayed or regenerated.
     */
    TOOL_CALLS_PRESENT
}
