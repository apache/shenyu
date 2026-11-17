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

import java.io.Serializable;

/**
 * Operator-managed structured output policy for one AI selector (issue #8257).
 *
 * <p>Both switches are opt-in: without a non-blank {@code schema} nothing is
 * validated and responses pass through byte for byte; without {@code repairEnabled}
 * a failed validation rejects the response instead of regenerating it. The schema
 * is validated against a bounded Draft 2020-12 dialect at use time — unsupported
 * or oversized schemas fail fast, never silently degrade.</p>
 */
public class AiStructuredOutputConfig implements Serializable {

    /**
     * Maximum regeneration attempts after a failed validation; bounded from above
     * so a broken schema cannot multiply upstream spend.
     */
    public static final int MAX_REPAIR_ATTEMPTS_LIMIT = 3;

    /**
     * Conservative default token bound of one repair generation (issue #8257
     * requires an explicit conservative default); an operator raises it by
     * configuring {@code repairMaxTokens} explicitly.
     */
    public static final int REPAIR_DEFAULT_MAX_TOKENS = 1024;

    private static final long serialVersionUID = -4216685160670534827L;

    /**
     * Operator-managed JSON Schema (Draft 2020-12, bounded dialect).
     */
    private String schema;

    private Boolean repairEnabled = false;

    private String repairPrompt;

    private Integer repairMaxAttempts = 1;

    private Integer repairMaxTokens = REPAIR_DEFAULT_MAX_TOKENS;

    public String getSchema() {
        return schema;
    }

    public void setSchema(final String schema) {
        this.schema = schema;
    }

    public Boolean getRepairEnabled() {
        return repairEnabled;
    }

    public void setRepairEnabled(final Boolean repairEnabled) {
        this.repairEnabled = repairEnabled;
    }

    public String getRepairPrompt() {
        return repairPrompt;
    }

    public void setRepairPrompt(final String repairPrompt) {
        this.repairPrompt = repairPrompt;
    }

    public Integer getRepairMaxAttempts() {
        return repairMaxAttempts;
    }

    public void setRepairMaxAttempts(final Integer repairMaxAttempts) {
        this.repairMaxAttempts = repairMaxAttempts;
    }

    public Integer getRepairMaxTokens() {
        return repairMaxTokens;
    }

    public void setRepairMaxTokens(final Integer repairMaxTokens) {
        this.repairMaxTokens = repairMaxTokens;
    }
}
