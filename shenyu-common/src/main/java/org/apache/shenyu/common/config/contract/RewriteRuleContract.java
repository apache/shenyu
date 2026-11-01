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

package org.apache.shenyu.common.config.contract;

import com.google.gson.JsonSyntaxException;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.convert.rule.RewriteHandle;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.RewriteHandleUtils;

import java.util.List;
import java.util.Map;
import java.util.Objects;

/** Version-one metadata and existing applicability rules for rewrite rule handles. */
public final class RewriteRuleContract implements ConfigurationContractProvider {

    private static final ConfigurationContract CONTRACT = new ConfigurationContract("rewrite", ConfigurationScope.RULE, "1", false,
            ConfigurationContract.UnknownFields.ALLOW,
            Map.of("regex", field(ConfigurationContract.Type.STRING, null, "", true, "Java regex, or the existing brace-based placeholder expression"),
                    "replace", field(ConfigurationContract.Type.STRING, null, "", true, "Replacement template; incomplete expressions are no-ops"),
                    "percentage", field(ConfigurationContract.Type.INTEGER, "100", "percent", false, "Legacy threshold; values are neither clamped nor range-rejected"),
                    "rewriteMetaData", field(ConfigurationContract.Type.BOOLEAN, "false", "", false, "Look up metadata for the rewritten URI when true")),
            Map.of("missingUpdate", "retain", "blankUpdate", "clear", "emptyObjectUpdate", "replace with no-op",
                    "explicitNullUpdate", "evict", "malformedJsonUpdate", "throw; retain",
                    "invalidApplicableExpressionUpdate", "evict", "remove", "idempotent; handle not required"));

    @Override
    public ConfigurationContract contract() {
        return CONTRACT;
    }

    @Override
    public List<ConfigurationViolation> validateSemantic(final String source) {
        if (StringUtils.isBlank(source)) {
            return List.of();
        }
        final RewriteHandle handle;
        try {
            handle = GsonUtils.getInstance().fromJson(source, RewriteHandle.class);
        } catch (JsonSyntaxException error) {
            return List.of(new ConfigurationViolation("$", ConfigurationViolation.Code.INVALID_JSON));
        }
        if (Objects.isNull(handle)) {
            return List.of(new ConfigurationViolation("$", ConfigurationViolation.Code.INVALID_JSON));
        }
        String problem = RewriteHandleUtils.applicabilityProblem(handle.getRegex(), handle.getReplace());
        if (Objects.isNull(problem)) {
            return List.of();
        }
        String path = problem.startsWith("invalid regular expression:") ? "regex" : "replace";
        return List.of(new ConfigurationViolation(path, ConfigurationViolation.Code.SEMANTIC_INVALID));
    }

    private static ConfigurationContract.Field field(final ConfigurationContract.Type type, final String defaultValue,
                                                     final String unit, final boolean sensitive, final String description) {
        return new ConfigurationContract.Field(type, false, true, defaultValue, unit, sensitive, null, null, List.of(), description);
    }
}
