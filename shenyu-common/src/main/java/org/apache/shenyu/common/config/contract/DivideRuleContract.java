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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.convert.rule.canary.CanaryConfigValidator;

import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import static org.apache.shenyu.common.config.contract.ConfigurationContract.Type.ARRAY;
import static org.apache.shenyu.common.config.contract.ConfigurationContract.Type.BOOLEAN;
import static org.apache.shenyu.common.config.contract.ConfigurationContract.Type.INTEGER;
import static org.apache.shenyu.common.config.contract.ConfigurationContract.Type.OBJECT;
import static org.apache.shenyu.common.config.contract.ConfigurationContract.Type.STRING;

/** Version-one inventory and existing pure semantic validation for Divide rule handles. */
public final class DivideRuleContract implements ConfigurationContractProvider {

    private static final ConfigurationContract CONTRACT = new ConfigurationContract("divide", ConfigurationScope.RULE, "1", false,
            ConfigurationContract.UnknownFields.ALLOW, fields(),
            Map.of("missingUpdate", "retain", "blankAdmin", "accepted legacy input", "blankGatewayUpdate", "parse failure; retain",
                    "invalidUpdate", "reject; retain", "remove", "only non-null input handles remove the cached handle",
                    "disable", "matching/lifecycle owned; no universal cache clearing", "defaults", "descriptive only; never persisted"));

    @Override
    public ConfigurationContract contract() {
        return CONTRACT;
    }

    @Override
    public List<ConfigurationViolation> validateSemantic(final String source) {
        // Admin historically permits blank handles; the gateway's parser still rejects blank updates.
        if (StringUtils.isBlank(source)) {
            return List.of();
        }
        try {
            CanaryConfigValidator.parseHandle(source);
            return List.of();
        } catch (IllegalArgumentException error) {
            String message = Objects.toString(error.getMessage(), "");
            String normalized = message.replaceAll("\\[[0-9]+\\]", "[]");
            String path = CONTRACT.fields().keySet().stream().filter(normalized::startsWith)
                    .max(Comparator.comparingInt(String::length)).orElse(message.startsWith("Canary ") ? "canary" : "$");
            ConfigurationViolation.Code code = ConfigurationViolation.Code.SEMANTIC_INVALID;
            if (message.startsWith("Invalid Divide handle JSON")) {
                code = ConfigurationViolation.Code.INVALID_JSON;
            } else if (message.contains("must be an integer") || message.contains("must be a boolean")
                    || message.contains("must be an object") || message.contains("must be an array") || message.contains("must be a string")) {
                code = ConfigurationViolation.Code.INVALID_TYPE;
            } else if (message.contains("must be between")) {
                code = ConfigurationViolation.Code.OUT_OF_RANGE;
            }
            return List.of(new ConfigurationViolation(path, code));
        }
    }

    private static Map<String, ConfigurationContract.Field> fields() {
        Map<String, ConfigurationContract.Field> fields = new LinkedHashMap<>();
        add(fields, "loadBalance", STRING, "random", "", false, "SPI name; custom providers are not a closed built-in enum");
        add(fields, "retryStrategy", STRING, "current", "", false, "Legacy retry strategy name; current/failover interpretation is unchanged");
        add(fields, "retryBackOffSpec", STRING, "default", "", false, "default/fixed/exponential; unknown names retain legacy fallback");
        add(fields, "retry", INTEGER, "3", "attempt retries", false, "Existing retry count; no new root-field range policy");
        add(fields, "timeout", INTEGER, "3000", "milliseconds", false, "Upstream timeout; inherited runtime interpretation is unchanged");
        add(fields, "headerMaxSize", INTEGER, "0", "bytes", false, "UTF-8 plus framing allowance; non-positive disables checking; legacy form preset is 10240");
        add(fields, "requestMaxSize", INTEGER, "0", "bytes", false, "Non-positive disables checking; legacy form preset is 102400");
        add(fields, "canary", OBJECT, null, "", false, "Absent/null uses legacy routing; a present object must satisfy the existing partition validator");
        add(fields, "canary.enabled", BOOLEAN, "false", "", false, "False selects Stable; present values must be JSON booleans");
        add(fields, "canary.conditions", ARRAY, null, "", true, "Optional request predicates; explicit null is invalid; an empty list admits all requests");
        add(fields, "canary.conditions[].paramType", STRING, null, "", false, "ParameterData SPI name; dictionary and installed-provider checks are separate");
        add(fields, "canary.conditions[].operator", STRING, null, "", false, "PredicateJudge SPI name, including custom extensions");
        add(fields, "canary.conditions[].paramName", STRING, null, "", true, "Required for named header/cookie/query/post sources");
        add(fields, "canary.conditions[].paramValue", STRING, null, "", true, "Required except isBlank; existing regex/time semantics apply");
        add(fields, "canary.matchMode", INTEGER, "0", "", false, "0 AND or 1 OR; explicit null and other values are invalid");
        add(fields, "canary.percentage", INTEGER, "0", "percent", false, "Existing closed range 0..100; no fractional or string coercion");
        add(fields, "canary.stickyKey", OBJECT, null, "", true, "Required when enabled with percentage 1..99; explicit null is invalid");
        add(fields, "canary.stickyKey.paramType", STRING, null, "", false, "ParameterData SPI name; availability is a binding fact");
        add(fields, "canary.stickyKey.paramName", STRING, null, "", true, "Source-specific name; required for named built-in sources");
        add(fields, "canary.canaryLabels", OBJECT, null, "", true, "Required nonempty string labels for a present Canary object");
        add(fields, "canary.stableLabels", OBJECT, null, "", true, "Required labels; partitions must share a key with different values");
        add(fields, "canary.fallbackPolicy", STRING, "STABLE", "", false, "STABLE or REJECT for an initially empty Canary pool");
        add(fields, "sticky", OBJECT, null, "", true, "Optional HTTP affinity; parser acceptance is not proof that affinity can activate");
        add(fields, "sticky.enabled", BOOLEAN, "false", "", false, "Activation also requires usable secrets, cookie name and positive lifetime");
        add(fields, "sticky.cookieName", STRING, "Shenyu-Route", "", false, "Default base name receives a rule suffix; custom names remain shared verbatim");
        add(fields, "sticky.maxAgeSeconds", INTEGER, "3600", "seconds", false, "Binding lifetime; existing affinity activation handles unusable values");
        add(fields, "sticky.secrets", ARRAY, null, "", true, "First HMAC key signs; all keys verify; never expose configured values");
        add(fields, "sticky.onUnavailable", STRING, "REBIND", "", false, "Existing unavailable-target policy; no new parser rejection");
        add(fields, "sticky.key", OBJECT, null, "", true, "Optional deterministic key; effective only while enclosing affinity is active");
        add(fields, "sticky.key.type", STRING, "HEADER", "", false, "Current DTO enum HEADER/COOKIE; existing unknown/null handling is retained");
        add(fields, "sticky.key.name", STRING, null, "", true, "Header or cookie name; request values never belong in metadata");
        return fields;
    }

    private static void add(final Map<String, ConfigurationContract.Field> fields, final String path, final ConfigurationContract.Type type,
                            final String defaultValue, final String unit, final boolean sensitive, final String description) {
        Long minimum = "canary.percentage".equals(path) || "canary.matchMode".equals(path) ? 0L : null;
        Long maximum = null;
        if ("canary.percentage".equals(path)) {
            maximum = 100L;
        } else if ("canary.matchMode".equals(path)) {
            maximum = 1L;
        }
        List<String> values = "canary.fallbackPolicy".equals(path) ? List.of("STABLE", "REJECT") : List.of();
        fields.put(path, new ConfigurationContract.Field(type, false, !path.startsWith("canary."), defaultValue, unit, sensitive,
                minimum, maximum, values, description));
    }
}
