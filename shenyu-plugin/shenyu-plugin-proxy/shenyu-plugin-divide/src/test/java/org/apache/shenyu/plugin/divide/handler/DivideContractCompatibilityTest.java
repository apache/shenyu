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

package org.apache.shenyu.plugin.divide.handler;

import org.apache.shenyu.common.config.contract.ConfigurationValidationException;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.rule.canary.CanaryConfigValidator;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

class DivideContractCompatibilityTest {

    @ParameterizedTest(name = "{0}")
    @MethodSource("fixtures")
    void canonicalFixturesPreserveCandidateAndRemovalOwnership(final String name, final String action, final String source,
                                                               final String path, final String code) {
        final DividePluginDataHandler handler = new DividePluginDataHandler();
        RuleData rule = new RuleData();
        rule.setSelectorId("contract-divide");
        rule.setId(name);
        rule.setHandle("{\"timeout\":5000}");
        handler.handlerRule(rule);
        String key = CacheKeyUtils.INST.getKey(rule);
        final DivideRuleHandle previous = DividePluginDataHandler.CACHED_HANDLE.get().obtainHandle(key);
        rule.setHandle(source);
        try {
            if ("REJECT_RETAIN".equals(action) || "BINDING_RETAIN".equals(action)) {
                ConfigurationValidationException error = assertThrows(ConfigurationValidationException.class, () -> handler.handlerRule(rule));
                assertEquals(path, error.validation().violations().get(0).path());
                assertEquals(code, error.validation().violations().get(0).code().name());
                assertFalse(error.getMessage().contains("contract-missing"));
            } else if ("THROW_RETAIN".equals(action)) {
                assertThrows(IllegalArgumentException.class, () -> handler.handlerRule(rule));
            } else {
                handler.handlerRule(rule);
            }
            final DivideRuleHandle active = DividePluginDataHandler.CACHED_HANDLE.get().obtainHandle(key);
            assertNotNull(active);
            if ("REPLACE".equals(action)) {
                assertNotSame(previous, active);
                assertEquals(GsonUtils.getInstance().toJson(CanaryConfigValidator.parseHandle(source)), GsonUtils.getInstance().toJson(active));
            } else {
                assertSame(previous, active);
            }
            // Characterize the existing conditional removal; schema adoption must not strengthen it silently.
            rule.setHandle(null);
            handler.removeRule(rule);
            assertSame(active, DividePluginDataHandler.CACHED_HANDLE.get().obtainHandle(key));
            rule.setHandle("{}");
            handler.removeRule(rule);
            handler.removeRule(rule);
            assertNull(DividePluginDataHandler.CACHED_HANDLE.get().obtainHandle(key));
        } finally {
            rule.setHandle("{}");
            handler.removeRule(rule);
        }
    }

    private static java.util.stream.Stream<org.junit.jupiter.params.provider.Arguments> fixtures() throws java.io.IOException {
        try (var input = java.util.Objects.requireNonNull(DivideContractCompatibilityTest.class.getResourceAsStream(
                "/META-INF/shenyu/config-contract-fixtures/divide-rule-v1.tsv"));
             var reader = new java.io.BufferedReader(new java.io.InputStreamReader(input, java.nio.charset.StandardCharsets.UTF_8))) {
            return reader.lines().filter(line -> !line.isBlank() && !line.startsWith("#")).map(line -> {
                String[] fields = line.split("\\t", -1);
                String source = "@missing".equals(fields[4]) ? null : "@blank".equals(fields[4]) ? "" : fields[4];
                return org.junit.jupiter.params.provider.Arguments.of(fields[0], fields[3], source, fields[5], fields[6]);
            }).toList().stream();
        }
    }
}
