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

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.convert.rule.canary.CanaryConfig;
import org.apache.shenyu.common.dto.convert.rule.canary.StickyKeyConfig;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.dto.convert.rule.sticky.DeterministicKeyConfig;
import org.apache.shenyu.common.dto.convert.rule.sticky.StickySessionConfig;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import java.beans.Introspector;
import java.util.Arrays;
import java.util.Set;
import java.util.Map;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertNotNull;

class DivideRuleContractTest {

    @ParameterizedTest(name = "{0}")
    @MethodSource("fixtures")
    void canonicalValidationFactsAreShared(final String name, final String expected, final String source, final String path, final String code) {
        ConfigurationValidation result = ConfigurationContractRegistry.shared().validate("divide", ConfigurationScope.RULE, "1", source);
        assertEquals(expected, result.status().name());
        if ("INVALID".equals(expected)) {
            assertEquals(path, result.violations().get(0).path());
            assertEquals(code, result.violations().get(0).code().name());
        }
    }

    @Test
    void inventoryIncludesEveryCurrentDtoFieldAndNestedField() throws Exception {
        ConfigurationContract contract = new DivideRuleContract().contract();
        assertFields(contract, "", DivideRuleHandle.class);
        assertFields(contract, "canary.", CanaryConfig.class);
        assertFields(contract, "canary.stickyKey.", StickyKeyConfig.class);
        assertFields(contract, "canary.conditions[].", ConditionData.class);
        assertFields(contract, "sticky.", StickySessionConfig.class);
        assertFields(contract, "sticky.key.", DeterministicKeyConfig.class);
        DivideRuleHandle defaults = DivideRuleHandle.newInstance();
        assertEquals(defaults.getLoadBalance(), contract.fields().get("loadBalance").effectiveDefault());
        assertEquals(String.valueOf(defaults.getTimeout()), contract.fields().get("timeout").effectiveDefault());
        assertEquals(String.valueOf(defaults.getRetry()), contract.fields().get("retry").effectiveDefault());
        assertEquals("milliseconds", contract.fields().get("timeout").unit());
        assertEquals("3600", contract.fields().get("sticky.maxAgeSeconds").effectiveDefault());
        assertEquals("0", contract.fields().get("canary.percentage").effectiveDefault());
        assertTrue(contract.fields().get("sticky.secrets").sensitive());
        assertTrue(contract.fields().get("loadBalance").values().isEmpty(), "SPI names are open-ended");
    }

    private void assertFields(final ConfigurationContract contract, final String prefix, final Class<?> type) throws Exception {
        Set<String> expected = Arrays.stream(Introspector.getBeanInfo(type, Object.class).getPropertyDescriptors())
                .filter(property -> java.util.Objects.nonNull(property.getWriteMethod()))
                .map(property -> prefix + property.getName()).collect(Collectors.toSet());
        for (String path : expected) {
            assertTrue(contract.fields().containsKey(path), path);
            assertTrue(!contract.fields().get(path).description().isBlank(), path);
        }
        Set<String> actual = contract.fields().keySet().stream().filter(path -> path.startsWith(prefix))
                .filter(path -> !path.substring(prefix.length()).contains(".")).collect(Collectors.toSet());
        assertEquals(expected, actual);
    }

    @Test
    void existingFormProjectionKeepsControlTypesAndConfiguredPresets() throws Exception {
        Path root = Path.of("").toAbsolutePath();
        while (java.util.Objects.nonNull(root) && !Files.isDirectory(root.resolve("db/init"))) {
            root = root.getParent();
        }
        assertNotNull(root);
        final Map<String, String> controls = Map.of("loadBalance", "loadStrategy',3", "retry", "retryCount',1", "timeout", "timeout',1",
                "headerMaxSize", "headerMaxSize',1", "requestMaxSize", "requestMaxSize',1", "retryStrategy", "retryStrategy',3",
                "retryBackOffSpec", "retryBackOffSpec',3");
        for (String seed : new String[]{"db/init/mysql/schema.sql", "db/init/oracle/schema.sql", "db/init/pg/create-table.sql",
            "shenyu-admin/src/main/resources/sql-script/h2/schema.sql"}) {
            String sql = Files.readString(root.resolve(seed));
            for (Map.Entry<String, String> field : controls.entrySet()) {
                assertTrue(Arrays.stream(sql.split(";")).anyMatch(statement -> statement.toLowerCase(java.util.Locale.ROOT).contains("plugin_handle")
                        && statement.replaceAll("\\s+", "").contains("'5','" + field.getKey() + "','" + field.getValue() + ",2,")),
                        seed + " missing field/control " + field.getKey());
            }
        }
        ConfigurationContract contract = new DivideRuleContract().contract();
        assertTrue(contract.fields().get("headerMaxSize").description().contains("10240"));
        assertTrue(contract.fields().get("requestMaxSize").description().contains("102400"));
        assertEquals("0", contract.fields().get("headerMaxSize").effectiveDefault());
        assertEquals("0", contract.fields().get("requestMaxSize").effectiveDefault());
    }

    private static java.util.stream.Stream<org.junit.jupiter.params.provider.Arguments> fixtures() throws java.io.IOException {
        try (var input = java.util.Objects.requireNonNull(DivideRuleContractTest.class.getResourceAsStream(
                "/META-INF/shenyu/config-contract-fixtures/divide-rule-v1.tsv"));
             var reader = new java.io.BufferedReader(new java.io.InputStreamReader(input, java.nio.charset.StandardCharsets.UTF_8))) {
            return reader.lines().filter(line -> !line.isBlank() && !line.startsWith("#")).map(line -> {
                String[] fields = line.split("\\t", -1);
                String source = "@missing".equals(fields[4]) ? null : "@blank".equals(fields[4]) ? "" : fields[4];
                return org.junit.jupiter.params.provider.Arguments.of(fields[0], fields[1], source, fields[5], fields[6]);
            }).toList().stream();
        }
    }
}
