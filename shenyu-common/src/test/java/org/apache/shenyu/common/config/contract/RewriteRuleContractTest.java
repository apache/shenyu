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

import org.apache.shenyu.common.dto.convert.rule.RewriteHandle;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import java.beans.Introspector;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Set;
import java.util.Map;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertNotNull;

class RewriteRuleContractTest {

    @ParameterizedTest(name = "{0}")
    @MethodSource("fixtures")
    void sharedValidatorPreservesLegacyAcceptance(final String name, final boolean accepted, final String source) {
        ConfigurationValidation result = ConfigurationContractRegistry.shared().validate("rewrite", ConfigurationScope.RULE, "1", source);
        assertEquals(accepted, result.status() == ConfigurationValidation.Status.VALID);
        assertFalse(result.toString().contains("SECRET"));
    }

    @Test
    void metadataMatchesDtoAndMaintainedFormSeeds() throws Exception {
        ConfigurationContract contract = new RewriteRuleContract().contract();
        Set<String> properties = Arrays.stream(Introspector.getBeanInfo(RewriteHandle.class, Object.class).getPropertyDescriptors())
                .map(property -> property.getName()).collect(Collectors.toSet());
        assertEquals(properties, contract.fields().keySet());
        assertEquals("100", contract.fields().get("percentage").effectiveDefault());
        assertEquals("false", contract.fields().get("rewriteMetaData").effectiveDefault());
        assertFalse(contract.structuralValidation());
        assertEquals(ConfigurationContract.UnknownFields.ALLOW, contract.unknownFields());
        assertEquals("retain", contract.operations().get("missingUpdate"));
        Path root = Path.of("").toAbsolutePath();
        while (java.util.Objects.nonNull(root) && !Files.isDirectory(root.resolve("db/init"))) {
            root = root.getParent();
        }
        assertNotNull(root, "Repository form seeds must be available for this metadata check");
        final Map<String, Integer> controls = Map.of("regex", 2, "replace", 2, "percentage", 1, "rewriteMetaData", 3);
        for (String seed : new String[]{"db/init/mysql/schema.sql", "db/init/oracle/schema.sql", "db/init/pg/create-table.sql",
            "shenyu-admin/src/main/resources/sql-script/h2/schema.sql"}) {
            String sql = Files.readString(root.resolve(seed));
            for (String field : properties) {
                assertTrue(Arrays.stream(sql.split(";")).anyMatch(line -> line.toLowerCase(java.util.Locale.ROOT).contains("plugin_handle")
                        && line.replaceAll("\\s+", "").contains("'3','" + field + "','" + field + "'," + controls.get(field) + ",")),
                        seed + " missing field/control " + field);
                assertFalse(contract.fields().get(field).description().isBlank());
            }
        }
    }

    private static java.util.stream.Stream<org.junit.jupiter.params.provider.Arguments> fixtures() throws java.io.IOException {
        try (var input = java.util.Objects.requireNonNull(RewriteRuleContractTest.class.getResourceAsStream(
                "/META-INF/shenyu/config-contract-fixtures/rewrite-rule-v1.tsv"));
             var reader = new java.io.BufferedReader(new java.io.InputStreamReader(input, java.nio.charset.StandardCharsets.UTF_8))) {
            return reader.lines().filter(line -> !line.isBlank() && !line.startsWith("#")).map(line -> {
                String[] fields = line.split("\\t", -1);
                String source = "@missing".equals(fields[3]) ? null : "@blank".equals(fields[3]) ? "" : fields[3];
                return org.junit.jupiter.params.provider.Arguments.of(fields[0], Boolean.parseBoolean(fields[1]), source);
            }).toList().stream();
        }
    }
}
