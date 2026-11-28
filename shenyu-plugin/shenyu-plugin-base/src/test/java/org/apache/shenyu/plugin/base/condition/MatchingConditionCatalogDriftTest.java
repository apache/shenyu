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

package org.apache.shenyu.plugin.base.condition;

import org.apache.shenyu.common.condition.MatchingConditionCatalog;
import org.apache.shenyu.common.condition.MatchingConditionOperator;
import org.apache.shenyu.common.condition.MatchingConditionSource;
import org.apache.shenyu.common.condition.MatchingConditionStatus;
import org.apache.shenyu.plugin.base.condition.data.ParameterData;
import org.apache.shenyu.plugin.base.condition.judge.PredicateJudge;
import org.apache.shenyu.spi.ExtensionLoader;
import org.junit.jupiter.api.Test;

import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

/**
 * Detects drift between matching-condition descriptors and gateway SPI registrations.
 */
final class MatchingConditionCatalogDriftTest {

    @Test
    void shouldMatchParameterDataRegistrations() {
        Set<String> registeredKeys = ExtensionLoader.getExtensionLoader(ParameterData.class)
                .getExtensionClasses1().keySet();
        Set<String> catalogKeys = MatchingConditionCatalog.sources().stream()
                .filter(MatchingConditionSource::runtimeRegistered)
                .map(MatchingConditionSource::id).collect(Collectors.toSet());

        assertEquals(catalogKeys, registeredKeys);
        assertDeclaredButUnregistered("path", registeredKeys);
        assertDeclaredButUnregistered("form-data", registeredKeys);
    }

    @Test
    void shouldMatchPredicateJudgeRegistrationsIncludingEqualsAlias() {
        Set<String> registeredKeys = ExtensionLoader.getExtensionLoader(PredicateJudge.class)
                .getExtensionClasses1().keySet();
        Set<String> catalogKeys = MatchingConditionCatalog.operators().stream()
                .map(MatchingConditionCatalogDriftTest::judgeKey).collect(Collectors.toSet());

        assertEquals(catalogKeys, registeredKeys);
    }

    private static void assertDeclaredButUnregistered(final String sourceId, final Set<String> registeredKeys) {
        MatchingConditionSource source = MatchingConditionCatalog.source(sourceId).orElseThrow();
        assertFalse(registeredKeys.contains(sourceId));
        assertFalse(source.runtimeRegistered());
        assertEquals(MatchingConditionStatus.DECLARED_NOT_REGISTERED, source.status());
    }

    private static String judgeKey(final MatchingConditionOperator operator) {
        return "=".equals(operator.id()) ? "equals" : operator.id();
    }
}
