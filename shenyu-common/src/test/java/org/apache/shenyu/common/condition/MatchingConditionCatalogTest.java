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

package org.apache.shenyu.common.condition;

import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests for {@link MatchingConditionCatalog}.
 */
final class MatchingConditionCatalogTest {

    @Test
    void shouldCoverSupportedEnumsExactlyOnceInDeclarationOrder() {
        List<String> expectedSources = ParamTypeEnum.acquireSupport().stream()
                .map(ParamTypeEnum::getName).collect(Collectors.toList());
        List<String> expectedOperators = OperatorEnum.acquireSupport().stream()
                .map(OperatorEnum::getAlias).collect(Collectors.toList());
        List<String> actualSources = MatchingConditionCatalog.sources().stream()
                .map(MatchingConditionSource::id).collect(Collectors.toList());
        List<String> actualOperators = MatchingConditionCatalog.operators().stream()
                .map(MatchingConditionOperator::id).collect(Collectors.toList());

        assertEquals(expectedSources, actualSources);
        assertEquals(expectedOperators, actualOperators);
        assertEquals(actualSources.size(), Set.copyOf(actualSources).size());
        assertEquals(actualOperators.size(), Set.copyOf(actualOperators).size());
        assertEquals("1", MatchingConditionCatalog.version());
    }

    @Test
    void shouldReferenceOnlyCatalogedOperators() {
        Set<String> operatorIds = MatchingConditionCatalog.operators().stream()
                .map(MatchingConditionOperator::id).collect(Collectors.toSet());
        MatchingConditionCatalog.sources().forEach(source -> {
            assertTrue(operatorIds.containsAll(source.operatorIds()));
            assertEquals(source.operatorIds().size(), MatchingConditionCatalog.operatorsFor(source.id()).size());
            source.examples().forEach(example -> {
                assertEquals(source.id(), example.sourceId());
                assertTrue(source.operatorIds().contains(example.operatorId()));
            });
        });
        assertTrue(MatchingConditionCatalog.operatorsFor("custom-source").isEmpty());
    }

    @Test
    void shouldDescribeDeclaredButUnregisteredSources() {
        MatchingConditionSource path = MatchingConditionCatalog.source(ParamTypeEnum.PATH.getName()).orElseThrow();
        MatchingConditionSource formData = MatchingConditionCatalog.source(ParamTypeEnum.FORM_DATA.getName()).orElseThrow();

        assertEquals(MatchingConditionStatus.DECLARED_NOT_REGISTERED, path.status());
        assertEquals(MatchingConditionStatus.DECLARED_NOT_REGISTERED, formData.status());
        assertFalse(path.runtimeRegistered());
        assertFalse(formData.runtimeRegistered());
        assertFalse(path.paramNameRequired());
        assertFalse(formData.paramNameRequired());
    }

    @Test
    void shouldUseStableKeysAndOperatorFormats() {
        final MatchingConditionSource uri = MatchingConditionCatalog.source("uri").orElseThrow();
        final MatchingConditionOperator equals = MatchingConditionCatalog.operator("=").orElseThrow();
        final MatchingConditionOperator isBlank = MatchingConditionCatalog.operator("isBlank").orElseThrow();

        assertEquals("matching.condition.source.uri.display", uri.displayKey());
        assertEquals("matching.condition.source.uri.help", uri.helpKey());
        assertEquals("matching.condition.operator.eq.display", equals.displayKey());
        assertEquals("matching.condition.operator.eq.help", equals.helpKey());
        assertEquals(MatchingConditionValueFormat.TEXT, equals.valueFormat());
        assertEquals(MatchingConditionValueFormat.TEXT,
                MatchingConditionCatalog.operator("startsWith").orElseThrow().valueFormat());
        assertFalse(isBlank.requiresValue());
        assertEquals(MatchingConditionValueFormat.NONE, isBlank.valueFormat());
    }

    @Test
    void shouldExposeSourceSpecificOperatorApplicability() {
        final MatchingConditionSource path = MatchingConditionCatalog.source("path").orElseThrow();
        final MatchingConditionSource formData = MatchingConditionCatalog.source("form-data").orElseThrow();
        final MatchingConditionSource post = MatchingConditionCatalog.source("post").orElseThrow();
        final MatchingConditionSource uri = MatchingConditionCatalog.source("uri").orElseThrow();

        assertTrue(path.operatorIds().contains("pathPattern"));
        assertTrue(uri.operatorIds().contains("pathPattern"));
        assertFalse(formData.operatorIds().contains("pathPattern"));
        assertFalse(uri.operatorIds().contains("TimeBefore"));
        assertTrue(post.operatorIds().containsAll(List.of("TimeBefore", "TimeAfter")));
    }

    @Test
    void shouldExposeOnlyImmutableCollections() {
        MatchingConditionSource uri = MatchingConditionCatalog.source("uri").orElseThrow();

        assertThrows(UnsupportedOperationException.class, () -> MatchingConditionCatalog.sources().clear());
        assertThrows(UnsupportedOperationException.class, () -> MatchingConditionCatalog.operators().clear());
        assertThrows(UnsupportedOperationException.class, () -> uri.operatorIds().clear());
        assertThrows(UnsupportedOperationException.class, () -> uri.examples().clear());
        assertThrows(UnsupportedOperationException.class, () -> MatchingConditionCatalog.operatorsFor("uri").clear());
    }
}
