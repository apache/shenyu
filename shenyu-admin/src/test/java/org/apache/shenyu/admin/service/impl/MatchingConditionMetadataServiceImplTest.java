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

package org.apache.shenyu.admin.service.impl;

import org.apache.shenyu.admin.model.vo.MatchingConditionCatalogVO;
import org.apache.shenyu.admin.model.vo.MatchingConditionExampleVO;
import org.apache.shenyu.admin.model.vo.MatchingConditionOperatorVO;
import org.apache.shenyu.admin.model.vo.MatchingConditionSourceVO;
import org.apache.shenyu.common.condition.MatchingConditionCatalog;
import org.apache.shenyu.common.condition.MatchingConditionExample;
import org.apache.shenyu.common.condition.MatchingConditionOperator;
import org.apache.shenyu.common.condition.MatchingConditionSource;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Tests for {@link MatchingConditionMetadataServiceImpl}.
 */
final class MatchingConditionMetadataServiceImplTest {

    @Test
    void shouldMapEveryCatalogFieldToViews() {
        MatchingConditionCatalogVO view = new MatchingConditionMetadataServiceImpl().catalog();

        assertEquals(MatchingConditionCatalog.version(), view.version());
        assertEquals(MatchingConditionCatalog.sources().size(), view.sources().size());
        assertEquals(MatchingConditionCatalog.operators().size(), view.operators().size());
        for (int index = 0; index < view.sources().size(); index++) {
            assertSource(MatchingConditionCatalog.sources().get(index), view.sources().get(index));
        }
        for (int index = 0; index < view.operators().size(); index++) {
            assertOperator(MatchingConditionCatalog.operators().get(index), view.operators().get(index));
        }
    }

    private static void assertSource(final MatchingConditionSource source, final MatchingConditionSourceVO view) {
        assertEquals(source.id(), view.id());
        assertEquals(source.displayKey(), view.displayKey());
        assertEquals(source.helpKey(), view.helpKey());
        assertEquals(source.status(), view.status());
        assertEquals(source.valueType(), view.valueType());
        assertEquals(source.paramNameRequired(), view.paramNameRequired());
        assertEquals(source.runtimeRegistered(), view.runtimeRegistered());
        assertEquals(source.operatorIds(), view.operatorIds());
        assertEquals(source.examples().size(), view.examples().size());
        for (int index = 0; index < source.examples().size(); index++) {
            assertExample(source.examples().get(index), view.examples().get(index));
        }
    }

    private static void assertOperator(final MatchingConditionOperator operator, final MatchingConditionOperatorVO view) {
        assertEquals(operator.id(), view.id());
        assertEquals(operator.displayKey(), view.displayKey());
        assertEquals(operator.helpKey(), view.helpKey());
        assertEquals(operator.status(), view.status());
        assertEquals(operator.requiresValue(), view.requiresValue());
        assertEquals(operator.valueFormat(), view.valueFormat());
    }

    private static void assertExample(final MatchingConditionExample example, final MatchingConditionExampleVO view) {
        assertEquals(example.sourceId(), view.sourceId());
        assertEquals(example.operatorId(), view.operatorId());
        assertEquals(example.paramName(), view.paramName());
        assertEquals(example.paramValue(), view.paramValue());
        assertEquals(example.descriptionKey(), view.descriptionKey());
    }
}
