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

package org.apache.shenyu.admin.service;

import org.apache.shenyu.admin.model.vo.EnumVO;
import org.apache.shenyu.admin.service.impl.EnumServiceImpl;
import org.apache.shenyu.common.condition.MatchingConditionCatalog;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Verifies that the Admin enum dictionary and shared matching-condition catalog stay consistent.
 */
final class MatchingConditionDictionaryConsistencyTest {

    @Test
    void shouldMatchCatalogIdentifiers() {
        Map<String, List<EnumVO>> dictionary = new EnumServiceImpl().list();
        Set<String> operatorIds = dictionary.get("operatorEnums").stream().map(EnumVO::getName).collect(Collectors.toSet());
        Set<String> sourceIds = dictionary.get("paramTypeEnums").stream().map(EnumVO::getName).collect(Collectors.toSet());
        Set<String> catalogOperatorIds = MatchingConditionCatalog.operators().stream()
                .map(operator -> operator.id()).collect(Collectors.toSet());
        Set<String> catalogSourceIds = MatchingConditionCatalog.sources().stream()
                .map(source -> source.id()).collect(Collectors.toSet());

        assertEquals(catalogOperatorIds, operatorIds);
        assertEquals(catalogSourceIds, sourceIds);
    }
}
