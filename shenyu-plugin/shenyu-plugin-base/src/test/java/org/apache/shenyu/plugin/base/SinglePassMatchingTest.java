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


package org.apache.shenyu.plugin.base;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.plugin.base.condition.strategy.MatchStrategyFactory;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.server.ServerWebExchange;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Random;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;

class SinglePassMatchingTest {

    @Test
    void preservesSelectionAndCacheEligibilityAcrossCandidateOrderings() {
        AbstractShenyuPlugin plugin = mock(AbstractShenyuPlugin.class, CALLS_REAL_METHODS);
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/test"));
        Random random = new Random(6579);
        try (MockedStatic<MatchStrategyFactory> matches = mockStatic(MatchStrategyFactory.class)) {
            matches.when(() -> MatchStrategyFactory.match(anyInt(), any(), any())).thenReturn(true);
            for (int round = 0; round < 100; round++) {
                List<SelectorData> selectors = new ArrayList<>();
                List<RuleData> rules = new ArrayList<>();
                for (int i = 0; i < round % 10; i++) {
                    int mode = random.nextInt(2);
                    int sort = random.nextInt(4);
                    boolean enabled = random.nextBoolean();
                    List<ConditionData> conditions = Collections.nCopies(random.nextInt(4), new ConditionData());
                    selectors.add(SelectorData.builder().id(String.valueOf(i)).enabled(enabled).matchMode(mode).sort(sort)
                            .type(SelectorTypeEnum.FULL_FLOW.getCode()).conditionList(conditions).build());
                    rules.add(RuleData.builder().id(String.valueOf(i)).enabled(enabled).matchMode(mode).sort(sort).conditionDataList(conditions).build());
                }
                selectors.addAll(new ArrayList<>(selectors));
                rules.addAll(new ArrayList<>(rules));
                Collections.shuffle(selectors, random);
                Collections.shuffle(rules, random);
                List<SelectorData> expectedSelectors = selectors.stream().filter(SelectorData::getEnabled).distinct().collect(Collectors.toList());
                SelectorData expectedSelector = expectedSelectors.stream().min(Comparator
                        .comparingInt((SelectorData selector) -> selector.getMatchMode() == 0 ? -selector.getConditionList().size() : 0)
                        .thenComparing(SelectorData::getSort)).orElse(null);
                Pair<Boolean, SelectorData> selectorResult = ReflectionTestUtils.invokeMethod(plugin, "matchSelector", exchange, selectors);
                assertSame(expectedSelector, selectorResult.getRight());
                assertEquals(expectedSelectors.size() <= 1, selectorResult.getLeft());
                List<RuleData> expectedRules = rules.stream().filter(RuleData::getEnabled).distinct().collect(Collectors.toList());
                RuleData expectedRule = expectedRules.stream().min(Comparator
                        .comparingInt((RuleData rule) -> rule.getMatchMode() == 0 ? -rule.getConditionDataList().size() : 0)
                        .thenComparing(RuleData::getSort)).orElse(null);
                Pair<Boolean, RuleData> ruleResult = ReflectionTestUtils.invokeMethod(plugin, "matchRule", exchange, rules);
                assertSame(expectedRule, ruleResult.getRight());
                assertEquals(expectedRules.size() <= 1, ruleResult.getLeft());
            }
        }
    }
}
