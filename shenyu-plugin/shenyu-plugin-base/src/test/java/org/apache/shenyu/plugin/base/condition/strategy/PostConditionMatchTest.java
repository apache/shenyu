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

package org.apache.shenyu.plugin.base.condition.strategy;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests post conditions through parameter extraction and predicate evaluation.
 */
public final class PostConditionMatchTest {

    private ServerWebExchange exchange;

    private ShenyuContext context;

    @BeforeEach
    public void setUp() {
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/http/test"));
        context = new ShenyuContext();
        context.setHttpMethod("get");
        context.setStartDateTime(LocalDateTime.of(2026, 11, 6, 12, 34, 56, 123456789));
        exchange.getAttributes().put(Constants.CONTEXT, context);
    }

    @ParameterizedTest
    @CsvSource({"=, 2026-11-06 12:34:56, true", "=, 2026-11-06 12:34:57, false",
        "TimeBefore, 2026-11-06 12:34:55, false", "TimeBefore, 2026-11-06 12:34:56, false", "TimeBefore, 2026-11-06 12:34:57, true",
        "TimeAfter, 2026-11-06 12:34:55, true", "TimeAfter, 2026-11-06 12:34:56, false", "TimeAfter, 2026-11-06 12:34:57, false",
        "isBlank, ignored, false"})
    public void testStartDateTime(final String operator, final String paramValue, final boolean expected) {
        assertMatch(condition("startDateTime", operator, paramValue), expected);
    }

    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {" ", "unknown", "appKey", "serialVersionUID"})
    public void testMissingValue(final String paramName) {
        assertMatch(condition(paramName, "=", "8668695964617280718"), false);
        assertMatch(condition(paramName, "TimeBefore", "2026-11-06 12:34:57"), false);
        assertMatch(condition(paramName, "isBlank", ""), true);
    }

    @Test
    public void testNullStartDateTime() {
        context.setStartDateTime(null);

        assertMatch(condition("startDateTime", "TimeBefore", "2026-11-06 12:34:57"), false);
        assertMatch(condition("startDateTime", "isBlank", ""), true);
    }

    @Test
    public void testStringValue() {
        assertMatch(condition("httpMethod", "=", "get"), true);
        assertMatch(condition("httpMethod", "=", "post"), false);
        assertMatch(condition("httpMethod", "isBlank", ""), false);

        context.setHttpMethod("");
        assertMatch(condition("httpMethod", "isBlank", ""), true);
    }

    @Test
    public void testWithoutContext() {
        exchange.getAttributes().remove(Constants.CONTEXT);

        assertMatch(condition("httpMethod", "=", "get"), false);
        assertMatch(condition("startDateTime", "TimeAfter", "2026-11-06 12:34:55"), false);
        assertMatch(condition("startDateTime", "isBlank", ""), true);
    }

    @Test
    public void testCombinedConditions() {
        List<ConditionData> conditions = Arrays.asList(condition("startDateTime", "TimeBefore", "2026-11-06 12:34:57"),
                condition("httpMethod", "=", "get"));
        assertTrue(MatchStrategyFactory.match(MatchModeEnum.AND.getCode(), conditions, exchange));

        context.setHttpMethod("post");
        assertFalse(MatchStrategyFactory.match(MatchModeEnum.AND.getCode(), conditions, exchange));
        assertTrue(MatchStrategyFactory.match(MatchModeEnum.OR.getCode(), conditions, exchange));
    }

    private void assertMatch(final ConditionData condition, final boolean expected) {
        List<ConditionData> conditions = Collections.singletonList(condition);
        assertEquals(expected, MatchStrategyFactory.match(MatchModeEnum.AND.getCode(), conditions, exchange));
        assertEquals(expected, MatchStrategyFactory.match(MatchModeEnum.OR.getCode(), conditions, exchange));
    }

    private ConditionData condition(final String paramName, final String operator, final String paramValue) {
        ConditionData condition = new ConditionData();
        condition.setParamType("post");
        condition.setParamName(paramName);
        condition.setOperator(operator);
        condition.setParamValue(paramValue);
        return condition;
    }
}
