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

import com.google.common.collect.Lists;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.time.format.DateTimeParseException;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for MatchStrategyFactory.
 */
public final class MatchStrategyFactoryTest {

    private ServerWebExchange exchange;

    private List<ConditionData> conditionDataList;

    @BeforeEach
    public void setUp() {
        this.conditionDataList = Lists.newArrayListWithCapacity(2);
        ConditionData matchConditionData = new ConditionData();
        matchConditionData.setOperator("match");
        matchConditionData.setParamName("shenyu");
        matchConditionData.setParamType("uri");
        matchConditionData.setParamValue("/http/**");
        ConditionData eqConditionData = new ConditionData();
        eqConditionData.setOperator("=");
        eqConditionData.setParamName("shenyu");
        eqConditionData.setParamType("uri");
        eqConditionData.setParamValue("/http/test");
        conditionDataList.add(matchConditionData);
        conditionDataList.add(eqConditionData);
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/http/shenyu")
                .build());
    }

    @Test
    public void testNewInstance() {
        MatchStrategy andMatchStrategy = MatchStrategyFactory.newInstance(MatchModeEnum.AND.getCode());
        assertEquals(andMatchStrategy.getClass(), AndMatchStrategy.class);

        MatchStrategy orMatchStrategy = MatchStrategyFactory.newInstance(MatchModeEnum.OR.getCode());
        assertEquals(orMatchStrategy.getClass(), OrMatchStrategy.class);

        int nonExistCode = -1;
        MatchStrategy defaultMatchStrategy = MatchStrategyFactory.newInstance(nonExistCode);
        assertEquals(defaultMatchStrategy.getClass(), AndMatchStrategy.class);
    }

    @Test
    public void testMatch() {
        assertFalse(MatchStrategyFactory.match(MatchModeEnum.AND.getCode(), conditionDataList, exchange));
        assertTrue(MatchStrategyFactory.match(MatchModeEnum.OR.getCode(), conditionDataList, exchange));
    }

    @ParameterizedTest
    @CsvSource({"TimeBefore, header, not-a-date", "TimeAfter, header, not-a-date",
            "TimeBefore, query, not-a-date", "TimeAfter, query, not-a-date",
            "TimeBefore, header, 2026-13-01 00:00:00", "TimeAfter, header, 2026-13-01 00:00:00",
            "TimeBefore, query, 2026-13-01 00:00:00", "TimeAfter, query, 2026-13-01 00:00:00"})
    public void testInvalidRequestTimeDoesNotPreventOtherConditions(final String operator, final String paramType, final String requestTime) {
        ConditionData timeCondition = timeCondition(operator, paramType, "2030-01-01 00:00:00");
        ServerWebExchange timeExchange = timeExchange(paramType, requestTime);
        List<ConditionData> conditions = List.of(timeCondition, conditionDataList.get(0));

        assertFalse(MatchStrategyFactory.match(MatchModeEnum.AND.getCode(), conditions, timeExchange));
        assertTrue(MatchStrategyFactory.match(MatchModeEnum.OR.getCode(), conditions, timeExchange));
    }

    @ParameterizedTest
    @CsvSource({"TimeBefore, header, 2026-01-01 00:00:00, true", "TimeAfter, header, 2026-01-01 00:00:00, false",
            "TimeBefore, query, 2026-01-01 00:00:00, true", "TimeAfter, query, 2026-01-01 00:00:00, false",
            "TimeBefore, header, 2031-01-01 00:00:00, false", "TimeAfter, header, 2031-01-01 00:00:00, true",
            "TimeBefore, query, 2031-01-01 00:00:00, false", "TimeAfter, query, 2031-01-01 00:00:00, true"})
    public void testValidRequestTimeMatches(final String operator, final String paramType, final String requestTime, final boolean expected) {
        ConditionData timeCondition = timeCondition(operator, paramType, "2030-01-01 00:00:00");

        assertEquals(expected, MatchStrategyFactory.match(MatchModeEnum.AND.getCode(), List.of(timeCondition), timeExchange(paramType, requestTime)));
    }

    @ParameterizedTest
    @CsvSource({"TimeBefore, header, 2026-01-01 00:00:00", "TimeAfter, header, 2026-01-01 00:00:00",
            "TimeBefore, query, 2026-01-01 00:00:00", "TimeAfter, query, 2026-01-01 00:00:00",
            "TimeBefore, header, not-a-date", "TimeAfter, header, not-a-date",
            "TimeBefore, query, not-a-date", "TimeAfter, query, not-a-date"})
    public void testInvalidConfiguredTimeStillFails(final String operator, final String paramType, final String requestTime) {
        ConditionData timeCondition = timeCondition(operator, paramType, "invalid-configured-time");

        assertThrows(DateTimeParseException.class,
                () -> MatchStrategyFactory.match(MatchModeEnum.AND.getCode(), List.of(timeCondition), timeExchange(paramType, requestTime)));
    }

    private ConditionData timeCondition(final String operator, final String paramType, final String configuredTime) {
        ConditionData conditionData = new ConditionData();
        conditionData.setOperator(operator);
        conditionData.setParamName("request-time");
        conditionData.setParamType(paramType);
        conditionData.setParamValue(configuredTime);
        return conditionData;
    }

    private ServerWebExchange timeExchange(final String paramType, final String requestTime) {
        MockServerHttpRequest.BaseBuilder<?> request = MockServerHttpRequest.get("/http/shenyu");
        if ("header".equals(paramType)) {
            request.header("request-time", requestTime);
        } else {
            request.queryParam("request-time", requestTime);
        }
        return MockServerWebExchange.from(request.build());
    }
}
