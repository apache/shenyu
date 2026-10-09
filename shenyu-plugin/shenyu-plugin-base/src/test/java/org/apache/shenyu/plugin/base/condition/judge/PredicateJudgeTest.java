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

package org.apache.shenyu.plugin.base.condition.judge;

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.MockedStatic;

import java.time.LocalDateTime;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mockStatic;

/**
 * Direct tests for routing condition predicates and their boundary cases.
 */
class PredicateJudgeTest {

    @ParameterizedTest
    @MethodSource("stringCases")
    void testStringPredicate(final PredicateJudge judge, final String expectedValue, final String realData, final boolean matches) {
        assertEquals(matches, judge.judge(condition(ParamTypeEnum.HEADER, expectedValue), realData));
    }

    private static Stream<Arguments> stringCases() {
        return Stream.of(
                Arguments.of(new EqualsPredicateJudge(), " value ", "value", true),
                Arguments.of(new EqualsPredicateJudge(), "value", "Value", false),
                Arguments.of(new EqualsPredicateJudge(), "value", " value ", false),
                Arguments.of(new EqualsPredicateJudge(), " ", "", true),
                Arguments.of(new ContainsPredicateJudge(), " value ", "a-value-b", true),
                Arguments.of(new ContainsPredicateJudge(), "value", "VALUE", false),
                Arguments.of(new ContainsPredicateJudge(), "value", "", false),
                Arguments.of(new StartsWithPredicateJudge(), " api ", "api/v1", true),
                Arguments.of(new StartsWithPredicateJudge(), "api", "/api", false),
                Arguments.of(new StartsWithPredicateJudge(), "api", "API", false),
                Arguments.of(new EndsWithPredicateJudge(), " .json ", "result.json", true),
                Arguments.of(new EndsWithPredicateJudge(), ".json", "result.json.gz", false),
                Arguments.of(new EndsWithPredicateJudge(), ".json", "result.JSON", false),
                Arguments.of(new MatchPredicateJudge(), " api/* ", "prefix-api/*-suffix", true),
                Arguments.of(new MatchPredicateJudge(), "api/*", "api/users", false),
                Arguments.of(new ExcludePredicateJudge(), " api/* ", "prefix-api/*-suffix", false),
                Arguments.of(new ExcludePredicateJudge(), "api/*", "api/users", true),
                Arguments.of(new PathPatternPredicateJudge(), " api/* ", "prefix-api/*-suffix", true),
                Arguments.of(new PathPatternPredicateJudge(), "api/*", "api/users", false));
    }

    @ParameterizedTest
    @MethodSource("pathCases")
    void testPathPredicate(final PredicateJudge judge, final String pattern, final String path, final boolean matches) {
        assertEquals(matches, judge.judge(condition(ParamTypeEnum.URI, pattern), path));
    }

    private static Stream<Arguments> pathCases() {
        return Stream.of(
                Arguments.of(new MatchPredicateJudge(), " /api/* ", "/api/users", true),
                Arguments.of(new MatchPredicateJudge(), "/api/*", "/api/users/1", false),
                Arguments.of(new MatchPredicateJudge(), "/api/**", "/api/users/1", true),
                Arguments.of(new MatchPredicateJudge(), "/api/**", "/other/api/users", false),
                Arguments.of(new MatchPredicateJudge(), "/api/{id}", "/api/42", true),
                Arguments.of(new MatchPredicateJudge(), "/api/**/detail", "/api/users/42/detail", true),
                Arguments.of(new ExcludePredicateJudge(), " /api/* ", "/api/users", false),
                Arguments.of(new ExcludePredicateJudge(), "/api/*", "/api/users/1", true),
                Arguments.of(new ExcludePredicateJudge(), "/api/**", "/api/users/1", false),
                Arguments.of(new ExcludePredicateJudge(), "/api/**", "/other/api/users", true),
                Arguments.of(new PathPatternPredicateJudge(), " /api/* ", "/api/users", true),
                Arguments.of(new PathPatternPredicateJudge(), "/api/*", "/api/users/1", false),
                Arguments.of(new PathPatternPredicateJudge(), "/api/**", "/api/users/1", true),
                Arguments.of(new PathPatternPredicateJudge(), "/api/**", "/other/api/users", false),
                Arguments.of(new PathPatternPredicateJudge(), "/api/{id}", "/api/42", true),
                Arguments.of(new PathPatternPredicateJudge(), "/api/{id}", "/api/42/detail", false));
    }

    @ParameterizedTest
    @CsvSource({"' /api/[0-9]+ ', /api/42, true", " /api/[0-9]+, /api/name, false",
            "/api/[0-9]+, /api/42/detail, false", "/api/[0-9]+, prefix/api/42, false", "/api/[0-9]+, /API/42, false"})
    void testRegexMatchesWholeValue(final String pattern, final String realData, final boolean matches) {
        assertEquals(matches, new RegexPredicateJudge().judge(condition(ParamTypeEnum.URI, pattern), realData));
    }

    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {" ", "  ", "\t", "\r\n"})
    void testBlankPredicate(final String realData) {
        assertTrue(new BlankPredicateJudge().judge(new ConditionData(), realData));
    }

    @ParameterizedTest
    @ValueSource(strings = {"value", " value ", "0"})
    void testBlankPredicateRejectsText(final String realData) {
        assertFalse(new BlankPredicateJudge().judge(new ConditionData(), realData));
    }

    @ParameterizedTest
    @CsvSource({"2026-01-01 11:59:59, false, true", "2026-01-01 12:00:00, false, false", "2026-01-01 12:00:01, true, false"})
    void testNamedTimePredicate(final String realData, final boolean after, final boolean before) {
        ConditionData conditionData = condition(ParamTypeEnum.HEADER, " 2026-01-01 12:00:00 ");
        conditionData.setParamName("request-time");
        assertEquals(after, new TimerAfterPredicateJudge().judge(conditionData, realData));
        assertEquals(before, new TimerBeforePredicateJudge().judge(conditionData, realData));
    }

    @ParameterizedTest
    @NullAndEmptySource
    void testUnnamedTimePredicateUsesCurrentTime(final String paramName) {
        LocalDateTime now = LocalDateTime.of(2026, 1, 1, 12, 0);
        ConditionData conditionData = condition(ParamTypeEnum.HEADER, "2026-01-01 12:00:00");
        conditionData.setParamName(paramName);
        try (MockedStatic<LocalDateTime> time = mockStatic(LocalDateTime.class, CALLS_REAL_METHODS)) {
            time.when(LocalDateTime::now).thenReturn(now);
            assertFalse(new TimerAfterPredicateJudge().judge(conditionData, "ignored"));
            assertFalse(new TimerBeforePredicateJudge().judge(conditionData, "ignored"));
            conditionData.setParamValue(" 2026-01-01 11:59:59 ");
            assertTrue(new TimerAfterPredicateJudge().judge(conditionData, "ignored"));
            assertFalse(new TimerBeforePredicateJudge().judge(conditionData, "ignored"));
            conditionData.setParamValue(" 2026-01-01 12:00:01 ");
            assertFalse(new TimerAfterPredicateJudge().judge(conditionData, "ignored"));
            assertTrue(new TimerBeforePredicateJudge().judge(conditionData, "ignored"));
        }
    }

    private static ConditionData condition(final ParamTypeEnum type, final String value) {
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(type.getName());
        conditionData.setParamValue(value);
        return conditionData;
    }
}
