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
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link PredicateJudgeFactory}.
 */
public final class PredicateJudgeFactoryTest {

    private static final String FIRST_TIME = "2018-07-11 17:20:00";

    private static final String MAX_TIME = "2099-07-11 17:20:00";

    private ConditionData conditionData;

    @BeforeEach
    public void setUp() {
        conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setParamValue("/http/");
    }

    @Test
    public void testConditionDataIsNull() {
        assertFalse(PredicateJudgeFactory.judge(null, "testRealData"));
    }

    @Test
    public void testRealDataIsBlank() {
        assertFalse(PredicateJudgeFactory.judge(conditionData, null));
        assertFalse(PredicateJudgeFactory.judge(conditionData, ""));
    }

    @Test
    public void testEqJudge() {
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        assertTrue(PredicateJudgeFactory.judge(conditionData, "/http/"));
        assertFalse(PredicateJudgeFactory.judge(conditionData, "/http/test"));
    }

    @Test
    public void testMatchJudge() {
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamValue("/http/**");
        assertTrue(PredicateJudgeFactory.judge(conditionData, "/http/**"));
        assertTrue(PredicateJudgeFactory.judge(conditionData, "/http/test"));
        assertTrue(PredicateJudgeFactory.judge(conditionData, "/http/test/test"));
        assertFalse(PredicateJudgeFactory.judge(conditionData, "/http1/**"));

        conditionData.setParamType(ParamTypeEnum.HEADER.getName());
        assertTrue(PredicateJudgeFactory.judge(conditionData, "/http/**"));
        assertTrue(PredicateJudgeFactory.judge(conditionData, "/http/**/test"));
        assertFalse(PredicateJudgeFactory.judge(conditionData, "/http1/**"));
    }
    
    @Test
    public void testPathPatternJudge() {
        conditionData.setOperator(OperatorEnum.PATH_PATTERN.getAlias());
        conditionData.setParamValue("/http/**");
        assertTrue(PredicateJudgeFactory.judge(conditionData, "/http/**"));
        assertTrue(PredicateJudgeFactory.judge(conditionData, "/http/test"));
        assertTrue(PredicateJudgeFactory.judge(conditionData, "/http/test/test"));
        assertFalse(PredicateJudgeFactory.judge(conditionData, "/http1/**"));
    }

    @Test
    public void testRegexJudge() {
        conditionData.setOperator(OperatorEnum.REGEX.getAlias());
        conditionData.setParamValue("[/a-zA-Z0-9]+");
        assertTrue(PredicateJudgeFactory.judge(conditionData, "/http/test"));
        assertFalse(PredicateJudgeFactory.judge(conditionData, "/http?/test"));
    }

    @Test
    public void testTimerBeforeJudge() {
        conditionData.setOperator(OperatorEnum.TIME_BEFORE.getAlias());
        //Because the realData can't be empty, so the realDate must fill in the values
        conditionData.setParamValue(MAX_TIME);
        assertTrue(PredicateJudgeFactory.judge(conditionData, MAX_TIME));
        conditionData.setParamValue(FIRST_TIME);
        assertFalse(PredicateJudgeFactory.judge(conditionData, MAX_TIME));
        conditionData.setParamName(OperatorEnum.TIME_BEFORE.name());
        conditionData.setParamValue(MAX_TIME);
        assertTrue(PredicateJudgeFactory.judge(conditionData, FIRST_TIME));
        assertFalse(PredicateJudgeFactory.judge(conditionData, MAX_TIME));
    }

    @Test
    public void testTimerAfterJudge() {
        conditionData.setOperator(OperatorEnum.TIME_AFTER.getAlias());
        conditionData.setParamValue(FIRST_TIME);
        assertTrue(PredicateJudgeFactory.judge(conditionData, FIRST_TIME));
        conditionData.setParamValue(MAX_TIME);
        assertFalse(PredicateJudgeFactory.judge(conditionData, FIRST_TIME));
    }

    @Test
    public void testExcludeJudge() {
        conditionData.setOperator(OperatorEnum.EXCLUDE.getAlias());
        conditionData.setParamValue("/http/test");
        assertFalse(PredicateJudgeFactory.judge(conditionData, "/http/test"));
        assertTrue(PredicateJudgeFactory.judge(conditionData, "/http?/test"));
    }

    @Test
    public void testContainsJudge() {
        conditionData.setOperator(OperatorEnum.CONTAINS.getAlias());
        assertTrue(PredicateJudgeFactory.judge(conditionData, "/http/**/test"));
        assertTrue(PredicateJudgeFactory.judge(conditionData, "/test/http/**"));
        assertFalse(PredicateJudgeFactory.judge(conditionData, "/http1/**"));
    }

    @Test
    public void testStartsJudge() {
        conditionData.setOperator(OperatorEnum.STARTS_WITH.getAlias());
        assertTrue(PredicateJudgeFactory.judge(conditionData, "/http/**/test"));
        assertFalse(PredicateJudgeFactory.judge(conditionData, "/test/http/**"));
        assertFalse(PredicateJudgeFactory.judge(conditionData, "/http1/**"));
    }

    @Test
    public void testEndsJudge() {
        conditionData.setOperator(OperatorEnum.ENDS_WITH.getAlias());
        assertTrue(PredicateJudgeFactory.judge(conditionData, "/**/test/http/"));
        assertFalse(PredicateJudgeFactory.judge(conditionData, "/test/http/**"));
        assertFalse(PredicateJudgeFactory.judge(conditionData, "/**/http1/"));
    }
}
