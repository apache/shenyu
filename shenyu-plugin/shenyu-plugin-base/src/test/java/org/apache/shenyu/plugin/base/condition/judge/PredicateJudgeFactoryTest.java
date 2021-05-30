/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License,  Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,  software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,  either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.plugin.base.condition.judge;

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * Test cases for {@link PredicateJudgeFactory}.
 */
public final class PredicateJudgeFactoryTest {
    private static final String FIRST_TIME = "2018-07-11 17:20:00";

    private static final String MAX_TIME = "2099-07-11 17:20:00";

    private ConditionData conditionData;

    @Before
    public void setUp() {
        conditionData = new ConditionData();
        conditionData.setParamType("uri");
        conditionData.setParamValue("/http/**");
    }

    @Test
    public void testConditionDataIsNull() {
        Assert.assertFalse(PredicateJudgeFactory.judge(null, "testRealData"));
    }

    @Test
    public void testRealDataIsBlank() {
        Assert.assertFalse(PredicateJudgeFactory.judge(conditionData, null));
        Assert.assertFalse(PredicateJudgeFactory.judge(conditionData, ""));
    }

    @Test
    public void testEqJudge() {
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        Assert.assertTrue(PredicateJudgeFactory.judge(conditionData, "/http/**"));
        Assert.assertFalse(PredicateJudgeFactory.judge(conditionData, "/http/test"));
    }

    @Test
    public void testMatchJudge() {
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        Assert.assertTrue(PredicateJudgeFactory.judge(conditionData, "/http/**"));
        Assert.assertTrue(PredicateJudgeFactory.judge(conditionData, "/http/test"));
        Assert.assertTrue(PredicateJudgeFactory.judge(conditionData, "/http/test/test"));
        Assert.assertFalse(PredicateJudgeFactory.judge(conditionData, "/http1/**"));

        conditionData.setParamType(ParamTypeEnum.HEADER.getName());
        Assert.assertTrue(PredicateJudgeFactory.judge(conditionData, "/http/**"));
        Assert.assertTrue(PredicateJudgeFactory.judge(conditionData, "/http/**/test"));
        Assert.assertFalse(PredicateJudgeFactory.judge(conditionData, "/http1/**"));
    }

    @Test
    public void testContainsJudge() {
        conditionData.setOperator(OperatorEnum.CONTAINS.getAlias());
        Assert.assertTrue(PredicateJudgeFactory.judge(conditionData, "/http/**/test"));
        Assert.assertTrue(PredicateJudgeFactory.judge(conditionData, "/test/http/**"));
        Assert.assertFalse(PredicateJudgeFactory.judge(conditionData, "/http1/**"));
    }

    @Test
    public void testRegexJudge() {
        conditionData.setOperator(OperatorEnum.REGEX.getAlias());
        conditionData.setParamValue("[/a-zA-Z0-9]+");
        Assert.assertTrue(PredicateJudgeFactory.judge(conditionData, "/http/test"));
        Assert.assertFalse(PredicateJudgeFactory.judge(conditionData, "/http?/test"));
    }

    @Test
    public void testSpELJudge() {
        conditionData.setOperator(OperatorEnum.SPEL.getAlias());
        conditionData.setParamType(ParamTypeEnum.HEADER.getName());
        conditionData.setParamName("userId");
        conditionData.setParamValue("#userId % 3 == 0");
        Assert.assertTrue(PredicateJudgeFactory.judge(conditionData, "3"));
        Assert.assertFalse(PredicateJudgeFactory.judge(conditionData, "4"));
    }

    @Test
    public void testGroovyJudge() {
        conditionData.setOperator(OperatorEnum.GROOVY.getAlias());
        conditionData.setParamType(ParamTypeEnum.HEADER.getName());
        conditionData.setParamName("userId");
        conditionData.setParamValue("Integer.valueOf(userId) % 3 == 0");
        Assert.assertTrue(PredicateJudgeFactory.judge(conditionData, "3"));
        Assert.assertFalse(PredicateJudgeFactory.judge(conditionData, "4"));
        conditionData.setParamValue("userId.hashCode() % 3 == 0");
        Assert.assertTrue(PredicateJudgeFactory.judge(conditionData, "3"));
        Assert.assertFalse(PredicateJudgeFactory.judge(conditionData, "4"));
    }

    @Test
    public void testTimerBeforeJudge() {
        conditionData.setOperator(OperatorEnum.TIME_BEFORE.getAlias());
        //Because the realData can't be empty, so the realDate must fill in the values
        conditionData.setParamValue(MAX_TIME);
        Assert.assertTrue(PredicateJudgeFactory.judge(conditionData, MAX_TIME));
        conditionData.setParamValue(FIRST_TIME);
        Assert.assertFalse(PredicateJudgeFactory.judge(conditionData, MAX_TIME));
        conditionData.setParamName(OperatorEnum.TIME_BEFORE.name());
        conditionData.setParamValue(MAX_TIME);
        Assert.assertTrue(PredicateJudgeFactory.judge(conditionData, FIRST_TIME));
        Assert.assertFalse(PredicateJudgeFactory.judge(conditionData, MAX_TIME));
    }

    @Test
    public void testTimerAfterJudge() {
        conditionData.setOperator(OperatorEnum.TIME_AFTER.getAlias());
        conditionData.setParamValue(FIRST_TIME);
        Assert.assertTrue(PredicateJudgeFactory.judge(conditionData, FIRST_TIME));
        conditionData.setParamValue(MAX_TIME);
        Assert.assertFalse(PredicateJudgeFactory.judge(conditionData, FIRST_TIME));
    }

}
