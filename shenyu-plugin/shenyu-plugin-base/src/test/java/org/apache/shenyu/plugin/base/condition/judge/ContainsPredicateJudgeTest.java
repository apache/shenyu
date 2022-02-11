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

import junit.framework.TestCase;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class ContainsPredicateJudgeTest extends TestCase {

    private ConditionData conditionDataForIp;

    @BeforeEach
    public void setUp() {
        conditionDataForIp = new ConditionData();
        conditionDataForIp.setParamType(ParamTypeEnum.IP.getName());
        conditionDataForIp.setParamValue("127.0.0.1,128.0.0.1");
    }

    @Test
    public void testContainsJudgeForIp() {
        conditionDataForIp.setOperator(OperatorEnum.CONTAINS.getAlias());
        Assertions.assertTrue(PredicateJudgeFactory.judge(conditionDataForIp, "127.0.0.1"));
        Assertions.assertTrue(PredicateJudgeFactory.judge(conditionDataForIp, "128.0.0.1"));
        Assertions.assertFalse(PredicateJudgeFactory.judge(conditionDataForIp, "0.1.128.0"));
    }
}
