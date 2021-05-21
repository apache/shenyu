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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.spi.ExtensionLoader;

import java.util.Objects;

/**
 * Predicate judge factory.
 */
public class PredicateJudgeFactory {


    /**
     * judge request realData has by pass.
     *
     * @param conditionData condition data
     * @param realData      realData
     * @return is true pass   is false not pass
     */
    public static Boolean judge(final ConditionData conditionData, final String realData) {
        if (Objects.isNull(conditionData) || StringUtils.isBlank(realData)) {
            return false;
        }
        String operator = processSpecialOperator(conditionData.getOperator());
        PredicateJudge predicateJudge = ExtensionLoader.getExtensionLoader(PredicateJudge.class).getJoin(operator);
        return predicateJudge.judge(conditionData, realData);
    }

    /**
     * process special operator, like = need to change to equals.
     *
     * @param operator {@linkplain OperatorEnum} alias
     * @return alias
     */
    private static String processSpecialOperator(final String operator) {
        if ("=".equals(operator)) {
            return "equals";
        } else {
            return operator;
        }
    }

}
