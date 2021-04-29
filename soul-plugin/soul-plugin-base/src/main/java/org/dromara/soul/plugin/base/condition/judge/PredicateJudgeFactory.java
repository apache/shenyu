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

package org.dromara.soul.plugin.base.condition.judge;

import com.google.common.collect.Maps;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.common.dto.ConditionData;
import org.dromara.soul.common.enums.OperatorEnum;

import java.util.Map;
import java.util.Objects;

/**
 * Predicate judge factory.
 *
 * @author xiaoyu(Myth)
 */
public class PredicateJudgeFactory {

    private static final Map<String, PredicateJudge> PREDICATE_JUDGE_MAP = Maps.newHashMapWithExpectedSize(16);

    static {
        PREDICATE_JUDGE_MAP.put(OperatorEnum.EQ.getAlias(), new EqualsPredicateJudge());
        PREDICATE_JUDGE_MAP.put(OperatorEnum.MATCH.getAlias(), new MatchPredicateJudge());
        PREDICATE_JUDGE_MAP.put(OperatorEnum.CONTAINS.getAlias(), new ContainsPredicateJudge());
        PREDICATE_JUDGE_MAP.put(OperatorEnum.REGEX.getAlias(), new RegexPredicateJudge());
        PREDICATE_JUDGE_MAP.put(OperatorEnum.SPEL.getAlias(), new SpELPredicateJudge());
        PREDICATE_JUDGE_MAP.put(OperatorEnum.GROOVY.getAlias(), new GroovyPredicateJudge());
        PREDICATE_JUDGE_MAP.put(OperatorEnum.TIME_BEFORE.getAlias(), new TimerBeforePredicateJudge());
        PREDICATE_JUDGE_MAP.put(OperatorEnum.TIME_AFTER.getAlias(), new TimerAfterPredicateJudge());
    }

    /**
     * judge request realData has by pass.
     * @param conditionData condition data
     * @param realData       realData
     * @return is true pass   is false not pass
     */
    public static Boolean judge(final ConditionData conditionData, final String realData) {
        if (Objects.isNull(conditionData) || StringUtils.isBlank(realData)) {
            return false;
        }
        return PREDICATE_JUDGE_MAP.get(conditionData.getOperator()).judge(conditionData, realData);
    }
}
