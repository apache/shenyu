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

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.condition.data.ParameterDataFactory;
import org.apache.shenyu.plugin.base.condition.judge.PredicateJudgeFactory;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.springframework.web.server.ServerWebExchange;

import java.util.Objects;

/**
 * AbstractMatchStrategy.
 */
public abstract class AbstractMatchStrategy {

    /**
     * Build real data string.
     *
     * @param condition the condition
     * @param exchange  the exchange
     * @return the string
     */
    public String buildRealData(final ConditionData condition, final ServerWebExchange exchange) {
        return ParameterDataFactory.builderData(condition.getParamType(), condition.getParamName(), exchange);
    }

    /**
     * this is condition match.
     *
     * @param pluginName the plugin name
     * @param condition  the condition.
     * @param exchange   {@linkplain ServerWebExchange}
     * @return true is match , false is not match.
     */
    public Boolean match(final String pluginName, final ConditionData condition, final ServerWebExchange exchange) {
        final String realData = buildRealData(condition, exchange);
        // The same real data here may be both a selector and a rule, which need to be distinguished
        final String cacheKey = CacheKeyUtils.INST.getKey(condition, realData);
        // condition and real data are not mapped one-to-one, so we need to add a plugin condition
        final Object matched = BaseDataCache.getInstance().obtainMatched(pluginName, cacheKey);
        if (Objects.nonNull(matched)) {
            return true;
        }
        final Object conditionParent = BaseDataCache.getInstance().getConditionParent(condition.getId());
        final Boolean match = PredicateJudgeFactory.judge(condition, realData);
        if (match && Objects.nonNull(conditionParent)) {
            BaseDataCache.getInstance().cacheMatched(pluginName, cacheKey, conditionParent);
            return true;
        }
        return match;
    }
}
