/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.web.condition.strategy;

import org.dromara.soul.common.dto.ConditionData;
import org.dromara.soul.common.enums.MatchModeEnum;
import org.dromara.soul.common.extension.ExtensionLoader;
import org.springframework.web.server.ServerWebExchange;

import java.util.List;

/**
 * MatchStrategyFactory.
 *
 * @author xiaoyu(Myth)
 */
public class MatchStrategyUtils {

    /**
     * Match boolean.
     *
     * @param strategy          the strategy
     * @param conditionDataList the condition data list
     * @param exchange          the exchange
     * @return the boolean
     */
    public static boolean match(final Integer strategy, final List<ConditionData> conditionDataList, final ServerWebExchange exchange) {
        String matchMode = MatchModeEnum.getMatchModeByCode(strategy);
        MatchStrategy matchStrategy = ExtensionLoader.getExtensionLoader(MatchStrategy.class).getJoin(matchMode);
        return matchStrategy.match(conditionDataList, exchange);
    }
}
