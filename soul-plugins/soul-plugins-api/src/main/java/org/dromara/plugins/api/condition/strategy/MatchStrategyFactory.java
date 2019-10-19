/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.plugins.api.condition.strategy;

import com.google.common.collect.Maps;
import org.dromara.soul.common.enums.MatchModeEnum;

import java.util.Map;

/**
 * MatchStrategyFactory.
 *
 * @author xiaoyu(Myth)
 */
public class MatchStrategyFactory {

    private static final Map<Integer, MatchStrategy> MATCH_STRATEGY_MAP = Maps.newHashMapWithExpectedSize(2);

    static {
        MATCH_STRATEGY_MAP.put(MatchModeEnum.AND.getCode(), new AndMatchStrategy());
        MATCH_STRATEGY_MAP.put(MatchModeEnum.OR.getCode(), new OrMatchStrategy());
    }

    /**
     * this factory of MatchStrategy.
     *
     * @param strategy which is strategy
     * @return {@linkplain MatchStrategy}
     */
    public static MatchStrategy of(final Integer strategy) {
        return MATCH_STRATEGY_MAP.get(strategy);
    }
}
