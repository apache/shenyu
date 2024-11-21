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
import org.apache.shenyu.spi.SPI;
import org.springframework.web.server.ServerWebExchange;

import java.util.List;

/**
 * This is condition strategy.
 */
@SPI
@FunctionalInterface
public interface MatchStrategy {

    /**
     * this is condition match.
     *
     * @param conditionDataList condition list.
     * @param exchange          {@linkplain ServerWebExchange}
     * @return true is match , false is not match.
     */
    Boolean match(List<ConditionData> conditionDataList, ServerWebExchange exchange);
}
