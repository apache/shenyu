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

package org.apache.shenyu.plugin.base.utils;

import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.plugin.api.result.SoulResultEnum;
import org.apache.shenyu.plugin.api.result.SoulResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

/**
 * The type Selector and rule check utils.
 *
 * @author xiaoyu
 */
@Slf4j
public class FallbackUtils {

    /**
     * get no selector result.
     *
     * @param pluginName the plugin name
     * @param exchange   the exchange
     * @return the mono
     */
    public static Mono<Void> getNoSelectorResult(final String pluginName, final ServerWebExchange exchange) {
        log.error("can not match selector data: {}", pluginName);
        Object error = SoulResultWrap.error(SoulResultEnum.CANNOT_FIND_SELECTOR.getCode(), SoulResultEnum.CANNOT_FIND_SELECTOR.getMsg(), null);
        return WebFluxResultUtils.result(exchange, error);
    }

    /**
     * get no rule result.
     *
     * @param pluginName the plugin name
     * @param exchange   the exchange
     * @return the mono
     */
    public static Mono<Void> getNoRuleResult(final String pluginName, final ServerWebExchange exchange) {
        log.error("can not match rule data: {}", pluginName);
        Object error = SoulResultWrap.error(SoulResultEnum.RULE_NOT_FIND.getCode(), SoulResultEnum.RULE_NOT_FIND.getMsg(), null);
        return WebFluxResultUtils.result(exchange, error);
    }
}
