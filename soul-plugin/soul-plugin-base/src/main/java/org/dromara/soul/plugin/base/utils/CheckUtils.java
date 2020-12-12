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

package org.dromara.soul.plugin.base.utils;

import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.api.result.SoulResultEnum;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

/**
 * The type Selector and rule check utils.
 *
 * @author xiaoyu
 */
@Slf4j
public class CheckUtils {

    /**
     * Check selector mono.
     *
     * @param pluginName the plugin name
     * @param exchange   the exchange
     * @param chain      the chain
     * @return the mono
     */
    public static Mono<Void> checkSelector(final String pluginName, final ServerWebExchange exchange, final SoulPluginChain chain) {
        if (PluginEnum.DIVIDE.getName().equals(pluginName)
                || PluginEnum.DUBBO.getName().equals(pluginName)
                || PluginEnum.SPRING_CLOUD.getName().equals(pluginName)) {
            log.error("can not match selector data: {}", pluginName);
            Object error = SoulResultWrap.error(SoulResultEnum.CANNOT_FIND_SELECTOR.getCode(), SoulResultEnum.CANNOT_FIND_SELECTOR.getMsg(), null);
            return WebFluxResultUtils.result(exchange, error);
        }
        return chain.execute(exchange);
    }

    /**
     * Check rule mono.
     *
     * @param pluginName the plugin name
     * @param exchange   the exchange
     * @param chain      the chain
     * @return the mono
     */
    public static Mono<Void> checkRule(final String pluginName, final ServerWebExchange exchange, final SoulPluginChain chain) {
        if (PluginEnum.DIVIDE.getName().equals(pluginName)
                || PluginEnum.DUBBO.getName().equals(pluginName)
                || PluginEnum.SPRING_CLOUD.getName().equals(pluginName)) {
            log.error("can not match rule data: {}", pluginName);
            Object error = SoulResultWrap.error(SoulResultEnum.RULE_NOT_FIND.getCode(), SoulResultEnum.RULE_NOT_FIND.getMsg(), null);
            return WebFluxResultUtils.result(exchange, error);
        }
        return chain.execute(exchange);
    }
}
