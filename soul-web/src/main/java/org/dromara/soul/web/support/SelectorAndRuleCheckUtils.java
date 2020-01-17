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

package org.dromara.soul.web.support;

import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.web.plugin.SoulPluginChain;
import org.dromara.soul.web.request.RequestDTO;
import org.dromara.soul.web.result.SoulResultEnum;
import org.dromara.soul.web.result.SoulResultUtils;
import org.dromara.soul.web.result.SoulResultWarp;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

/**
 * The type Selector and rule check utils.
 *
 * @author xiaoyu
 */
public class SelectorAndRuleCheckUtils {

    /**
     * logger.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(SelectorAndRuleCheckUtils.class);

    /**
     * Check selector mono.
     *
     * @param pluginName the plugin name
     * @param request    the request
     * @param exchange   the exchange
     * @param chain      the chain
     * @return the mono
     */
    public static Mono<Void> checkSelector(final String pluginName, final RequestDTO request, final ServerWebExchange exchange, final SoulPluginChain chain) {
        if (PluginEnum.DIVIDE.getName().equals(pluginName)
                || PluginEnum.DUBBO.getName().equals(pluginName)
                || PluginEnum.SPRING_CLOUD.getName().equals(pluginName)) {
            LOGGER.error("can not match selector data :{},params:{}", pluginName, request.toString());
            Object error = SoulResultWarp.error(SoulResultEnum.CANNOT_FIND_SELECTOR.getCode(), SoulResultEnum.CANNOT_FIND_SELECTOR.getMsg(), null);
            return SoulResultUtils.result(exchange, error);
        }
        return chain.execute(exchange);
    }

    /**
     * Check rule mono.
     *
     * @param pluginName the plugin name
     * @param request    the request
     * @param exchange   the exchange
     * @param chain      the chain
     * @return the mono
     */
    public static Mono<Void> checkRule(final String pluginName, final RequestDTO request, final ServerWebExchange exchange, final SoulPluginChain chain) {
        if (PluginEnum.DIVIDE.getName().equals(pluginName)
                || PluginEnum.DUBBO.getName().equals(pluginName)
                || PluginEnum.SPRING_CLOUD.getName().equals(pluginName)) {
            LOGGER.error("can not match rule data :{},params:{}", pluginName, request.toString());
            Object error = SoulResultWarp.error(SoulResultEnum.RULE_NOT_FIND.getCode(), SoulResultEnum.RULE_NOT_FIND.getMsg(), null);
            return SoulResultUtils.result(exchange, error);
        }
        return chain.execute(exchange);
    }
}
