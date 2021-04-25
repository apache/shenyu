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

package org.dromara.soul.plugin.waf;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.convert.WafHandle;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.WafEnum;
import org.dromara.soul.common.enums.WafModelEnum;
import org.dromara.soul.plugin.base.utils.Singleton;
import org.dromara.soul.plugin.api.result.SoulResultWrap;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.base.AbstractSoulPlugin;
import org.dromara.soul.plugin.api.utils.WebFluxResultUtils;
import org.dromara.soul.plugin.waf.cache.WafRuleHandleCache;
import org.dromara.soul.plugin.waf.config.WafConfig;
import org.dromara.soul.plugin.waf.handler.WafPluginDataHandler;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;

/**
 * use waf plugin we can control some access.
 *
 * @author xiaoyu(Myth)
 */
@Slf4j
public class WafPlugin extends AbstractSoulPlugin {

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final SoulPluginChain chain, final SelectorData selector, final RuleData rule) {
        WafConfig wafConfig = Singleton.INST.get(WafConfig.class);
        if (Objects.isNull(selector) && Objects.isNull(rule)) {
            if (WafModelEnum.BLACK.getName().equals(wafConfig.getModel())) {
                return chain.execute(exchange);
            }
            exchange.getResponse().setStatusCode(HttpStatus.FORBIDDEN);
            Object error = SoulResultWrap.error(HttpStatus.FORBIDDEN.value(), Constants.REJECT_MSG, null);
            return WebFluxResultUtils.result(exchange, error);
        }
        String handle = rule.getHandle();
        WafHandle wafHandle = WafRuleHandleCache.getInstance()
                .obtainHandle(WafPluginDataHandler.getCacheKeyName(rule));
        if (Objects.isNull(wafHandle) || StringUtils.isBlank(wafHandle.getPermission())) {
            log.error("waf handler can not configuration：{}", handle);
            return chain.execute(exchange);
        }
        if (WafEnum.REJECT.getName().equals(wafHandle.getPermission())) {
            exchange.getResponse().setStatusCode(HttpStatus.FORBIDDEN);
            Object error = SoulResultWrap.error(Integer.parseInt(wafHandle.getStatusCode()), Constants.REJECT_MSG, null);
            return WebFluxResultUtils.result(exchange, error);
        }
        return chain.execute(exchange);
    }

    @Override
    protected Mono<Void> handleSelectorIsNull(final String pluginName, final ServerWebExchange exchange, final SoulPluginChain chain) {
        return doExecute(exchange, chain, null, null);
    }

    @Override
    protected Mono<Void> handleRuleIsNull(final String pluginName, final ServerWebExchange exchange, final SoulPluginChain chain) {
        return doExecute(exchange, chain, null, null);
    }

    @Override
    public String named() {
        return PluginEnum.WAF.getName();
    }

    @Override
    public int getOrder() {
        return PluginEnum.WAF.getCode();
    }
}
