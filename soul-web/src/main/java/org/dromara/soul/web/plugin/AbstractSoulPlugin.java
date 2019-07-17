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

package org.dromara.soul.web.plugin;

import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.SelectorTypeEnum;
import org.dromara.soul.common.result.SoulResult;
import org.dromara.soul.common.utils.JsonUtils;
import org.dromara.soul.common.utils.LogUtils;
import org.dromara.soul.web.cache.LocalCacheManager;
import org.dromara.soul.web.condition.strategy.MatchStrategyFactory;
import org.dromara.soul.web.request.RequestDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Objects;


/**
 * abstract soul plugin please extends.
 *
 * @author xiaoyu(Myth)
 */
@RequiredArgsConstructor
public abstract class AbstractSoulPlugin implements SoulPlugin {

    /**
     * logger.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(AbstractSoulPlugin.class);

    private final LocalCacheManager localCacheManager;

    /**
     * this is Template Method child has Implement your own logic.
     *
     * @param exchange exchange the current server exchange {@linkplain ServerWebExchange}
     * @param chain    chain the current chain  {@linkplain ServerWebExchange}
     * @param selector selector    {@linkplain SelectorData}
     * @param rule     rule    {@linkplain RuleData}
     * @return {@code Mono<Void>} to indicate when request handling is complete
     */
    protected abstract Mono<Void> doExecute(ServerWebExchange exchange, SoulPluginChain chain, SelectorData selector, RuleData rule);


    /**
     * Process the Web request and (optionally) delegate to the next
     * {@code WebFilter} through the given {@link SoulPluginChain}.
     *
     * @param exchange the current server exchange
     * @param chain    provides a way to delegate to the next filter
     * @return {@code Mono<Void>} to indicate when request processing is complete
     */
    @Override
    public Mono<Void> execute(final ServerWebExchange exchange, final SoulPluginChain chain) {
        final PluginData pluginData = localCacheManager.findPluginByName(named());
        if (!(skip(exchange) || pluginData == null || !pluginData.getEnabled())) {
            //获取selector
            final List<SelectorData> selectors = localCacheManager.findSelectorByPluginName(named());
            if (CollectionUtils.isEmpty(selectors)) {
                return chain.execute(exchange);
            }
            final SelectorData selectorData = selectors.stream()
                    .filter(selector -> selector.getEnabled() && filterSelector(selector, exchange))
                    .findFirst().orElse(null);

            if (Objects.isNull(selectorData)) {
                return chain.execute(exchange);
            }

            if (selectorData.getLoged()) {
                LogUtils.info(LOGGER, named()
                        + " selector success selector name :{}", selectorData::getName);
            }
            final List<RuleData> rules =
                    localCacheManager.findRuleBySelectorId(selectorData.getId());
            if (CollectionUtils.isEmpty(rules)) {
                return chain.execute(exchange);
            }

            RuleData rule = filterRule(exchange, rules);

            final RequestDTO request = exchange.getAttribute(Constants.REQUESTDTO);

            if (Objects.isNull(rule)) {
                //If the divide or dubbo or spring cloud plug-in does not match, return directly
                if (PluginEnum.DIVIDE.getName().equals(named())
                        || PluginEnum.DUBBO.getName().equals(named())
                        || PluginEnum.SPRING_CLOUD.getName().equals(named())) {
                    LogUtils.info(LOGGER, () -> Objects.requireNonNull(request).getModule() + ":"
                            + request.getMethod() + " not match  " + named() + "  rule");
                    final SoulResult error = SoulResult.error(HttpStatus.NOT_FOUND.value(),
                            Constants.UPSTREAM_NOT_FIND);
                    return exchange.getResponse()
                            .writeWith(Mono.just(exchange.getResponse().bufferFactory()
                                    .wrap(Objects.requireNonNull(JsonUtils.toJson(error)).getBytes())));
                }
                return chain.execute(exchange);
            }

            if (rule.getLoged()) {
                LogUtils.info(LOGGER, () -> Objects.requireNonNull(request).getModule() + ":" + request.getMethod() + " match " + named()
                        + " rule is name :"
                        + rule.getName());
            }
            return doExecute(exchange, chain, selectorData, rule);
        }
        return chain.execute(exchange);
    }

    private Boolean filterSelector(final SelectorData selector, final ServerWebExchange exchange) {
        if (selector.getType() == SelectorTypeEnum.CUSTOM_FLOW.getCode()) {
            if (CollectionUtils.isEmpty(selector.getConditionList())) {
                return false;
            }
            return MatchStrategyFactory.of(selector.getMatchMode())
                    .match(selector.getConditionList(), exchange);
        }
        return true;
    }

    private RuleData filterRule(final ServerWebExchange exchange, final List<RuleData> rules) {
        return rules.stream()
                .filter(rule -> Objects.nonNull(rule) && rule.getEnabled())
                .filter(ruleData -> MatchStrategyFactory.of(ruleData.getMatchMode())
                        .match(ruleData.getConditionDataList(), exchange))
                .findFirst().orElse(null);
    }
}
