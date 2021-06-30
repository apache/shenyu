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

package org.apache.shenyu.plugin.oauth2.filter;

import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.common.utils.CollectionUtils;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.condition.strategy.MatchStrategyFactory;
import org.springframework.security.web.server.util.matcher.PathPatternParserServerWebExchangeMatcher;
import org.springframework.security.web.server.util.matcher.ServerWebExchangeMatcher;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilter;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * The OAuth2PreFilter.
 *
 * <p>This filter is mainly used to process the usage status of OAuth2 plugins
 * and the paths that need to be authorized by OAuth2
 */
@Slf4j
public class OAuth2PreFilter implements WebFilter {

    private static final AtomicBoolean SKIP = new AtomicBoolean(true);

    private final List<ServerWebExchangeMatcher> matchers;

    public OAuth2PreFilter(final List<ServerWebExchangeMatcher> matchers) {
        this.matchers = matchers;
    }

    @Override
    public Mono<Void> filter(final ServerWebExchange serverWebExchange, final WebFilterChain webFilterChain) {
        PluginData pluginData = BaseDataCache.getInstance().obtainPluginData(PluginEnum.OAUTH2.getName());
        boolean enable = Objects.nonNull(pluginData) && pluginData.getEnabled();
        serverWebExchange.getAttributes().put("enable", enable);
        if (enable) {
            this.processPathMatchers(serverWebExchange);
        }
        return webFilterChain.filter(serverWebExchange);
    }

    private void processPathMatchers(final ServerWebExchange serverWebExchange) {
        if ((Boolean) serverWebExchange.getAttributes().get("enable")) {
            this.buildPathMatchers(serverWebExchange);
        } else {
            this.refreshPathMatchers();
        }
    }

    private Boolean filterSelector(final SelectorData selector, final ServerWebExchange exchange) {
        if (selector.getType() == SelectorTypeEnum.CUSTOM_FLOW.getCode()) {
            if (CollectionUtils.isEmpty(selector.getConditionList())) {
                return false;
            }
            return MatchStrategyFactory.match(selector.getMatchMode(), selector.getConditionList(), exchange);
        }
        return true;
    }

    private void buildPathMatchers(final ServerWebExchange serverWebExchange) {
        refreshPathMatchers();
        List<SelectorData> oauth2Selectors = BaseDataCache.getInstance().obtainSelectorData(PluginEnum.OAUTH2.getName());
        if (CollectionUtils.isEmpty(oauth2Selectors)) {
            serverWebExchange.getAttributes().put("skip", true);
            return;
        }
        SelectorData selectorData = oauth2Selectors
            .parallelStream()
            .filter(selector -> selector.getEnabled() && filterSelector(selector, serverWebExchange))
            .findFirst()
            .orElse(null);
        if (Objects.isNull(selectorData)) {
            serverWebExchange.getAttributes().put("skip", true);
            return;
        }
        if (selectorData.getType().equals(SelectorTypeEnum.FULL_FLOW.getCode())) {
            matchers.add(new PathPatternParserServerWebExchangeMatcher("/**"));
        } else {
            List<RuleData> ruleData = BaseDataCache.getInstance().obtainRuleData(selectorData.getId());
            ruleData.parallelStream()
                .filter(RuleData::getEnabled)
                .forEach(rule ->
                    rule
                        .getConditionDataList()
                        .forEach(data -> matchers.add(new PathPatternParserServerWebExchangeMatcher(data.getParamValue()))
                        )
                );
        }
        SKIP.set(matchers.size() <= 1);
        serverWebExchange.getAttributes().put("skip", SKIP.get());
    }

    private void refreshPathMatchers() {
        matchers.clear();
        matchers.add(new PathPatternParserServerWebExchangeMatcher("-"));
    }
}
