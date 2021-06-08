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
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.springframework.security.web.server.util.matcher.PathPatternParserServerWebExchangeMatcher;
import org.springframework.security.web.server.util.matcher.ServerWebExchangeMatcher;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilter;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * The OAuth2PreFilter.
 *
 * <p>This filter is mainly used to process the usage status of OAuth2 plugins
 * and the paths that need to be authorized by OAuth2
 */
@Slf4j
public class OAuth2PreFilter implements WebFilter {

    private static final AtomicBoolean ENABLE = new AtomicBoolean(false);

    private final List<ServerWebExchangeMatcher> matchers;

    public OAuth2PreFilter(final List<ServerWebExchangeMatcher> matchers) {
        this.matchers = matchers;
    }

    @Override
    public Mono<Void> filter(final ServerWebExchange serverWebExchange, final WebFilterChain webFilterChain) {
        this.changeSkipOAuth2Status();
        serverWebExchange.getAttributes().put("enable", ENABLE.get());
        return webFilterChain.filter(serverWebExchange);
    }

    /**
     * Change OAuth2 skip status.
     */
    private void changeSkipOAuth2Status() {
        PluginData pluginData = BaseDataCache.getInstance().obtainPluginData(PluginEnum.OAUTH2.getName());
        boolean expect = ENABLE.get();
        if (pluginData == null || (expect == pluginData.getEnabled() && matchers.size() > 1)) {
            return;
        }

        if (expect != pluginData.getEnabled()) {
            Boolean enableStatus = pluginData.getEnabled();
            do {
                log.info("the current OAuth2 enable status is {}, try to change OAuth2 enable status to {}", expect, enableStatus);
            } while (!ENABLE.compareAndSet(expect, enableStatus));
            log.info("change OAuth2 enable status to {}", enableStatus);
        }

        this.processPathMatchers();
    }

    private void processPathMatchers() {
        if (ENABLE.get()) {
            this.buildPathMatchers();
        } else {
            this.refreshPathMatchers();
        }
    }

    private void refreshPathMatchers() {
        if (matchers.size() == 1) {
            return;
        }
        matchers.clear();
        matchers.add(new PathPatternParserServerWebExchangeMatcher("/"));
    }

    private void buildPathMatchers() {
        matchers.clear();
        List<SelectorData> oauth2Selectors = BaseDataCache.getInstance().obtainSelectorData("oauth2");
        oauth2Selectors
            .parallelStream()
            .filter(SelectorData::getEnabled)
            .forEach(select -> {
                List<RuleData> ruleData = BaseDataCache.getInstance().obtainRuleData(select.getId());
                ruleData.parallelStream()
                    .filter(RuleData::getEnabled)
                    .forEach(rule ->
                        rule.getConditionDataList().forEach(data ->
                            matchers.add(new PathPatternParserServerWebExchangeMatcher(data.getParamValue()))
                        )
                    );
            });
    }
}
