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
 */
@Slf4j
public class OAuth2PreFilter implements WebFilter {

    private static final AtomicBoolean SKIP = new AtomicBoolean(false);

    private final List<ServerWebExchangeMatcher> matchers;

    public OAuth2PreFilter(final List<ServerWebExchangeMatcher> matchers) {
        this.matchers = matchers;
    }

    @Override
    public Mono<Void> filter(final ServerWebExchange serverWebExchange, final WebFilterChain webFilterChain) {
        changeSkipOAuth2Status();
        serverWebExchange.getAttributes().put("skip", SKIP.get());
        return webFilterChain.filter(serverWebExchange);
    }

    /**
     * Change OAuth2 skip status.
     */
    public void changeSkipOAuth2Status() {
        PluginData pluginData = BaseDataCache.getInstance().obtainPluginData("oauth2");
        if (pluginData == null) {
            return;
        }
        boolean expect = SKIP.get();
        Boolean skipStatus = pluginData.getEnabled();
        while (!SKIP.compareAndSet(expect, skipStatus)) {
            log.info("try to change OAuth2 skip status to {}", skipStatus);
        }
        log.info("change OAuth2 skip status to {}", skipStatus);
        matchers.clear();
        matchers.add(new PathPatternParserServerWebExchangeMatcher("/"));
    }
}
