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

package org.apache.shenyu.plugin.redirect;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.RedirectHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.common.utils.UriUtils;
import org.apache.shenyu.plugin.redirect.handler.RedirectPluginDataHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.web.reactive.DispatcherHandler;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;

/**
 * Redirect Plugin.
 */
public class RedirectPlugin extends AbstractShenyuPlugin {

    public static final String ROOT_PATH_PREFIX = "/";

    private static final Logger LOG = LoggerFactory.getLogger(RedirectPlugin.class);

    private final DispatcherHandler dispatcherHandler;

    public RedirectPlugin(final DispatcherHandler dispatcherHandler) {
        this.dispatcherHandler = dispatcherHandler;
    }

    @Override
    public int getOrder() {
        return PluginEnum.REDIRECT.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.REDIRECT.getName();
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                   final SelectorData selector, final RuleData rule) {
        final String handle = rule.getHandle();
        final RedirectHandle redirectHandle = RedirectPluginDataHandler.CACHED_HANDLE.get()
                .obtainHandle(CacheKeyUtils.INST.getKey(rule));
        if (Objects.isNull(redirectHandle) || StringUtils.isBlank(redirectHandle.getRedirectURI())) {
            LOG.error("uri redirect rule can not configuration: {}", handle);
            return chain.execute(exchange);
        }
        if (redirectHandle.getRedirectURI().startsWith(ROOT_PATH_PREFIX)) {
            ServerHttpRequest request = exchange.getRequest().mutate()
                    .uri(Objects.requireNonNull(UriUtils.createUri(redirectHandle.getRedirectURI()))).build();
            ServerWebExchange mutated = exchange.mutate().request(request).build();
            return dispatcherHandler.handle(mutated);
        } else {
            ServerHttpResponse response = exchange.getResponse();
            response.setStatusCode(HttpStatus.PERMANENT_REDIRECT);
            response.getHeaders().add(HttpHeaders.LOCATION, redirectHandle.getRedirectURI());
            return response.setComplete();
        }
    }
}
