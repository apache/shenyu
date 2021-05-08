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

package org.apache.shenyu.plugin.sentinel;

import com.alibaba.csp.sentinel.adapter.reactor.SentinelReactorTransformer;
import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.SentinelHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.api.SoulPluginChain;
import org.apache.shenyu.plugin.api.context.SoulContext;
import org.apache.shenyu.plugin.base.AbstractSoulPlugin;
import org.apache.shenyu.plugin.base.utils.UriUtils;
import org.apache.shenyu.plugin.sentinel.fallback.SentinelFallbackHandler;
import org.apache.shenyu.plugin.sentinel.handler.SentinelRuleHandle;
import org.springframework.http.HttpStatus;
import org.springframework.web.client.HttpStatusCodeException;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

/**
 * Sentinel Plugin.
 *
 * @author tydhot
 */
@Slf4j
public class SentinelPlugin extends AbstractSoulPlugin {

    private final SentinelFallbackHandler sentinelFallbackHandler;

    public SentinelPlugin(final SentinelFallbackHandler sentinelFallbackHandler) {
        this.sentinelFallbackHandler = sentinelFallbackHandler;
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final SoulPluginChain chain, final SelectorData selector, final RuleData rule) {
        final SoulContext soulContext = exchange.getAttribute(Constants.CONTEXT);
        assert soulContext != null;
        String resourceName = SentinelRuleHandle.getResourceName(rule);
        SentinelHandle sentinelHandle = GsonUtils.getInstance().fromJson(rule.getHandle(), SentinelHandle.class);
        sentinelHandle.checkData(sentinelHandle);
        return chain.execute(exchange).transform(new SentinelReactorTransformer<>(resourceName)).doOnSuccess(v -> {
            HttpStatus status = exchange.getResponse().getStatusCode();
            if (status == null || !status.is2xxSuccessful()) {
                exchange.getResponse().setStatusCode(null);
                throw new SentinelFallbackException(status == null ? HttpStatus.INTERNAL_SERVER_ERROR : status);
            }
        }).onErrorResume(throwable -> sentinelFallbackHandler.fallback(exchange, UriUtils.createUri(sentinelHandle.getFallbackUri()), throwable));
    }

    @Override
    public String named() {
        return PluginEnum.SENTINEL.getName();
    }

    @Override
    public int getOrder() {
        return PluginEnum.SENTINEL.getCode();
    }

    public static class SentinelFallbackException extends HttpStatusCodeException {

        public SentinelFallbackException(final HttpStatus statusCode) {
            super(statusCode);
        }
    }
}
