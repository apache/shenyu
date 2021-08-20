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
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.SentinelHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.fallback.FallbackHandler;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.base.utils.UriUtils;
import org.springframework.http.HttpStatus;
import org.springframework.web.client.HttpStatusCodeException;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

/**
 * Sentinel Plugin.
 */
public class SentinelPlugin extends AbstractShenyuPlugin {

    private final FallbackHandler fallbackHandler;

    public SentinelPlugin(final FallbackHandler fallbackHandler) {
        this.fallbackHandler = fallbackHandler;
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        final ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        assert shenyuContext != null;
        String resourceName = CacheKeyUtils.INST.getKey(rule);
        SentinelHandle sentinelHandle = GsonUtils.getInstance().fromJson(rule.getHandle(), SentinelHandle.class);
        sentinelHandle.checkData(sentinelHandle);
        return chain.execute(exchange).transform(new SentinelReactorTransformer<>(resourceName)).doOnSuccess(v -> {
            HttpStatus status = exchange.getResponse().getStatusCode();
            if (status == null || !status.is2xxSuccessful()) {
                exchange.getResponse().setStatusCode(null);
                throw new SentinelFallbackException(status == null ? HttpStatus.INTERNAL_SERVER_ERROR : status);
            }
        }).onErrorResume(throwable -> fallbackHandler.fallback(exchange, UriUtils.createUri(sentinelHandle.getFallbackUri()), throwable));
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
