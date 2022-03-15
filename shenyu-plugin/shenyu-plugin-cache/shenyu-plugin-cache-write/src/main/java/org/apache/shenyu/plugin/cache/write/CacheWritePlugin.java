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

package org.apache.shenyu.plugin.cache.write;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.CacheWriteRuleHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.cache.base.ICache;
import org.apache.shenyu.plugin.cache.base.config.CacheConfig;
import org.apache.shenyu.plugin.cache.base.enums.CacheEnum;
import org.apache.shenyu.plugin.cache.base.memory.MemoryCache;
import org.apache.shenyu.plugin.cache.base.redis.ShenyuCacheReactiveRedisTemplate;
import org.apache.shenyu.plugin.cache.base.utils.CacheKeys;
import org.apache.shenyu.plugin.cache.write.handler.CacheWritePluginDataHandler;
import org.reactivestreams.Publisher;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.util.annotation.NonNull;

import java.util.Objects;

/**
 * CacheWritePlugin.
 */
public class CacheWritePlugin extends AbstractShenyuPlugin {

    /**
     * Process the Web request and (optionally) delegate to the next
     * {@code WebFilter} through the given {@link ShenyuPluginChain}.
     *
     * @param exchange the current server exchange
     * @param chain    provides a way to delegate to the next filter
     * @return {@code Mono<Void>} to indicate when request processing is complete
     */
    @Override
    protected Mono<Void> doExecute(ServerWebExchange exchange, ShenyuPluginChain chain, SelectorData selector, RuleData rule) {
        CacheConfig cacheConfig = Singleton.INST.get(CacheConfig.class);
        assert cacheConfig != null;
        CacheWriteRuleHandle cacheWriteRuleHandle = CacheWritePluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        return chain.execute(exchange.mutate()
                .response(new CacheWriteHttpResponse(exchange, exchange.getResponse(), cacheConfig, cacheWriteRuleHandle)).build());
    }

    /**
     * return plugin order .
     * This attribute To determine the plugin execution order in the same type plugin.
     *
     * @return int order
     */
    @Override
    public int getOrder() {
        return PluginEnum.CACHE_WRITE.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.CACHE_WRITE.getName();
    }

    static class CacheWriteHttpResponse extends ServerHttpResponseDecorator {

        private final ServerWebExchange exchange;

        private final CacheConfig cacheConfig;

        private final CacheWriteRuleHandle cacheWriteRuleHandle;

        CacheWriteHttpResponse(final ServerWebExchange exchange, final ServerHttpResponse delegate, final CacheConfig cacheConfig, final CacheWriteRuleHandle cacheWriteRuleHandle) {
            super(delegate);
            this.exchange = exchange;
            this.cacheConfig = cacheConfig;
            this.cacheWriteRuleHandle = cacheWriteRuleHandle;
        }

        @Override
        @NonNull
        public Mono<Void> writeWith(@NonNull final Publisher<? extends DataBuffer> body) {
            return super.writeWith(cacheWriteResponse(body));
        }

        @NonNull
        private Flux<? extends DataBuffer> cacheWriteResponse(final Publisher<? extends DataBuffer> body) {
            final String dataKey = CacheKeys.dataKey(this.exchange);
            ICache cache = null;
            if (CacheEnum.REDIS.getName().equals(cacheConfig.getMode())) {
                cache = Singleton.INST.get(ShenyuCacheReactiveRedisTemplate.class);
            } else if (CacheEnum.MEMORY.getName().equals(cacheConfig.getMode())) {
                cache = Singleton.INST.get(MemoryCache.class);
            }
            if (Objects.nonNull(cache)) {
                final ICache finalCache = cache;
                // TODO @z cache context type from client response
                // cache.cacheContextType()
                return Flux.from(body).doOnNext(buffer -> finalCache.cacheData(dataKey, buffer.asByteBuffer().array(), this.cacheWriteRuleHandle.getTimeoutSeconds()));
            }
            return Flux.from(body);
        }
    }
}
