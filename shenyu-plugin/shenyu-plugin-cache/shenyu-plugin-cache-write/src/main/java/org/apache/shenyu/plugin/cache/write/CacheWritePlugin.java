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

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.CacheWriteRuleHandle;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.cache.base.config.CacheConfig;
import org.apache.shenyu.plugin.cache.base.enums.CacheEnum;
import org.apache.shenyu.plugin.cache.base.memory.MemoryCache;
import org.apache.shenyu.plugin.cache.base.redis.ShenyuCacheReactiveRedisTemplate;
import org.apache.shenyu.plugin.cache.write.handler.CacheWritePluginDataHandler;
import org.reactivestreams.Publisher;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.util.annotation.NonNull;

import java.util.Optional;

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
        ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        assert shenyuContext != null;
        String path = shenyuContext.getPath();
        CacheWriteRuleHandle cacheWriteRuleHandle = CacheWritePluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        return chain.execute(exchange.mutate()
                .response(new CacheWriteHttpResponse(exchange.getResponse(), path,  cacheWriteRuleHandle.getTimeoutSeconds())).build());
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

    class CacheWriteHttpResponse extends ServerHttpResponseDecorator {

        private final String path;

        private final long timeoutSeconds;

        CacheWriteHttpResponse(final ServerHttpResponse delegate, final String path, final long timeoutSeconds) {
            super(delegate);
            this.path = path;
            this.timeoutSeconds = timeoutSeconds;
        }

        @Override
        @NonNull
        public Mono<Void> writeWith(@NonNull final Publisher<? extends DataBuffer> body) {
            return super.writeWith(cacheWriteResponse(body));
        }

        @NonNull
        private Flux<? extends DataBuffer> cacheWriteResponse(final Publisher<? extends DataBuffer> body) {
            CacheConfig cacheConfig = Singleton.INST.get(CacheConfig.class);
            if (CacheEnum.REDIS.getName().equals(cacheConfig.getMode())) {
                ShenyuCacheReactiveRedisTemplate shenyuCacheReactiveRedisTemplate = Singleton.INST.get(ShenyuCacheReactiveRedisTemplate.class);
                return Flux.from(body).doOnNext(buffer -> shenyuCacheReactiveRedisTemplate.cache(this.path, buffer.asByteBuffer().array(), this.timeoutSeconds));
            } else if (CacheEnum.MEMORY.getName().equals(cacheConfig.getMode())) {
                MemoryCache memoryCache = Singleton.INST.get(MemoryCache.class);
                return Flux.from(body).doOnNext(buffer -> memoryCache.cache(this.path, buffer.asByteBuffer().array(), this.timeoutSeconds));
            }
            return Flux.from(body);
        }
    }
}
