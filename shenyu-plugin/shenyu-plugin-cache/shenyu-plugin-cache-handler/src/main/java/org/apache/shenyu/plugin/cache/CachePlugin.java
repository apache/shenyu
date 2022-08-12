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

package org.apache.shenyu.plugin.cache;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.CacheRuleHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.cache.handler.CachePluginDataHandler;
import org.apache.shenyu.plugin.cache.utils.CacheUtils;
import org.reactivestreams.Publisher;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.MediaType;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;
import reactor.util.annotation.NonNull;

import java.util.Objects;
import java.util.Optional;

/**
 * CacheWritePlugin.
 */
public class CachePlugin extends AbstractShenyuPlugin {

    @Override
    public Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                final SelectorData selector, final RuleData rule) {
        ICache cache = CacheUtils.getCache();
        if (Objects.nonNull(cache)) {
            return cache.getData(CacheUtils.dataKey(exchange))
                    .zipWith(cache.getData(CacheUtils.contentTypeKey(exchange)))
                    .flatMap(data -> Mono.just(Optional.of(data)))
                    .defaultIfEmpty(Optional.empty())
                    .flatMap(dataFlg -> {
                        if (dataFlg.isPresent()) {
                            byte[] bytes = dataFlg.get().getT1();
                            byte[] contentTypeBytes = dataFlg.get().getT2();
                            cache.setContentType(exchange, contentTypeBytes);
                            return exchange.getResponse().writeWith(Mono.just(exchange.getResponse().bufferFactory().wrap(bytes))
                                    .doOnNext(data -> exchange.getResponse().getHeaders().setContentLength(data.readableByteCount())));
                        }
                        CacheRuleHandle cacheRuleHandle = CachePluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
                        return chain.execute(exchange.mutate().response(new CacheHttpResponse(exchange, cacheRuleHandle)).build());
                    });
        }
        CacheRuleHandle cacheRuleHandle = CachePluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        return chain.execute(exchange.mutate().response(new CacheHttpResponse(exchange, cacheRuleHandle)).build());
    }

    @Override
    public int getOrder() {
        return PluginEnum.CACHE.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.CACHE.getName();
    }

    static class CacheHttpResponse extends ServerHttpResponseDecorator {

        private final ServerWebExchange exchange;

        private final CacheRuleHandle cacheRuleHandle;

        CacheHttpResponse(final ServerWebExchange exchange,
                          final CacheRuleHandle cacheRuleHandle) {
            super(exchange.getResponse());
            this.exchange = exchange;
            this.cacheRuleHandle = cacheRuleHandle;
        }

        @Override
        @NonNull
        public Mono<Void> writeWith(@NonNull final Publisher<? extends DataBuffer> body) {
            return super.writeWith(cacheResponse(body));
        }

        @NonNull
        private Flux<? extends DataBuffer> cacheResponse(final Publisher<? extends DataBuffer> body) {
            final ICache cache = CacheUtils.getCache();
            if (Objects.nonNull(cache)) {
                final MediaType contentType = this.getHeaders().getContentType();
                return Flux.from(body).doOnNext(buffer -> {
                    cache.cacheData(CacheUtils.dataKey(this.exchange), buffer.asByteBuffer().array(),
                            this.cacheRuleHandle.getTimeoutSeconds()).subscribeOn(Schedulers.boundedElastic()).subscribe();
                    cache.cacheContentType(CacheUtils.contentTypeKey(this.exchange), contentType, this.cacheRuleHandle.getTimeoutSeconds());
                });
            }
            return Flux.from(body);
        }
    }
}
