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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.CacheRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.CacheUpstream;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.cache.cache.ApplicationConfigCache;
import org.apache.shenyu.plugin.cache.handler.CachePluginDataHandler;
import org.apache.shenyu.plugin.cache.utils.CacheUtils;
import org.reactivestreams.Publisher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.server.reactive.ServerHttpRequestDecorator;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;
import reactor.util.annotation.NonNull;

import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Caches responses only for bodyless GET requests.
 */
public class CachePlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(CachePlugin.class);

    private static final Set<String> ORIGIN_REQUEST_HEADERS = Set.of(HttpHeaders.AUTHORIZATION, HttpHeaders.COOKIE, HttpHeaders.RANGE,
            HttpHeaders.IF_MATCH, HttpHeaders.IF_NONE_MATCH, HttpHeaders.IF_MODIFIED_SINCE, HttpHeaders.IF_UNMODIFIED_SINCE, HttpHeaders.IF_RANGE,
            HttpHeaders.CACHE_CONTROL, HttpHeaders.PRAGMA);

    @Override
    public Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                final SelectorData selector, final RuleData rule) {
        if (!HttpMethod.GET.equals(exchange.getRequest().getMethod())
                || exchange.getRequest().getHeaders().getContentLength() > 0
                || exchange.getRequest().getHeaders().containsKey(HttpHeaders.TRANSFER_ENCODING)
                || ORIGIN_REQUEST_HEADERS.stream().anyMatch(exchange.getRequest().getHeaders()::containsKey)) {
            return chain.execute(exchange);
        }
        final CacheRuleHandle cacheRuleHandle = buildRuleHandle(rule);
        if (Objects.isNull(cacheRuleHandle)) {
            return chain.execute(exchange);
        }
        return exchange.getRequest().getBody().switchOnFirst((signal, body) -> {
            if (signal.isOnError()) {
                return Mono.error(Objects.requireNonNull(signal.getThrowable()));
            }
            if (signal.hasValue()) {
                ServerHttpRequestDecorator request = new ServerHttpRequestDecorator(exchange.getRequest()) {
                    @Override
                    @NonNull
                    public Flux<DataBuffer> getBody() {
                        return body;
                    }
                };
                return Mono.defer(() -> chain.execute(exchange.mutate().request(request).build()));
            }
            return executeCache(exchange, chain, selector, cacheRuleHandle, rule.getId());
        }).doOnDiscard(DataBuffer.class, DataBufferUtils::release).then();
    }

    private Mono<Void> executeCache(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                   final SelectorData selector, final CacheRuleHandle cacheRuleHandle, final String ruleId) {
        CacheUpstream cacheUpstream = GsonUtils.getInstance().fromJson(selector.getHandle(), CacheUpstream.class);
        final String responseKey = CacheUtils.responseKey(exchange, selector.getId(), ruleId);
        ICache cache = getCache(selector.getId(), cacheUpstream);
        if (Objects.nonNull(cache)) {
            return cache.getData(responseKey)
                    .map(CachedResponse::decode)
                    .defaultIfEmpty(Optional.empty())
                    .flatMap(dataFlg -> {
                        if (dataFlg.isPresent()) {
                            return dataFlg.get().writeTo(exchange.getResponse());
                        }
                        return chain.execute(exchange.mutate().response(new CacheHttpResponse(exchange, cacheRuleHandle, selector.getId(), responseKey)).build());
                    });
        }
        return chain.execute(exchange.mutate().response(new CacheHttpResponse(exchange, cacheRuleHandle, selector.getId(), responseKey)).build());
    }

    /**
     * get and init ICache.
     *
     * @param selectorId selectorId
     * @param cacheUpstream cacheUpstream
     * @return ICache
     */
    private ICache getCache(final String selectorId, final CacheUpstream cacheUpstream) {
        if (Objects.isNull(cacheUpstream) || StringUtils.isBlank(cacheUpstream.getUrl())) {
            return CacheUtils.getCache();
        }
        ICache cache = ApplicationConfigCache.getInstance().get(selectorId);
        if (Objects.isNull(cache)) {
            cache = ApplicationConfigCache.getInstance().init(selectorId, cacheUpstream);
        }
        return cache;
    }

    @Override
    public int getOrder() {
        return PluginEnum.CACHE.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.CACHE.getName();
    }
    
    private CacheRuleHandle buildRuleHandle(final RuleData rule) {
        return CachePluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
    }

    static class CacheHttpResponse extends ServerHttpResponseDecorator {

        private final String selectorId;

        private final String responseKey;

        private final CacheRuleHandle cacheRuleHandle;

        CacheHttpResponse(final ServerWebExchange exchange,
                          final CacheRuleHandle cacheRuleHandle,
                          final String selectorId, final String responseKey) {
            super(exchange.getResponse());
            this.responseKey = responseKey;
            this.cacheRuleHandle = cacheRuleHandle;
            this.selectorId = selectorId;
        }

        @Override
        @NonNull
        public Mono<Void> writeWith(@NonNull final Publisher<? extends DataBuffer> body) {
            return DataBufferUtils.join(body).map(dataBuffer -> {
                byte[] bytes = new byte[dataBuffer.readableByteCount()];
                try {
                    dataBuffer.read(bytes);
                } finally {
                    DataBufferUtils.release(dataBuffer);
                }
                return bytes;
            }).defaultIfEmpty(new byte[0]).flatMap(this::writeBody);
        }

        @Override
        @NonNull
        public Mono<Void> setComplete() {
            return writeBody(new byte[0]);
        }

        private Mono<Void> writeBody(final byte[] bytes) {
            return Mono.defer(() -> {
                if (isCommitted()) {
                    return Mono.empty();
                }
                boolean noBody = HttpStatus.NO_CONTENT.equals(getStatusCode()) || HttpStatus.NOT_MODIFIED.equals(getStatusCode());
                byte[] body = noBody ? new byte[0] : bytes;
                getHeaders().remove(HttpHeaders.TRANSFER_ENCODING);
                if (noBody) {
                    getHeaders().remove(HttpHeaders.CONTENT_LENGTH);
                } else {
                    getHeaders().setContentLength(body.length);
                }
                AtomicReference<CachedResponse> captured = new AtomicReference<>();
                getDelegate().beforeCommit(() -> {
                    try {
                        captured.set(CachedResponse.capture(getDelegate(), body).orElse(null));
                    } catch (RuntimeException ex) {
                        LOG.warn("Failed to capture response cache metadata", ex);
                    }
                    return Mono.empty();
                });
                Mono<Void> write = body.length == 0 ? super.setComplete() : super.writeWith(Mono.just(bufferFactory().wrap(body)));
                return write.doOnSuccess(ignored -> Optional.ofNullable(captured.get()).ifPresent(this::cacheResponse));
            });
        }

        private void cacheResponse(final CachedResponse response) {
            try {
                ICache selectorCache = ApplicationConfigCache.getInstance().get(selectorId);
                ICache cache = Objects.isNull(selectorCache) ? CacheUtils.getCache() : selectorCache;
                if (Objects.nonNull(cache)) {
                    cache.cacheData(responseKey, response.encode(), cacheRuleHandle.getTimeoutSeconds())
                            .subscribeOn(Schedulers.boundedElastic())
                            .subscribe(ignored -> { }, error -> LOG.warn("Failed to store response cache entry", error));
                }
            } catch (RuntimeException ex) {
                LOG.warn("Failed to store response cache entry", ex);
            }
        }
    }
}
