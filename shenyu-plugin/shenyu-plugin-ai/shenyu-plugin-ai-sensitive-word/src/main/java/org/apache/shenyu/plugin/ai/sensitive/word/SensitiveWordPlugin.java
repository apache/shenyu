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

package org.apache.shenyu.plugin.ai.sensitive.word;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.SensitiveWordHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.exception.ResponsiveException;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.base.utils.ServerWebExchangeUtils;
import org.apache.shenyu.plugin.ai.sensitive.word.ac.AhoCorasick;
import org.apache.shenyu.plugin.ai.sensitive.word.handler.SensitiveWordPluginDataHandler;
import org.apache.shenyu.plugin.ai.sensitive.word.handler.SensitiveWordPluginDataHandler.CachedDictionary;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import org.springframework.http.codec.HttpMessageReader;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

import java.util.List;
import java.util.Objects;

/**
 * The sensitive word plugin, it rejects the request when its body contains a word of the dictionary
 * configured for the matched rule.
 *
 * <p>The dictionary is read from a redis set, so it can be maintained outside of shenyu. A loaded
 * dictionary is reused for the refresh interval of the rule handle, see {@link SensitiveWordHandle}.
 */
public class SensitiveWordPlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(SensitiveWordPlugin.class);

    /**
     * The error code returned to the client when the request contains sensitive words.
     */
    private static final int SENSITIVE_WORD_CODE = 1500;

    private final List<HttpMessageReader<?>> readers;

    public SensitiveWordPlugin(final List<HttpMessageReader<?>> readers) {
        this.readers = readers;
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange,
                                   final ShenyuPluginChain chain,
                                   final SelectorData selector,
                                   final RuleData rule) {
        SensitiveWordHandle handle = SensitiveWordPluginDataHandler.CACHED_HANDLE.get()
                .obtainHandle(CacheKeyUtils.INST.getKey(rule));
        if (Objects.isNull(handle)) {
            return chain.execute(exchange);
        }
        ReactiveRedisTemplate<String, String> redisTemplate = SensitiveWordPluginDataHandler.REDIS_TEMPLATES.get()
                .obtainHandle(SensitiveWordPluginDataHandler.PLUGIN_NAME);
        if (Objects.isNull(redisTemplate)) {
            LOG.warn("sensitive word plugin: the redis template is not initialized, skip the sensitive word check");
            return chain.execute(exchange);
        }
        return ServerWebExchangeUtils.rewriteRequestBody(exchange, readers,
                        body -> check(exchange, redisTemplate, handle, body))
                .flatMap(chain::execute)
                .onErrorResume(error -> {
                    if (error instanceof ResponsiveException) {
                        return WebFluxResultUtils.failedResult((ResponsiveException) error);
                    }
                    return Mono.error(error);
                });
    }

    private Mono<String> check(final ServerWebExchange exchange,
                               final ReactiveRedisTemplate<String, String> redisTemplate,
                               final SensitiveWordHandle handle,
                               final String body) {
        return dictionary(redisTemplate, handle)
                .map(automaton -> automaton.search(body))
                .flatMap(matches -> {
                    if (matches.isEmpty()) {
                        return Mono.just(body);
                    }
                    return Mono.error(new ResponsiveException(SENSITIVE_WORD_CODE,
                            String.format("The request contains sensitive words: %s", matches), exchange));
                });
    }

    /**
     * Get the dictionary of the rule, it is read from redis when the cached one is missing or
     * expired. The request is never blocked by the dictionary: a dictionary that cannot be read
     * is treated as empty, so that a broken redis never stops the traffic.
     *
     * @param redisTemplate the redis template
     * @param handle the rule handle
     * @return the automaton of the dictionary
     */
    private Mono<AhoCorasick> dictionary(final ReactiveRedisTemplate<String, String> redisTemplate,
                                         final SensitiveWordHandle handle) {
        String redisKey = Objects.isNull(handle.getRedisKey())
                ? SensitiveWordHandle.DEFAULT_REDIS_KEY : handle.getRedisKey();
        CachedDictionary cached = SensitiveWordPluginDataHandler.DICTIONARIES.get().obtainHandle(redisKey);
        if (Objects.nonNull(cached) && !cached.isExpired(handle.getRefreshIntervalSeconds())) {
            return Mono.just(cached.getAutomaton());
        }
        return redisTemplate.opsForSet()
                .members(redisKey)
                .collectList()
                // building the automaton is cpu bound, keep it away from the event loop
                .map(AhoCorasick::of)
                .subscribeOn(Schedulers.boundedElastic())
                .doOnNext(automaton -> SensitiveWordPluginDataHandler.DICTIONARIES.get()
                        .cachedHandle(redisKey, new CachedDictionary(automaton)))
                .onErrorResume(error -> {
                    LOG.error("sensitive word plugin: cannot read the dictionary from redis key {}", redisKey, error);
                    return Mono.just(Objects.isNull(cached) ? AhoCorasick.empty() : cached.getAutomaton());
                });
    }

    @Override
    public String named() {
        return PluginEnum.SENSITIVE_WORD.getName();
    }

    @Override
    public int getOrder() {
        return PluginEnum.SENSITIVE_WORD.getCode();
    }
}
