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
import org.springframework.http.HttpStatus;
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
     * The error code returned to a client whose request was rejected. It follows the waf plugin,
     * which rejects with 403 as well.
     */
    private static final int SENSITIVE_WORD_CODE = HttpStatus.FORBIDDEN.value();

    /**
     * The message returned to a client whose request was rejected. It never contains the matched
     * words: echoing them back would confirm the dictionary to the caller and would re-emit
     * forbidden content into the caller side logs.
     */
    private static final String REJECT_MESSAGE = "Request rejected: sensitive content detected";

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
        return dictionary(exchange, redisTemplate, handle)
                .map(automaton -> automaton.search(body))
                .flatMap(matches -> {
                    if (matches.isEmpty()) {
                        return Mono.just(body);
                    }
                    // The matches are logged server side only: returning them to the caller would
                    // turn the gateway into an oracle of the dictionary and would push content that
                    // was just classified as forbidden into the caller side logs.
                    LOG.warn("sensitive word plugin: the request was rejected, matched words: {}", matches);
                    return Mono.error(new ResponsiveException(SENSITIVE_WORD_CODE, REJECT_MESSAGE, exchange));
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
    private Mono<AhoCorasick> dictionary(final ServerWebExchange exchange,
                                         final ReactiveRedisTemplate<String, String> redisTemplate,
                                         final SensitiveWordHandle handle) {
        String redisKey = Objects.isNull(handle.getRedisKey())
                ? SensitiveWordHandle.DEFAULT_REDIS_KEY : handle.getRedisKey();
        CachedDictionary cached = SensitiveWordPluginDataHandler.DICTIONARIES.get().obtainHandle(redisKey);
        Mono<AhoCorasick> automaton;
        if (Objects.nonNull(cached) && !cached.isExpired(handle.getRefreshIntervalSeconds())) {
            automaton = Mono.just(cached.getAutomaton());
        } else {
            automaton = redisTemplate.opsForSet()
                    .members(redisKey)
                    .collectList()
                    // building the automaton is cpu bound, keep it away from the event loop
                    .map(AhoCorasick::of)
                    .subscribeOn(Schedulers.boundedElastic())
                    .doOnNext(loaded -> SensitiveWordPluginDataHandler.DICTIONARIES.get()
                            .cachedHandle(redisKey, new CachedDictionary(loaded)))
                    .onErrorResume(error -> {
                        LOG.error("sensitive word plugin: cannot read the dictionary from redis key {}", redisKey, error);
                        if (Objects.nonNull(cached)) {
                            return Mono.just(cached.getAutomaton());
                        }
                        if (handle.isFailClosed()) {
                            LOG.warn("sensitive word plugin: no dictionary is available and failClosed is set,"
                                    + " the request is rejected");
                            return Mono.error(new ResponsiveException(SENSITIVE_WORD_CODE, REJECT_MESSAGE, exchange));
                        }
                        // fail open: a broken redis must not take the traffic down
                        LOG.warn("sensitive word plugin: no dictionary is available, the request is passed through");
                        return Mono.just(AhoCorasick.empty());
                    });
        }
        // The cached path emits on the thread that subscribes to it, which is the netty event loop
        // of the request being filtered: hop once here so that the scan below never runs on it.
        return automaton.publishOn(Schedulers.boundedElastic());
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
