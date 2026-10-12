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

package org.apache.shenyu.plugin.divide;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.enums.HttpRetryBackoffSpecEnum;
import org.apache.shenyu.common.enums.LoadBalanceEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RetryEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.RequestUrlUtils;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.circuitbreaker.UpstreamCircuitBreaker;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.base.utils.LoadbalancerUtils;
import org.apache.shenyu.plugin.divide.handler.DividePluginDataHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * Divide Plugin.
 */
public class DividePlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(DividePlugin.class);

    private static final String P2C = "p2c";

    private static final String SHORTEST_RESPONSE = "shortestResponse";

    @Override
    protected String getRawPath(final ServerWebExchange exchange) {
        return RequestUrlUtils.getRewrittenRawPath(exchange);
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        Objects.requireNonNull(shenyuContext);
        DivideRuleHandle ruleHandle = buildRuleHandle(rule);
        if (ruleHandle.getHeaderMaxSize() > 0) {
            long headerSize = exchange.getRequest().getHeaders().values()
                    .stream()
                    .flatMap(Collection::stream)
                    .mapToLong(header -> header.getBytes(StandardCharsets.UTF_8).length)
                    .sum();
            if (headerSize > ruleHandle.getHeaderMaxSize()) {
                LOG.error("request header is too large");
                Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.REQUEST_HEADER_TOO_LARGE);
                return WebFluxResultUtils.result(exchange, error);
            }
        }
        if (ruleHandle.getRequestMaxSize() > 0) {
            if (exchange.getRequest().getHeaders().getContentLength() > ruleHandle.getRequestMaxSize()) {
                LOG.error("request entity is too large");
                Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.REQUEST_ENTITY_TOO_LARGE);
                return WebFluxResultUtils.result(exchange, error);
            }
        }
        List<Upstream> upstreamList = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId(selector.getId());
        if (CollectionUtils.isEmpty(upstreamList)) {
            LOG.error("divide upstream configuration error： {}", selector);
            Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.CANNOT_FIND_HEALTHY_UPSTREAM_URL);
            return WebFluxResultUtils.result(exchange, error);
        }
        final List<Upstream> breakeredUpstreamList = filterByCircuitBreaker(selector.getId(), upstreamList);
        if (CollectionUtils.isEmpty(breakeredUpstreamList)) {
            LOG.warn("all upstreams of selector {} are blocked by the circuit breaker, fail fast", selector.getId());
            Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.CANNOT_FIND_HEALTHY_UPSTREAM_URL);
            return WebFluxResultUtils.result(exchange, error);
        }
        List<String> specifyDomains = exchange.getRequest().getHeaders().get(Constants.SPECIFY_DOMAIN);
        Upstream upstream;
        if (CollectionUtils.isNotEmpty(specifyDomains)) {
            String requested = specifyDomains.get(0);
            upstream = breakeredUpstreamList.stream()
                    .filter(u -> u.getUrl().equals(requested))
                    .findFirst()
                    .map(u -> Upstream.builder()
                            .url(u.getUrl())
                            .protocol(u.getProtocol())
                            .weight(u.getWeight())
                            .warmup(u.getWarmup())
                            .status(u.isStatus())
                            .build())
                    .orElseGet(() -> LoadbalancerUtils.getForExchange(breakeredUpstreamList, ruleHandle.getLoadBalance(), exchange));
        } else {
            upstream = LoadbalancerUtils.getForExchange(breakeredUpstreamList, ruleHandle.getLoadBalance(), exchange);
        }
        if (Objects.isNull(upstream)) {
            LOG.error("divide has no upstream");
            Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.CANNOT_FIND_HEALTHY_UPSTREAM_URL);
            return WebFluxResultUtils.result(exchange, error);
        }
        // set domain
        String domain = upstream.buildDomain();
        exchange.getAttributes().put(Constants.HTTP_DOMAIN, domain);
        // set the http timeout
        exchange.getAttributes().put(Constants.HTTP_TIME_OUT, ruleHandle.getTimeout());
        exchange.getAttributes().put(Constants.HTTP_RETRY, ruleHandle.getRetry());
        // set retry strategy stuff
        exchange.getAttributes().put(Constants.HTTP_RETRY_BACK_OFF_SPEC, StringUtils.defaultIfEmpty(ruleHandle.getRetryBackOffSpec(), HttpRetryBackoffSpecEnum.getDefault()));
        exchange.getAttributes().put(Constants.RETRY_STRATEGY, StringUtils.defaultIfEmpty(ruleHandle.getRetryStrategy(), RetryEnum.CURRENT.getName()));
        exchange.getAttributes().put(Constants.LOAD_BALANCE, StringUtils.defaultIfEmpty(ruleHandle.getLoadBalance(), LoadBalanceEnum.RANDOM.getName()));
        exchange.getAttributes().put(Constants.DIVIDE_SELECTOR_ID, selector.getId());
        return forwardWithCircuitBreaker(exchange, chain, ruleHandle, selector.getId(), upstream);
    }

    @Override
    public String named() {
        return PluginEnum.DIVIDE.getName();
    }

    @Override
    public boolean skip(final ServerWebExchange exchange) {
        return skipExcept(exchange, RpcTypeEnum.HTTP);
    }

    @Override
    public int getOrder() {
        return PluginEnum.DIVIDE.getCode();
    }

    @Override
    protected Mono<Void> handleSelectorIfNull(final String pluginName, final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        return WebFluxResultUtils.noSelectorResult(pluginName, exchange);
    }

    @Override
    protected Mono<Void> handleRuleIfNull(final String pluginName, final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        return WebFluxResultUtils.noRuleResult(pluginName, exchange);
    }

    /**
     * Forward the request through the plugin chain and record its outcome into
     * the built-in circuit breaker, keeping the load-balance specific triggers.
     *
     * @param exchange the current server exchange
     * @param chain the plugin chain
     * @param ruleHandle the divide rule handle
     * @param selectorId the selector id
     * @param upstream the chosen upstream
     * @return {@code Mono<Void>} to indicate when request processing is complete
     */
    private Mono<Void> forwardWithCircuitBreaker(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                                 final DivideRuleHandle ruleHandle, final String selectorId, final Upstream upstream) {
        String breakerKey = UpstreamCircuitBreaker.buildKey(selectorId, upstream);
        if (ruleHandle.getLoadBalance().equals(P2C)) {
            return UpstreamCircuitBreaker.recordOutcome(chain.execute(exchange)
                    .doFinally(signalType -> responseTrigger(upstream)), breakerKey);
        } else if (ruleHandle.getLoadBalance().equals(SHORTEST_RESPONSE)) {
            long beginTime = System.currentTimeMillis();
            return UpstreamCircuitBreaker.recordOutcome(chain.execute(exchange)
                    .doOnSuccess(e -> successResponseTrigger(upstream, beginTime)), breakerKey);
        }
        return UpstreamCircuitBreaker.recordOutcome(chain.execute(exchange), breakerKey);
    }

    private DivideRuleHandle buildRuleHandle(final RuleData rule) {
        return DividePluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
    }

    /**
     * Filter out upstreams blocked by the built-in circuit breaker.
     *
     * <p>When every upstream of the selector is blocked, a single half-open
     * probe request is granted to one of them so the breaker can recover;
     * otherwise the caller should fail fast.
     *
     * @param selectorId the selector id the upstreams belong to
     * @param upstreamList the healthy upstream list from the cache
     * @return the upstreams allowed to receive requests, possibly empty
     */
    private List<Upstream> filterByCircuitBreaker(final String selectorId, final List<Upstream> upstreamList) {
        List<Upstream> allowed = new ArrayList<>(upstreamList.size());
        for (Upstream upstream : upstreamList) {
            if (UpstreamCircuitBreaker.getInstance().isRequestAllowed(UpstreamCircuitBreaker.buildKey(selectorId, upstream))) {
                allowed.add(upstream);
            }
        }
        if (CollectionUtils.isNotEmpty(allowed)) {
            return allowed;
        }
        for (Upstream upstream : upstreamList) {
            if (UpstreamCircuitBreaker.getInstance().tryAcquireHalfOpenProbe(UpstreamCircuitBreaker.buildKey(selectorId, upstream))) {
                return Collections.singletonList(upstream);
            }
        }
        return Collections.emptyList();
    }

    private void responseTrigger(final Upstream upstream) {
        long now = System.currentTimeMillis();
        upstream.getInflight().decrementAndGet();
        long stamp = upstream.getResponseStamp();
        upstream.setResponseStamp(now);
        long td = now - stamp;
        if (td < 0) {
            td = 0;
        }
        double w = Math.exp((double) -td / (double) 600);

        long lag = now - upstream.getLastPicked();
        if (lag < 0) {
            lag = 0;
        }
        long oldLag = upstream.getLag();
        if (oldLag == 0) {
            w = 0;
        }
        lag = (int) ((double) oldLag * w + (double) lag * (1.0 - w));
        upstream.setLag(lag);
    }

    private void successResponseTrigger(final Upstream upstream, final long beginTime) {
        upstream.getSucceededElapsed().addAndGet(System.currentTimeMillis() - beginTime);
        upstream.getSucceeded().incrementAndGet();
    }

}
