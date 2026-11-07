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
import org.apache.shenyu.common.dto.convert.rule.canary.CanaryConfig;
import org.apache.shenyu.common.dto.convert.rule.sticky.DeterministicKeyConfig;
import org.apache.shenyu.common.dto.convert.rule.sticky.StickySessionConfig;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.enums.HttpRetryBackoffSpecEnum;
import org.apache.shenyu.common.enums.LoadBalanceEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RetryEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.CanaryContext;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.RequestUrlUtils;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.circuitbreaker.UpstreamCircuitBreaker;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.base.utils.LoadbalancerUtils;
import org.apache.shenyu.plugin.base.utils.UpstreamRoutingContext;
import org.apache.shenyu.plugin.divide.canary.CanaryDecision;
import org.apache.shenyu.plugin.divide.canary.CanaryDecisionService;
import org.apache.shenyu.plugin.divide.canary.DefaultCanaryDecisionService;
import org.apache.shenyu.plugin.divide.handler.DividePluginDataHandler;
import org.apache.shenyu.loadbalancer.factory.LoadBalancerFactory;
import org.apache.shenyu.loadbalancer.entity.LoadBalanceData;
import org.apache.shenyu.plugin.divide.sticky.DeterministicKeyExtractor;
import org.apache.shenyu.plugin.divide.sticky.StickySessionAffinity;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.server.reactive.ServerHttpRequestDecorator;
import org.springframework.lang.NonNull;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Consumer;

/**
 * Divide Plugin.
 */
public class DividePlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(DividePlugin.class);

    private static final String P2C = "p2c";

    private static final String LEAST_ACTIVE = "leastActive";

    private static final String SHORTEST_RESPONSE = "shortestResponse";

    private final CanaryDecisionService canaryDecisionService;

    private final StickySessionAffinity stickySessionAffinity = new StickySessionAffinity();

    private final DeterministicKeyExtractor deterministicKeyExtractor = new DeterministicKeyExtractor();

    public DividePlugin() {
        this(new DefaultCanaryDecisionService());
    }

    public DividePlugin(final CanaryDecisionService canaryDecisionService) {
        this.canaryDecisionService = Objects.requireNonNull(canaryDecisionService);
    }

    @Override
    protected String getRawPath(final ServerWebExchange exchange) {
        return RequestUrlUtils.getRewrittenRawPath(exchange);
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        return Mono.defer(() -> executeRequest(exchange, chain, selector, rule));
    }

    private Mono<Void> executeRequest(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        // A re-entered exchange must not inherit a previous selector's partition.
        exchange.getAttributes().remove(UpstreamRoutingContext.ATTRIBUTE);
        exchange.getAttributes().remove(Constants.SHENYU_CANARY_CONTEXT);
        ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        Objects.requireNonNull(shenyuContext);
        DivideRuleHandle ruleHandle = buildRuleHandle(rule);
        final ServerWebExchange effectiveExchange = limitChunkedRequestBody(exchange, ruleHandle);
        if (ruleHandle.getHeaderMaxSize() > 0) {
            long headerSize = calculateHeaderSize(effectiveExchange.getRequest().getHeaders());
            if (headerSize > ruleHandle.getHeaderMaxSize()) {
                LOG.error("request header is too large");
                Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.REQUEST_HEADER_TOO_LARGE);
                return WebFluxResultUtils.result(exchange, error);
            }
        }
        if (ruleHandle.getRequestMaxSize() > 0) {
            if (effectiveExchange.getRequest().getHeaders().getContentLength() > ruleHandle.getRequestMaxSize()) {
                LOG.error("request entity is too large");
                Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.REQUEST_ENTITY_TOO_LARGE);
                return WebFluxResultUtils.result(exchange, error);
            }
        }
        boolean canaryRouting = Objects.nonNull(ruleHandle.getCanary())
                && CollectionUtils.isEmpty(effectiveExchange.getRequest().getHeaders().get(Constants.SPECIFY_DOMAIN));
        // Resolve the intent even for an empty pool so rejected requests can be attributed.
        List<Upstream> upstreamList = canaryRouting
                ? selectPartition(exchange, selector.getId(), rule.getId(), ruleHandle.getCanary())
                : UpstreamCacheManager.getInstance().findUpstreamListBySelectorId(selector.getId());
        if (CollectionUtils.isEmpty(upstreamList)) {
            if (canaryRouting) {
                exchange.getResponse().setStatusCode(HttpStatus.SERVICE_UNAVAILABLE);
                reportCanary(exchange);
            }
            LOG.error("divide upstream configuration error： {}", selector);
            Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.CANNOT_FIND_HEALTHY_UPSTREAM_URL);
            return WebFluxResultUtils.result(exchange, error);
        }
        String requested = effectiveExchange.getRequest().getHeaders().getFirst(Constants.SPECIFY_DOMAIN);
        final CircuitBreakerCandidates candidates = CircuitBreakerCandidates.filter(selector.getId(), upstreamList, requested);
        final List<Upstream> breakeredUpstreamList = candidates.getUpstreams();
        if (CollectionUtils.isEmpty(breakeredUpstreamList)) {
            if (canaryRouting) {
                exchange.getResponse().setStatusCode(HttpStatus.SERVICE_UNAVAILABLE);
                rejectCanary(exchange);
            }
            LOG.warn("all upstreams of selector {} are blocked by the circuit breaker, fail fast", selector.getId());
            Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.CANNOT_FIND_HEALTHY_UPSTREAM_URL);
            return WebFluxResultUtils.result(exchange, error);
        }
        return candidates.recordProbeOutcome(Mono.defer(() -> forwardSelected(effectiveExchange, chain, selector, rule, ruleHandle, candidates)));
    }

    private List<Upstream> selectPartition(final ServerWebExchange exchange, final String selectorId, final String ruleId,
                                           final CanaryConfig config) {
        long start = System.nanoTime();
        CanaryDecision decision = canaryDecisionService.decide(exchange, ruleId, config);
        long decisionDuration = System.nanoTime() - start;
        CanaryDecision actual = decision;
        Map<String, String> labels = decision == CanaryDecision.CANARY ? config.getCanaryLabels() : config.getStableLabels();
        List<Upstream> candidates = UpstreamCacheManager.getInstance().findUpstreamListByLabels(selectorId, labels);
        if (candidates.isEmpty() && decision == CanaryDecision.CANARY && "STABLE".equals(config.getFallbackPolicy())) {
            actual = CanaryDecision.STABLE;
            labels = config.getStableLabels();
            candidates = UpstreamCacheManager.getInstance().findUpstreamListByLabels(selectorId, labels);
        }
        String rejectReason = candidates.isEmpty()
                ? (actual == CanaryDecision.CANARY ? CanaryContext.CANARY_POOL_EMPTY : CanaryContext.STABLE_POOL_EMPTY) : null;
        exchange.getAttributes().put(Constants.SHENYU_CANARY_CONTEXT, new CanaryContext(selectorId, ruleId, decision.getName(), null,
                actual != decision && !candidates.isEmpty() ? CanaryContext.CANARY_POOL_EMPTY : null, rejectReason, decisionDuration));
        // Retry the resolved partition, including an initial fallback, without repeating the decision.
        exchange.getAttributes().put(UpstreamRoutingContext.ATTRIBUTE, new UpstreamRoutingContext(selectorId, labels));
        // Resolve the pool before circuit-breaker probes and load balancing; neither may escape it.
        return candidates;
    }

    private Mono<Void> forwardSelected(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector,
                                        final RuleData rule, final DivideRuleHandle ruleHandle, final CircuitBreakerCandidates candidates) {
        List<Upstream> breakeredUpstreamList = candidates.getUpstreams();
        exchange.getAttributes().put(UpstreamCircuitBreaker.HALF_OPEN_PROBE, candidates.isProbe());
        List<String> specifyDomains = exchange.getRequest().getHeaders().get(Constants.SPECIFY_DOMAIN);
        Upstream upstream;
        if (candidates.isProbe()) {
            upstream = breakeredUpstreamList.get(0);
            if (P2C.equals(ruleHandle.getLoadBalance())) {
                upstream.getInflight().incrementAndGet();
                upstream.setLastPicked(System.currentTimeMillis());
            }
        } else if (CollectionUtils.isNotEmpty(specifyDomains)) {
            // an explicit specify-domain request is an operations override and never affinity-bound
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
        } else if (stickySessionAffinity.isActive(ruleHandle.getSticky())) {
            upstream = resolveWithAffinity(exchange, rule, ruleHandle, breakeredUpstreamList);
            if (Objects.isNull(upstream)) {
                return WebFluxResultUtils.result(exchange,
                        ShenyuResultWrap.error(exchange, ShenyuResultEnum.CANNOT_FIND_HEALTHY_UPSTREAM_URL));
            }
        } else {
            upstream = LoadbalancerUtils.getForExchange(breakeredUpstreamList, ruleHandle.getLoadBalance(), exchange);
        }
        if (Objects.isNull(upstream)) {
            rejectCanary(exchange);
            LOG.error("divide has no upstream");
            Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.CANNOT_FIND_HEALTHY_UPSTREAM_URL);
            return WebFluxResultUtils.result(exchange, error);
        }
        if (LEAST_ACTIVE.equals(ruleHandle.getLoadBalance())) {
            // Least active compares outstanding requests, so the choice is reserved here and released
            // once the request terminates, whatever its outcome.
            upstream.getInflight().incrementAndGet();
        }
        // a sticky binding is committed for the target chosen here, before the response headers
        // are committed: a downstream retry to another target never rewrites the binding (#8037)
        if (stickySessionAffinity.isActive(ruleHandle.getSticky())
                && CollectionUtils.isEmpty(specifyDomains) && !candidates.isProbe()) {
            stickySessionAffinity.writeCookie(exchange, rule.getId(), upstream, ruleHandle.getSticky());
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
        CanaryContext observation = exchange.getAttribute(Constants.SHENYU_CANARY_CONTEXT);
        if (Objects.nonNull(observation)) {
            observation.setActualPartition(Objects.nonNull(observation.getFallbackReason()) ? CanaryDecision.STABLE.getName() : observation.getIntendedPartition());
            reportCanary(exchange);
        }
        return forwardWithCircuitBreaker(exchange, chain, ruleHandle, candidates.getBreakerKey(upstream), upstream, candidates.isProbe());
    }

    private void rejectCanary(final ServerWebExchange exchange) {
        CanaryContext context = exchange.getAttribute(Constants.SHENYU_CANARY_CONTEXT);
        if (Objects.nonNull(context)) {
            context.setActualPartition(null);
            context.setFallbackReason(null);
            context.setRejectReason(CanaryContext.NO_UPSTREAM_SELECTED);
            reportCanary(exchange);
        }
    }

    private void reportCanary(final ServerWebExchange exchange) {
        CanaryContext context = exchange.getAttribute(Constants.SHENYU_CANARY_CONTEXT);
        if (Objects.isNull(context)) {
            return;
        }
        LOG.debug("canary routing selector={} rule={} intended={} actual={} fallback={} reject={}", context.getSelectorId(), context.getRuleId(),
                context.getIntendedPartition(), context.getActualPartition(), context.getFallbackReason(), context.getRejectReason());
        Consumer<CanaryContext> callback = exchange.getAttribute(Constants.METRICS_CANARY);
        if (Objects.nonNull(callback)) {
            try {
                callback.accept(context);
            } catch (RuntimeException ex) {
                LOG.debug("Unable to report Canary routing observation", ex);
            }
        }
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
     * @param breakerKey the key already computed when filtering candidates
     * @param upstream the chosen upstream
     * @param probe whether the caller already attached exclusive probe accounting
     * @return {@code Mono<Void>} to indicate when request processing is complete
     */
    private Mono<Void> forwardWithCircuitBreaker(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                                 final DivideRuleHandle ruleHandle, final String breakerKey, final Upstream upstream, final boolean probe) {
        Mono<Void> response = Mono.defer(() -> chain.execute(exchange));
        if (ruleHandle.getLoadBalance().equals(LEAST_ACTIVE)) {
            response = response.doFinally(signalType -> upstream.getInflight().decrementAndGet());
        } else if (ruleHandle.getLoadBalance().equals(P2C)) {
            response = response.doFinally(signalType -> responseTrigger(upstream));
        } else if (ruleHandle.getLoadBalance().equals(SHORTEST_RESPONSE)) {
            long beginTime = System.currentTimeMillis();
            response = response.doOnSuccess(e -> successResponseTrigger(upstream, beginTime));
        }
        return probe ? response : UpstreamCircuitBreaker.recordOutcome(response, breakerKey);
    }

    /**
     * Honors the request's sticky binding when it points at an admitted candidate,
     * otherwise selects with the base algorithm or, under the STRICT policy,
     * rejects the request instead of silently moving the session (#8037).
     *
     * @param exchange the current server exchange
     * @param rule the matched rule, the binding scope
     * @param ruleHandle the divide rule handle
     * @param breakeredUpstreamList the candidates after canary and breaker admission
     * @return the selected upstream, or null when the request must be rejected
     */
    private Upstream resolveWithAffinity(final ServerWebExchange exchange, final RuleData rule,
                                         final DivideRuleHandle ruleHandle, final List<Upstream> breakeredUpstreamList) {
        final StickySessionConfig sticky = ruleHandle.getSticky();
        final StickySessionAffinity.Resolution resolution =
                stickySessionAffinity.resolve(exchange, rule.getId(), sticky, breakeredUpstreamList);
        final Upstream bound = resolution.getBound();
        if (Objects.nonNull(bound)) {
            if (P2C.equals(ruleHandle.getLoadBalance())) {
                // a binding hit skips the P2C selection, which normally reserves the
                // attempt: the same accounting must apply or the count drifts down
                bound.getInflight().incrementAndGet();
                bound.setLastPicked(System.currentTimeMillis());
            }
            return bound;
        }
        if (resolution.isBindingPresent() && stickySessionAffinity.isStrict(sticky)) {
            // only a presented binding whose target is not eligible rejects the request;
            // a request without a binding is a new session and must be allocated (#8037)
            LOG.warn("sticky session binding is not eligible and the policy is STRICT, rejecting the request");
            exchange.getResponse().setStatusCode(HttpStatus.SERVICE_UNAVAILABLE);
            return null;
        }
        // a configured deterministic key maps a binding-less request onto the admitted
        // candidates by the consistent hash ring; it is key hashing, not a binding —
        // no separate cookie is written for it and a missing key falls back to the
        // algorithm (#8037). The key is a sub-mode of the sticky configuration: it is
        // evaluated inside the affinity path, so sticky itself must be active (enabled
        // with usable binding secrets), which keeps one configuration surface.
        final Upstream keyed = resolveWithDeterministicKey(exchange, sticky, ruleHandle, breakeredUpstreamList);
        if (Objects.nonNull(keyed)) {
            return keyed;
        }
        return LoadbalancerUtils.getForExchange(breakeredUpstreamList, ruleHandle.getLoadBalance(), exchange);
    }

    private Upstream resolveWithDeterministicKey(final ServerWebExchange exchange, final StickySessionConfig sticky,
                                                 final DivideRuleHandle ruleHandle, final List<Upstream> breakeredUpstreamList) {
        final DeterministicKeyConfig keyConfig = Objects.nonNull(sticky) ? sticky.getKey() : null;
        if (Objects.isNull(keyConfig) || !keyConfig.isConfigured()) {
            return null;
        }
        final String key = deterministicKeyExtractor.extract(exchange, keyConfig);
        if (Objects.isNull(key)) {
            return null;
        }
        final LoadBalanceData data = new LoadBalanceData();
        data.setHashKey(key);
        final Upstream upstream = LoadBalancerFactory.selector(breakeredUpstreamList, LoadBalanceEnum.HASH.getName(), data);
        if (Objects.nonNull(upstream) && P2C.equals(ruleHandle.getLoadBalance())) {
            // the hash pick skips the P2C selection, which normally reserves the
            // attempt: the same accounting must apply or the count drifts down
            upstream.getInflight().incrementAndGet();
            upstream.setLastPicked(System.currentTimeMillis());
        }
        return upstream;
    }

    private DivideRuleHandle buildRuleHandle(final RuleData rule) {
        return DividePluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
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

    /**
     * A positive requestMaxSize must also cover chunked / unknown-length bodies: the
     * Content-Length header is absent (-1) there, so the limit is enforced on the actual
     * bytes while the request body streams through.
     */
    private ServerWebExchange limitChunkedRequestBody(final ServerWebExchange exchange, final DivideRuleHandle ruleHandle) {
        if (ruleHandle.getRequestMaxSize() <= 0 || exchange.getRequest().getHeaders().getContentLength() >= 0) {
            return exchange;
        }
        final long maxSize = ruleHandle.getRequestMaxSize();
        return exchange.mutate().request(new ServerHttpRequestDecorator(exchange.getRequest()) {
            private long forwarded;

            @Override
            @NonNull
            public Flux<DataBuffer> getBody() {
                return Flux.defer(() -> {
                    forwarded = 0;
                    // The overflowing chunk is owned by this operator and is released before the
                    // error is propagated. doOnDiscard covers chunks that an upstream publisher
                    // has already queued when the body is cancelled or fails before forwarding.
                    return super.getBody()
                            .doOnDiscard(DataBuffer.class, DataBufferUtils::release)
                            .doOnNext(buffer -> {
                                forwarded += buffer.readableByteCount();
                                if (forwarded > maxSize) {
                                    DataBufferUtils.release(buffer);
                                    LOG.error("request entity is too large");
                                    throw new ResponseStatusException(HttpStatus.PAYLOAD_TOO_LARGE, "request entity is too large");
                                }
                            });
                });
            }
        }).build();
    }

    /**
     * Calculate request header size in bytes.
     * Header size includes field names, field values, and line delimiters (": " and CRLF) per header line,
     * plus the terminating CRLF of the HTTP header section. If headers is null or empty, 0L is returned.
     * Note: This measures the header field section; request line and HTTP/2 pseudo-headers are not included.
     *
     * @param headers the http headers
     * @return total header size in bytes
     */
    long calculateHeaderSize(final HttpHeaders headers) {
        if (Objects.isNull(headers) || headers.isEmpty()) {
            return 0L;
        }
        long headerSize = 0L;
        for (Map.Entry<String, List<String>> entry : headers.entrySet()) {
            // Uses UTF-8 byte counting conservatively across HTTP/1.1 and HTTP/2 without protocol branching.
            int nameSize = entry.getKey().getBytes(StandardCharsets.UTF_8).length;
            List<String> values = entry.getValue();
            if (CollectionUtils.isEmpty(values)) {
                headerSize += nameSize + 4L;
            } else {
                for (String value : values) {
                    int valueSize = Objects.isNull(value) ? 0 : value.getBytes(StandardCharsets.UTF_8).length;
                    headerSize += nameSize + valueSize + 4L;
                }
            }
        }
        return headerSize + 2L;
    }
}
