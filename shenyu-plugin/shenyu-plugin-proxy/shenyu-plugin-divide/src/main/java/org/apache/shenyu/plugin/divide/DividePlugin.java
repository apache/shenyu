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
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
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
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.base.utils.LoadbalancerUtils;
import org.apache.shenyu.plugin.base.utils.UpstreamLabelUtils;
import org.apache.shenyu.plugin.divide.canary.CanaryDecision;
import org.apache.shenyu.plugin.divide.canary.CanaryDecisionService;
import org.apache.shenyu.plugin.divide.canary.DefaultCanaryDecisionService;
import org.apache.shenyu.plugin.divide.handler.DividePluginDataHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Divide Plugin.
 */
public class DividePlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(DividePlugin.class);

    private static final String P2C = "p2c";

    private static final String SHORTEST_RESPONSE = "shortestResponse";

    private final CanaryDecisionService canaryDecisionService;

    private Long beginTime;

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
        Upstream upstream = selectUpstream(exchange, selector.getId(), rule, ruleHandle, upstreamList);
        if (Objects.isNull(upstream)) {
            LOG.error("divide has no upstream");
            Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.CANNOT_FIND_HEALTHY_UPSTREAM_URL);
            return WebFluxResultUtils.result(exchange, error);
        }
        // set the http url
        List<String> specifyDomains = exchange.getRequest().getHeaders().get(Constants.SPECIFY_DOMAIN);
        String domain = upstream.buildDomain();
        if (CollectionUtils.isNotEmpty(specifyDomains)) {
            String protocol = StringUtils.defaultIfBlank(upstream.getProtocol(), "http://");
            domain = protocol + specifyDomains.get(0).trim();
        }
        // set domain
        exchange.getAttributes().put(Constants.HTTP_DOMAIN, domain);
        // set the http timeout
        exchange.getAttributes().put(Constants.HTTP_TIME_OUT, ruleHandle.getTimeout());
        exchange.getAttributes().put(Constants.HTTP_RETRY, ruleHandle.getRetry());
        // set retry strategy stuff
        exchange.getAttributes().put(Constants.RETRY_STRATEGY, StringUtils.defaultIfEmpty(ruleHandle.getRetryStrategy(), RetryEnum.CURRENT.getName()));
        exchange.getAttributes().put(Constants.LOAD_BALANCE, StringUtils.defaultIfEmpty(ruleHandle.getLoadBalance(), LoadBalanceEnum.RANDOM.getName()));
        exchange.getAttributes().put(Constants.DIVIDE_SELECTOR_ID, selector.getId());
        if (ruleHandle.getLoadBalance().equals(P2C)) {
            return chain.execute(exchange).doOnSuccess(e -> responseTrigger(upstream
            )).doOnError(throwable -> responseTrigger(upstream));
        } else if (ruleHandle.getLoadBalance().equals(SHORTEST_RESPONSE)) {
            beginTime = System.currentTimeMillis();
            return chain.execute(exchange).doOnSuccess(e -> successResponseTrigger(upstream
            ));
        }
        return chain.execute(exchange);
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
    
    private Upstream selectUpstream(final ServerWebExchange exchange, final String selectorId, final RuleData rule,
                                    final DivideRuleHandle ruleHandle, final List<Upstream> upstreams) {
        CanaryConfig config = ruleHandle.getCanary();
        if (CollectionUtils.isNotEmpty(exchange.getRequest().getHeaders().get(Constants.SPECIFY_DOMAIN))) {
            return selectLegacyUpstream(exchange, selectorId, ruleHandle);
        }
        if (Objects.isNull(config)) {
            return selectLegacyUpstream(exchange, selectorId, ruleHandle);
        }
        CanaryDecision actual = canaryDecisionService.decide(exchange, rule.getId(), config);
        Map<String, String> labels = partitionLabels(actual, config);
        List<Upstream> candidates = UpstreamLabelUtils.filter(upstreams, labels);
        if (candidates.isEmpty() && actual == CanaryDecision.CANARY && "STABLE".equals(config.getFallbackPolicy())) {
            actual = CanaryDecision.STABLE;
            labels = partitionLabels(actual, config);
            candidates = UpstreamLabelUtils.filter(upstreams, labels);
        }
        if (candidates.isEmpty()) {
            exchange.getResponse().setStatusCode(HttpStatus.SERVICE_UNAVAILABLE);
            return null;
        }
        // The pool is resolved before load balancing. A backend failure must not change this partition.
        exchange.getAttributes().put(Constants.SHENYU_CANARY_PARTITION, actual.getName());
        exchange.getAttributes().put(Constants.SHENYU_CANARY_LABELS, labels);
        return LoadbalancerUtils.getForExchange(candidates, ruleHandle.getLoadBalance(), exchange);
    }

    private Upstream selectLegacyUpstream(final ServerWebExchange exchange, final String selectorId, final DivideRuleHandle ruleHandle) {
        List<Upstream> candidates = UpstreamCacheManager.getInstance().findLegacyUpstreamListBySelectorId(selectorId);
        if (CollectionUtils.isEmpty(candidates)) {
            return null;
        }
        return LoadbalancerUtils.getForExchange(candidates, ruleHandle.getLoadBalance(), exchange);
    }

    private Map<String, String> partitionLabels(final CanaryDecision partition, final CanaryConfig config) {
        Map<String, String> labels = partition == CanaryDecision.CANARY ? config.getCanaryLabels() : config.getStableLabels();
        return Objects.isNull(labels) ? Map.of() : Map.copyOf(labels);
    }

    private DivideRuleHandle buildRuleHandle(final RuleData rule) {
        return DividePluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
    }

    private void responseTrigger(final Upstream upstream) {
        long now = System.currentTimeMillis();
        upstream.getInflight().decrementAndGet();
        upstream.setResponseStamp(now);
        long stamp = upstream.getResponseStamp();
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

    private void successResponseTrigger(final Upstream upstream) {
        upstream.getSucceededElapsed().addAndGet(System.currentTimeMillis() - beginTime);
        upstream.getSucceeded().incrementAndGet();
    }
    
}
