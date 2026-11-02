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

package org.apache.shenyu.plugin.divide.capability;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.plugin.capability.PluginCapabilityRegistry;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.cache.CachePlugin;
import org.apache.shenyu.plugin.cache.ICache;
import org.apache.shenyu.plugin.cache.handler.CachePluginDataHandler;
import org.apache.shenyu.plugin.cache.memory.MemoryCache;
import org.apache.shenyu.plugin.divide.DividePlugin;
import org.apache.shenyu.plugin.divide.handler.DividePluginDataHandler;
import org.apache.shenyu.plugin.httpclient.NettyHttpClientPlugin;
import org.apache.shenyu.plugin.response.ResponsePlugin;
import org.apache.shenyu.plugin.response.strategy.NettyClientMessageWriter;
import org.apache.shenyu.plugin.uri.URIPlugin;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.junit.jupiter.api.Test;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.GenericApplicationContext;
import org.springframework.http.server.reactive.ReactorHttpHandlerAdapter;
import org.springframework.web.server.adapter.WebHttpHandlerBuilder;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.netty.DisposableServer;
import reactor.netty.http.client.HttpClient;
import reactor.netty.http.server.HttpServer;

import java.time.Duration;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class HttpCapabilityPilotTest {

    @Test
    void actualHttpPipelineCooperatesAndCacheHitMakesNoUpstreamCall() {
        final ApplicationContext previousContext = SpringBeanUtils.getInstance().getApplicationContext();
        final ICache previousCache = Singleton.INST.get(ICache.class);
        final ObservedCache cache = new ObservedCache();
        GenericApplicationContext context = new GenericApplicationContext();
        ShenyuConfig config = new ShenyuConfig();
        config.getScheduler().setEnabled(false);
        context.registerBean(ShenyuConfig.class, () -> config);
        context.registerBean(ShenyuResult.class, DefaultShenyuResult::new);
        context.refresh();
        SpringBeanUtils.getInstance().setApplicationContext(context);
        Singleton.INST.single(ICache.class, cache);
        AtomicInteger calls = new AtomicInteger();
        DisposableServer upstream = null;
        DisposableServer gateway = null;
        RuleData cacheRule = null;
        RuleData divideRule = null;
        try {
            upstream = HttpServer.create().host("127.0.0.1").port(0).handle((request, response) -> {
                calls.incrementAndGet();
                return response.header("Content-Type", "application/json").header("Cache-Control", "public,max-age=60")
                        .sendString(Mono.just("{\"pilot\":true}"));
            }).bindNow();
            CachePlugin cachePlugin = new CachePlugin();
            DividePlugin divide = new DividePlugin();
            SelectorData cacheSelector = route(cachePlugin, "pilot-cache");
            SelectorData divideSelector = route(divide, "pilot-divide");
            cacheRule = rule(cacheSelector, "{\"timeoutSeconds\":60}");
            divideRule = rule(divideSelector, "{\"retry\":0,\"timeout\":5000}");
            new CachePluginDataHandler().handlerRule(cacheRule);
            new DividePluginDataHandler().handlerRule(divideRule);
            UpstreamCacheManager.getInstance().submit(divideSelector.getId(), List.of(Upstream.builder()
                    .protocol("http://").url("127.0.0.1:" + upstream.port()).weight(100).status(true).ready(true).healthCheckEnabled(false).build()));
            List<ShenyuPlugin> plugins = new ArrayList<>(List.of(cachePlugin, divide, new URIPlugin(),
                    new NettyHttpClientPlugin(HttpClient.create(), 1048576),
                    new ResponsePlugin(Map.of("http", new NettyClientMessageWriter()))));
            plugins.sort(Comparator.comparingInt(ShenyuPlugin::getOrder));
            ShenyuWebHandler handler = new ShenyuWebHandler(plugins, null, config);
            gateway = HttpServer.create().host("127.0.0.1").port(0).handle(adapter(handler)).bindNow();
            String uri = "http://127.0.0.1:" + gateway.port() + "/pilot";
            assertEquals("{\"pilot\":true}", get(uri));
            assertEquals(1, calls.get(), "cache miss must execute the real URI/client/response chain");
            cache.written.asMono().block(Duration.ofSeconds(5));
            assertEquals("{\"pilot\":true}", get(uri));
            assertEquals(1, calls.get(), "a reusable cache hit must make zero additional upstream calls");
            var observed = plugins.stream().map(plugin -> new PluginCapabilityRegistry.Implementation(plugin.named(), plugin.getClass().getName())).toList();
            var report = PluginCapabilityRegistry.shared().preflight(observed,
                    Set.of("load-balancer", "http-writer-binding", "response-cache-provider"), "http");
            assertTrue(report.findings().isEmpty());
        } finally {
            if (Objects.nonNull(gateway)) {
                gateway.disposeNow();
            }
            if (Objects.nonNull(upstream)) {
                upstream.disposeNow();
            }
            if (Objects.nonNull(cacheRule)) {
                new CachePluginDataHandler().removeRule(cacheRule);
            }
            if (Objects.nonNull(divideRule)) {
                new DividePluginDataHandler().removeRule(divideRule);
            }
            UpstreamCacheManager.getInstance().removeByKey("pilot-divide");
            BaseDataCache.getInstance().cleanPluginData();
            BaseDataCache.getInstance().cleanSelectorData();
            BaseDataCache.getInstance().cleanRuleData();
            if (Objects.isNull(previousCache)) {
                Singleton.INST.remove(ICache.class);
            } else {
                Singleton.INST.single(ICache.class, previousCache);
            }
            cache.close();
            SpringBeanUtils.getInstance().setApplicationContext(previousContext);
            context.close();
        }
    }

    private String get(final String uri) {
        return HttpClient.create().get().uri(uri).responseSingle((response, bytes) -> {
            assertEquals(200, response.status().code());
            return bytes.asString();
        }).block(Duration.ofSeconds(5));
    }

    private ReactorHttpHandlerAdapter adapter(final ShenyuWebHandler handler) {
        var http = WebHttpHandlerBuilder.webHandler(handler).filter((exchange, chain) -> {
            ShenyuContext shenyu = new ShenyuContext();
            shenyu.setRpcType("http");
            shenyu.setRealUrl(exchange.getRequest().getURI().getRawPath());
            shenyu.setPath(exchange.getRequest().getURI().getRawPath());
            shenyu.setHttpMethod(exchange.getRequest().getMethod().name());
            exchange.getAttributes().put(Constants.CONTEXT, shenyu);
            return chain.filter(exchange);
        }).build();
        return new ReactorHttpHandlerAdapter(http);
    }

    private SelectorData route(final ShenyuPlugin plugin, final String id) {
        BaseDataCache.getInstance().cachePluginData(PluginData.builder().id(plugin.named()).name(plugin.named()).enabled(true).build());
        ConditionData condition = new ConditionData();
        condition.setParamType("uri");
        condition.setOperator("match");
        condition.setParamName("/");
        condition.setParamValue("/**");
        SelectorData selector = SelectorData.builder().id(id).pluginName(plugin.named()).name(id).enabled(true).logged(false).continued(true)
                .sort(1).matchMode(0).type(1).matchRestful(false).handle("{}").conditionList(List.of(condition)).build();
        BaseDataCache.getInstance().cacheSelectData(selector);
        return selector;
    }

    private RuleData rule(final SelectorData selector, final String handle) {
        RuleData rule = RuleData.builder().id(selector.getId() + "-rule").selectorId(selector.getId()).pluginName(selector.getPluginName())
                .name("pilot").enabled(true).logged(false).sort(1).matchMode(0).matchRestful(false).handle(handle)
                .conditionDataList(selector.getConditionList()).build();
        BaseDataCache.getInstance().cacheRuleData(rule);
        return rule;
    }

    private static final class ObservedCache implements ICache {

        private final MemoryCache delegate = new MemoryCache();

        private final Sinks.Empty<Void> written = Sinks.empty();

        @Override
        public Mono<Boolean> cacheData(final String key, final byte[] bytes, final long timeoutSeconds) {
            return delegate.cacheData(key, bytes, timeoutSeconds).doOnSuccess(success -> written.tryEmitEmpty());
        }

        @Override
        public Mono<Boolean> isExist(final String key) {
            return delegate.isExist(key);
        }

        @Override
        public Mono<byte[]> getData(final String key) {
            return delegate.getData(key);
        }

        @Override
        public void close() {
            delegate.close();
        }
    }
}
