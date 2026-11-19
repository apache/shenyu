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

package org.apache.shenyu.benchmark;

import com.google.gson.Gson;
import com.sun.management.ThreadMXBean;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.MatchDataCache;
import org.apache.shenyu.plugin.base.circuitbreaker.UpstreamCircuitBreaker;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.divide.DividePlugin;
import org.apache.shenyu.plugin.divide.handler.DividePluginDataHandler;
import org.apache.shenyu.plugin.global.GlobalPlugin;
import org.apache.shenyu.plugin.httpclient.NettyHttpClientPlugin;
import org.apache.shenyu.plugin.response.ResponsePlugin;
import org.apache.shenyu.plugin.response.strategy.NettyClientMessageWriter;
import org.apache.shenyu.plugin.uri.URIPlugin;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.springframework.context.support.GenericApplicationContext;
import org.springframework.http.server.reactive.ReactorHttpHandlerAdapter;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.adapter.WebHttpHandlerBuilder;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.core.scheduler.Scheduler;
import reactor.core.scheduler.Schedulers;
import reactor.netty.DisposableServer;
import reactor.netty.http.server.HttpServer;
import reactor.netty.resources.ConnectionProvider;
import reactor.netty.resources.LoopResources;

import java.lang.management.ManagementFactory;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.Semaphore;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.locks.LockSupport;

/** Standalone, loopback-only HTTP gateway benchmark. See README.md beside this file. */
public final class DivideGatewayBenchmark {
    private static final String SELECTOR = "gateway-benchmark";
    private static final Gson GSON = new Gson();
    private static final String BODY = "benchmark-ok-" + "x".repeat(1011);
    private static final ThreadMXBean THREADS = (ThreadMXBean) ManagementFactory.getThreadMXBean();
    private static final com.sun.management.OperatingSystemMXBean OS =
            (com.sun.management.OperatingSystemMXBean) ManagementFactory.getOperatingSystemMXBean();
    private static final HttpClient CLIENT = HttpClient.newBuilder().version(HttpClient.Version.HTTP_1_1)
            .connectTimeout(Duration.ofSeconds(5)).executor(Executors.newFixedThreadPool(8)).build();
    private static Map<String, Long> startMetrics;
    private static List<Upstream> configured = List.of();

    private DivideGatewayBenchmark() { }

    public static void main(final String[] args) throws Exception {
        switch (args[0]) {
            case "upstream" -> upstream(Path.of(args[1]));
            case "gateway" -> gateway(Path.of(args[1]), Path.of(args[2]), args.length > 3 && args[3].equals("config-views"));
            case "load-config" -> {
                load(Integer.parseInt(args[1]), Integer.parseInt(args[2]), Integer.parseInt(args[3]), Path.of(args[4]),
                        args[5], Integer.parseInt(args[6]), Integer.parseInt(args[7]));
                System.exit(0);
            }
            case "load" -> {
                load(Integer.parseInt(args[1]), Integer.parseInt(args[2]), Integer.parseInt(args[3]), Path.of(args[4]));
                System.exit(0);
            }
            case "verify" -> {
                verify(Integer.parseInt(args[1]), Path.of(args[2]), Path.of(args[3]));
                System.exit(0);
            }
            default -> throw new IllegalArgumentException("mode: upstream, gateway [config-views], load, load-config or verify");
        }
    }

    private static void upstream(final Path ready) throws Exception {
        LoopResources loops = LoopResources.create("benchmark-upstream", 4, true);
        List<Integer> ports = new ArrayList<>();
        List<Endpoint> endpoints = new ArrayList<>();
        for (int index = 0; index < 128; index++) {
            Endpoint endpoint = new Endpoint();
            endpoints.add(endpoint);
            DisposableServer server = HttpServer.create().host("127.0.0.1").port(0).runOn(loops).handle((request, response) -> {
                if (request.uri().startsWith("/_bench/")) {
                    String action = request.uri().substring("/_bench/".length());
                    if (action.equals("hold")) {
                        endpoint.release = Sinks.empty();
                        endpoint.mode = "hold";
                    } else if (action.equals("ok")) {
                        endpoint.mode = "ok";
                        endpoint.release.tryEmitEmpty();
                    } else if (action.equals("fail")) {
                        endpoint.mode = "fail";
                    }
                    return response.sendString(Mono.just(GSON.toJson(Map.of("requests", endpoint.requests.get(),
                            "active", endpoint.active.get(), "maxActive", endpoint.maxActive.get(), "mode", endpoint.mode))));
                }
                endpoint.requests.incrementAndGet();
                long active = endpoint.active.incrementAndGet();
                endpoint.maxActive.accumulateAndGet(active, Math::max);
                if (endpoint.mode.equals("fail")) {
                    response.withConnection(connection -> connection.dispose());
                    endpoint.active.decrementAndGet();
                    return Mono.empty();
                }
                Mono<Void> delay = endpoint.mode.equals("hold") ? endpoint.release.asMono() : Mono.delay(Duration.ofMillis(2)).then();
                return delay.then(response.header("Content-Type", "text/plain").header("Content-Length", "1024")
                        .sendString(Mono.just(BODY)).then()).doFinally(signal -> endpoint.active.decrementAndGet());
            }).bindNow();
            ports.add(server.port());
        }
        Files.writeString(ready, GSON.toJson(ports));
        new CountDownLatch(1).await();
    }

    private static void gateway(final Path upstreamFile, final Path ready, final boolean configViews) throws Exception {
        int[] ports = GSON.fromJson(Files.readString(upstreamFile), int[].class);
        ShenyuConfig config = new ShenyuConfig();
        config.getUpstreamCheck().setEnabled(false);
        config.getScheduler().setEnabled(false);
        config.getSelectorMatchCache().getCache().setEnabled(false);
        config.getRuleMatchCache().getCache().setEnabled(false);
        Singleton.INST.single(ShenyuConfig.class, config);
        GenericApplicationContext context = new GenericApplicationContext();
        context.registerBean(ShenyuConfig.class, () -> config);
        context.registerBean(ShenyuResult.class, DefaultShenyuResult::new);
        context.refresh();
        SpringBeanUtils.getInstance().setApplicationContext(context);
        PluginData plugin = new PluginData();
        plugin.setName("divide");
        plugin.setEnabled(true);
        BaseDataCache.getInstance().cachePluginData(plugin);
        SelectorData selector = new SelectorData();
        selector.setId(SELECTOR);
        selector.setName(SELECTOR);
        selector.setPluginName("divide");
        selector.setEnabled(true);
        selector.setType(0);
        selector.setSort(0);
        selector.setLogged(false);
        selector.setContinued(false);
        BaseDataCache.getInstance().cacheSelectData(selector);
        DivideRuleHandle rule = new DivideRuleHandle();
        rule.setRetry(0);
        rule.setTimeout(60000);
        DividePluginDataHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(SELECTOR, Constants.DEFAULT_RULE), rule);
        ConnectionProvider pool = ConnectionProvider.builder("gateway-upstreams").maxConnections(128).pendingAcquireMaxCount(2048).build();
        reactor.netty.http.client.HttpClient netty = reactor.netty.http.client.HttpClient.create(pool)
                .runOn(LoopResources.create("gateway-client", 4, true)).disableRetry(true);
        ConfigViews views = configViews ? new ConfigViews(config, ports[0]) : null;
        List<ShenyuPlugin> plugins = List.of(new GlobalPlugin(exchange -> {
            ShenyuContext value = new ShenyuContext();
            value.setRpcType("http");
            value.setPath(exchange.getRequest().getURI().getRawPath());
            value.setRealUrl(exchange.getRequest().getURI().getRawPath());
            value.setHttpMethod(exchange.getRequest().getMethod().name());
            return value;
        }), configViews ? new ConfigDividePlugin() : new DividePlugin(), new URIPlugin(), new NettyHttpClientPlugin(netty, 1024 * 1024),
                new ResponsePlugin(Map.of("http", new NettyClientMessageWriter())));
        ShenyuWebHandler handler = new ShenyuWebHandler(plugins, null, config);
        ReactorHttpHandlerAdapter adapter = new ReactorHttpHandlerAdapter(WebHttpHandlerBuilder.webHandler(exchange -> {
            if (views != null && exchange.getRequest().getPath().value().startsWith("/_bench/")) {
                return Mono.fromCallable(() -> views.control(exchange)).subscribeOn(views.controlWorker)
                        .flatMap(data -> text(exchange, GSON.toJson(data)));
            }
            if (exchange.getRequest().getPath().value().startsWith("/_bench/")) {
                String action = exchange.getRequest().getPath().value().substring("/_bench/".length());
                Map<String, Long> data;
                if (action.equals("config")) {
                    int size = Integer.parseInt(exchange.getRequest().getQueryParams().getFirst("size"));
                    String state = exchange.getRequest().getQueryParams().getFirst("state");
                    configure(ports, size, state);
                    data = Map.of("configured", (long) size);
                } else if (action.equals("remove")) {
                    new DividePluginDataHandler().removeSelector(selector);
                    data = Map.of("removed", 1L);
                } else if (action.equals("retry")) {
                    int retries = Integer.parseInt(exchange.getRequest().getQueryParams().getFirst("count"));
                    DividePluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(SELECTOR, Constants.DEFAULT_RULE)).setRetry(retries);
                    data = Map.of("retries", (long) retries);
                } else if (action.equals("start")) {
                    startMetrics = metrics();
                    data = Map.of("started", 1L);
                } else {
                    data = metrics();
                    if (startMetrics != null) {
                        data.replaceAll((key, value) -> value - startMetrics.get(key));
                    }
                }
                byte[] bytes = GSON.toJson(data).getBytes(java.nio.charset.StandardCharsets.UTF_8);
                return exchange.getResponse().writeWith(Mono.just(exchange.getResponse().bufferFactory().wrap(bytes)));
            }
            return handler.handle(exchange).onErrorResume(error -> {
                exchange.getResponse().setStatusCode(org.springframework.http.HttpStatus.BAD_GATEWAY);
                return exchange.getResponse().setComplete();
            });
        }).build());
        DisposableServer server = HttpServer.create().host("127.0.0.1").port(0)
                .runOn(LoopResources.create("gateway-server", 4, true)).handle(adapter).bindNow();
        THREADS.setThreadAllocatedMemoryEnabled(true);
        Files.writeString(ready, Integer.toString(server.port()));
        new CountDownLatch(1).await();
    }

    private static void configure(final int[] ports, final int size, final String state) {
        configured.forEach(upstream -> UpstreamCircuitBreaker.getInstance().reset(UpstreamCircuitBreaker.buildKey(SELECTOR, upstream)));
        UpstreamCacheManager.getInstance().removeByKey(SELECTOR);
        List<Upstream> targets = new ArrayList<>();
        for (int index = 0; index < size; index++) {
            Upstream upstream = Upstream.builder().protocol("http://").url("127.0.0.1:" + ports[index]).weight(50).healthCheckEnabled(false).build();
            targets.add(upstream);
            if (state.equals("blocked") || state.equals("mixed") && index % 2 != 0) {
                for (int failure = 0; failure < 3; failure++) {
                    UpstreamCircuitBreaker.getInstance().recordFailure(UpstreamCircuitBreaker.buildKey(SELECTOR, upstream));
                }
            }
        }
        configured = targets;
        UpstreamCacheManager.getInstance().submit(SELECTOR, targets);
        // Reinstall after a selector-removal correctness scenario.
        DivideRuleHandle rule = new DivideRuleHandle();
        rule.setRetry(0);
        rule.setTimeout(60000);
        DividePluginDataHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(SELECTOR, Constants.DEFAULT_RULE), rule);
    }

    private static Mono<Void> text(final ServerWebExchange exchange, final String value) {
        byte[] bytes = value.getBytes(java.nio.charset.StandardCharsets.UTF_8);
        return exchange.getResponse().writeWith(Mono.just(exchange.getResponse().bufferFactory().wrap(bytes)));
    }

    // Only observation and explicit miss responses are fixture-specific; matching and forwarding
    // use the variant's actual DividePlugin/AbstractShenyuPlugin, with no new read-only API linkage.
    private static final class ConfigDividePlugin extends DividePlugin {
        @Override
        protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                       final SelectorData selector, final RuleData rule) {
            exchange.getResponse().getHeaders().set("X-Bench-Version", rule.getName());
            exchange.getResponse().getHeaders().set("X-Bench-Observed-Ns", Long.toString(System.nanoTime()));
            return super.doExecute(exchange, chain, selector, rule);
        }

        @Override
        protected Mono<Void> handleSelectorIfNull(final String plugin, final ServerWebExchange exchange, final ShenyuPluginChain chain) {
            return miss(exchange);
        }

        @Override
        protected Mono<Void> handleRuleIfNull(final String plugin, final ServerWebExchange exchange, final ShenyuPluginChain chain) {
            return miss(exchange);
        }

        private Mono<Void> miss(final ServerWebExchange exchange) {
            exchange.getResponse().setStatusCode(org.springframework.http.HttpStatus.NOT_FOUND);
            return text(exchange, "config-no-match");
        }
    }

    private static String selectorId(final int index) {
        return SELECTOR + "-config-" + index;
    }

    private static String matchedPath(final int size) {
        return "/config/" + (size - 1) + "/r" + (size - 1) + "/test";
    }

    private static List<ConditionData> conditions(final String prefix, final int count) {
        List<ConditionData> result = new ArrayList<>();
        for (int index = 0; index < count; index++) {
            ConditionData condition = new ConditionData();
            condition.setParamType("uri");
            condition.setParamName("/");
            condition.setOperator("startsWith");
            // All predicates are path-only and true for the sole matching candidate.
            condition.setParamValue(index == 0 ? prefix : "/config/");
            result.add(condition);
        }
        return result;
    }

    private static RuleData configRule(final int selector, final int index, final int count, final long version) {
        RuleData rule = new RuleData();
        rule.setId(selectorId(selector) + "-r" + index);
        rule.setSelectorId(selectorId(selector));
        rule.setPluginName("divide");
        rule.setName("v" + version);
        rule.setEnabled(true);
        rule.setLogged(false);
        rule.setSort(index);
        rule.setMatchMode(0);
        rule.setMatchRestful(false);
        rule.setConditionDataList(conditions("/config/" + selector + "/r" + index + "/", count));
        return rule;
    }

    private static final class ConfigViews {
        private final ShenyuConfig config;
        private final int upstreamPort;
        private final Scheduler controlWorker = Schedulers.newBoundedElastic(1, 16, "config-control");
        private final java.util.concurrent.ScheduledExecutorService sampler = Executors.newSingleThreadScheduledExecutor();
        private final AtomicLong peakHeap = new AtomicLong();
        private volatile boolean sampling;
        private Map<String, Long> before;
        private Map<String, Object> retainedBefore;
        private int size;
        private int conditionCount;
        private long version;

        private ConfigViews(final ShenyuConfig config, final int upstreamPort) {
            this.config = config;
            this.upstreamPort = upstreamPort;
            sampler.scheduleAtFixedRate(() -> {
                if (sampling) {
                    peakHeap.accumulateAndGet(heap(), Math::max);
                }
            }, 0, 10, java.util.concurrent.TimeUnit.MILLISECONDS);
            Runtime.getRuntime().addShutdownHook(new Thread(() -> {
                sampler.shutdownNow();
                controlWorker.dispose();
            }));
        }

        private Map<String, Object> control(final ServerWebExchange exchange) throws Exception {
            String action = exchange.getRequest().getPath().value();
            var query = exchange.getRequest().getQueryParams();
            if (action.equals("/_bench/config-view")) {
                int nextSize = Integer.parseInt(query.getFirst("size"));
                int nextConditions = Integer.parseInt(query.getFirst("conditions"));
                String state = query.getFirst("state");
                if ((nextSize != 1 && nextSize != 128) || nextConditions < 1 || nextConditions > 128
                        || !List.of("matched", "no-match", "cache-hit").contains(state)) {
                    throw new IllegalArgumentException("size=1|128; conditions=1..128; state=matched|no-match|cache-hit");
                }
                Map<String, Long> setupBefore = metrics();
                long heapBefore = heap();
                long begin = System.nanoTime();
                install(nextSize, nextConditions, state.equals("cache-hit"));
                long elapsed = System.nanoTime() - begin;
                Map<String, Long> setup = metrics();
                setup.replaceAll((key, value) -> value - setupBefore.get(key));
                setup.put("elapsed_ns", elapsed);
                setup.put("heap_before_bytes", heapBefore);
                setup.put("heap_after_bytes", heap());
                return Map.of("selectors", size, "rulesPerSelector", size, "conditionsPerEntry", conditionCount,
                        "state", state, "version", "v" + version, "setup", setup);
            }
            if (action.equals("/_bench/config-burst")) {
                int updates = Integer.parseInt(query.getFirst("count"));
                if (size == 0 || updates < 1 || updates > 128) {
                    throw new IllegalArgumentException("configure first; burst count=1..128");
                }
                long total = 0;
                long max = 0;
                long published = 0;
                long started = System.nanoTime();
                for (int index = 0; index < updates; index++) {
                    RuleData rule = configRule(size - 1, size - 1, conditionCount, ++version);
                    long begin = System.nanoTime();
                    // Publication + derived-cache invalidation, NOT Admin/network synchronization.
                    BaseDataCache.getInstance().cacheRuleData(rule);
                    MatchDataCache.getInstance().removeRuleData("divide");
                    published = System.nanoTime();
                    long elapsed = published - begin;
                    total += elapsed;
                    max = Math.max(max, elapsed);
                }
                return Map.of("updates", updates, "updateNs", total, "maxUpdateNs", max,
                        "burstNs", System.nanoTime() - started, "publishedAtNs", Long.toString(published), "version", "v" + version);
            }
            if (action.equals("/_bench/start")) {
                retainedBefore = retained();
                peakHeap.set(heap());
                sampling = true;
                before = metrics();
                return Map.of("started", 1);
            }
            if (action.equals("/_bench/metrics")) {
                Map<String, Long> delta = metrics();
                if (before == null) {
                    throw new IllegalStateException("start measurement first");
                }
                delta.replaceAll((key, value) -> value - before.get(key));
                sampling = false;
                peakHeap.accumulateAndGet(heap(), Math::max);
                Map<String, Object> result = new LinkedHashMap<>(delta);
                result.put("sampled_peak_heap_bytes", peakHeap.get());
                result.put("heap_sample_interval_ms", 10);
                result.put("retained_before", retainedBefore);
                return result;
            }
            if (action.equals("/_bench/retained")) {
                return retained();
            }
            throw new IllegalArgumentException("unknown config-view control action");
        }

        private void install(final int nextSize, final int nextConditions, final boolean cacheHit) {
            BaseDataCache cache = BaseDataCache.getInstance();
            cache.cleanSelectorData();
            cache.cleanRuleData();
            MatchDataCache.getInstance().cleanSelectorData();
            MatchDataCache.getInstance().cleanRuleDataData();
            for (int index = 0; index < size; index++) {
                UpstreamCacheManager.getInstance().removeByKey(selectorId(index));
            }
            DividePluginDataHandler.CACHED_HANDLE.get().getAllCache().clear();
            size = nextSize;
            conditionCount = nextConditions;
            version = 0;
            for (ShenyuConfig.MatchCacheConfig match : List.of(config.getSelectorMatchCache().getCache(), config.getRuleMatchCache().getCache())) {
                match.setEnabled(cacheHit);
                match.setInitialCapacity(256);
                match.setMaximumSize(1024);
                match.setTtl(600000);
            }
            for (int index = 0; index < size; index++) {
                SelectorData selector = new SelectorData();
                selector.setId(selectorId(index));
                selector.setName(selectorId(index));
                selector.setPluginName("divide");
                selector.setEnabled(true);
                selector.setType(1);
                selector.setSort(index);
                selector.setLogged(false);
                selector.setContinued(true);
                selector.setMatchMode(0);
                selector.setMatchRestful(false);
                selector.setConditionList(conditions("/config/" + index + "/", conditionCount));
                cache.cacheSelectData(selector);
                UpstreamCacheManager.getInstance().submit(selector.getId(), List.of(Upstream.builder().protocol("http://")
                        .url("127.0.0.1:" + upstreamPort).weight(50).healthCheckEnabled(false).build()));
                for (int ruleIndex = 0; ruleIndex < size; ruleIndex++) {
                    RuleData rule = configRule(index, ruleIndex, conditionCount, version);
                    DivideRuleHandle handle = new DivideRuleHandle();
                    handle.setRetry(0);
                    handle.setTimeout(60000);
                    DividePluginDataHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(rule), handle);
                    cache.cacheRuleData(rule);
                }
            }
        }

        private static long heap() {
            return ManagementFactory.getMemoryMXBean().getHeapMemoryUsage().getUsed();
        }

        private static Map<String, Object> retained() throws InterruptedException {
            long beforeGc = metrics().get("gc_count");
            System.gc(); // Control worker only; explicit GC lies outside the measurement window.
            long deadline = System.nanoTime() + Duration.ofSeconds(5).toNanos();
            while (metrics().get("gc_count") <= beforeGc && System.nanoTime() < deadline) {
                Thread.sleep(20);
            }
            return Map.of("heap_bytes", heap(), "gc_observed", metrics().get("gc_count") > beforeGc);
        }
    }

    private static Map<String, Long> metrics() {
        Map<String, Long> result = new LinkedHashMap<>();
        long allocated = 0;
        for (long value : THREADS.getThreadAllocatedBytes(THREADS.getAllThreadIds())) {
            allocated += Math.max(0, value);
        }
        result.put("allocated_bytes", allocated);
        result.put("cpu_ns", OS.getProcessCpuTime());
        result.put("gc_count", ManagementFactory.getGarbageCollectorMXBeans().stream().mapToLong(gc -> gc.getCollectionCount()).sum());
        result.put("gc_ms", ManagementFactory.getGarbageCollectorMXBeans().stream().mapToLong(gc -> gc.getCollectionTime()).sum());
        return result;
    }

    private static void load(final int port, final int count, final int rate, final Path output) throws Exception {
        load(port, count, rate, output, null, 0, 0);
    }

    private static void load(final int port, final int count, final int rate, final Path output,
                             final String state, final int size, final int burst) throws Exception {
        boolean configViews = state != null;
        if (count < 2 || rate < 1) {
            throw new IllegalArgumentException("count >= 2 and rate >= 1 required");
        }
        String path = configViews ? state.equals("no-match") ? "/config/no-match/test" : matchedPath(size) : "/test";
        boolean miss = configViews && state.equals("no-match");
        // Warm the load-generator JVM's connection and JIT before measuring.
        for (int index = 0; index < 100; index++) {
            HttpResponse<String> warm = CLIENT.send(request(port, path), HttpResponse.BodyHandlers.ofString());
            if (configViews) {
                check(expected(warm, miss), "config-view warmup response", null);
            }
        }
        if (configViews) {
            // Exercise both selector-miss and rule-miss paths; neither may fall through to URIPlugin.
            check(expected(CLIENT.send(request(port, "/config/no-match/test"), HttpResponse.BodyHandlers.ofString()), true), "selector miss", null);
            check(expected(CLIENT.send(request(port, "/config/" + (size - 1) + "/no-rule"), HttpResponse.BodyHandlers.ofString()), true), "rule miss", null);
            checkVersion(port, matchedPath(size), "v0");
        }
        get(port, "/_bench/start");
        CompletableFuture<Map<String, Object>> update = null;
        Semaphore concurrent = new Semaphore(128);
        CountDownLatch completed = new CountDownLatch(count);
        AtomicInteger successes = new AtomicInteger();
        AtomicInteger errors = new AtomicInteger();
        long[] latency = new long[count];
        long[] scheduledLatency = new long[count];
        long started = System.nanoTime();
        for (int index = 0; index < count; index++) {
            final int sample = index;
            final long due = started + index * 1000000000L / rate;
            long wait = due - System.nanoTime();
            if (wait > 0) {
                LockSupport.parkNanos(wait);
            }
            if (configViews && burst > 0 && index == count / 2) {
                update = CompletableFuture.supplyAsync(() -> {
                    try {
                        Map<String, Object> data = GSON.fromJson(get(port, "/_bench/config-burst?count=" + burst), Map.class);
                        String version = (String) data.get("version");
                        long published = Long.parseLong((String) data.get("publishedAtNs"));
                        HttpResponse<String> visible = checkVersion(port, matchedPath(size), version);
                        long observed = Long.parseLong(visible.headers().firstValue("X-Bench-Observed-Ns").orElseThrow());
                        data.put("firstVisibleNs", observed - published);
                        data.put("firstRequestVisible", true);
                        return data;
                    } catch (Exception error) {
                        throw new java.util.concurrent.CompletionException(error);
                    }
                });
            }
            concurrent.acquire();
            long sent = System.nanoTime();
            CLIENT.sendAsync(request(port, path), HttpResponse.BodyHandlers.ofString()).whenComplete((response, error) -> {
                long finished = System.nanoTime();
                latency[sample] = finished - sent;
                scheduledLatency[sample] = finished - due;
                if (error == null && expected(response, miss)) {
                    successes.incrementAndGet();
                } else {
                    errors.incrementAndGet();
                }
                concurrent.release();
                completed.countDown();
            });
        }
        if (!completed.await(30, java.util.concurrent.TimeUnit.SECONDS)) {
            throw new IllegalStateException("load did not finish");
        }
        long elapsed = System.nanoTime() - started;
        Map<String, Object> updateResult = update == null ? Map.of() : update.get(30, java.util.concurrent.TimeUnit.SECONDS);
        Map<String, Object> result = GSON.fromJson(get(port, "/_bench/metrics"), Map.class);
        if (configViews) {
            result.put("update", updateResult);
            result.put("state", state);
            result.put("upstream_successes", miss ? 0 : successes.get());
            result.put("retained_after", GSON.fromJson(get(port, "/_bench/retained"), Map.class));
            if (burst > 0) {
                checkVersion(port, matchedPath(size), (String) updateResult.get("version"));
            }
        }
        Arrays.sort(latency);
        Arrays.sort(scheduledLatency);
        result.put("requests", count);
        result.put("successes", successes.get());
        result.put("errors", errors.get());
        result.put("elapsed_ns", elapsed);
        result.put("p95_ns", latency[count * 95 / 100]);
        result.put("scheduled_p95_ns", scheduledLatency[count * 95 / 100]);
        result.put("p99_ns", latency[count * 99 / 100]);
        result.put("scheduled_p99_ns", scheduledLatency[count * 99 / 100]);
        result.put("rate", rate);
        Files.writeString(output, GSON.toJson(result));
    }

    private static boolean expected(final HttpResponse<String> response, final boolean miss) {
        return response.statusCode() == (miss ? 404 : 200) && response.body().equals(miss ? "config-no-match" : BODY);
    }

    private static HttpResponse<String> checkVersion(final int port, final String path, final String version) throws Exception {
        HttpResponse<String> response = CLIENT.send(request(port, path), HttpResponse.BodyHandlers.ofString());
        check(expected(response, false) && version.equals(response.headers().firstValue("X-Bench-Version").orElse(null)),
                "first post-publication request must see " + version, null);
        return response;
    }

    private static HttpRequest request(final int port, final String path) {
        return HttpRequest.newBuilder(URI.create("http://127.0.0.1:" + port + path)).timeout(Duration.ofSeconds(30)).GET().build();
    }

    private static String get(final int port, final String path) throws Exception {
        return CLIENT.send(request(port, path), HttpResponse.BodyHandlers.ofString()).body();
    }

    private static void verify(final int gateway, final Path upstreamFile, final Path output) throws Exception {
        int[] ports = GSON.fromJson(Files.readString(upstreamFile), int[].class);
        List<String> checks = new ArrayList<>();
        get(gateway, "/_bench/config?size=2&state=healthy");
        get(ports[0], "/_bench/fail");
        for (int index = 0; index < 3; index++) {
            HttpRequest specified = HttpRequest.newBuilder(URI.create("http://127.0.0.1:" + gateway + "/test"))
                    .header(Constants.SPECIFY_DOMAIN, "127.0.0.1:" + ports[0]).GET().build();
            CLIENT.send(specified, HttpResponse.BodyHandlers.ofString());
        }
        check(get(gateway, "/test").equals(BODY), "healthy sibling serves while breaker is open", checks);
        get(ports[0], "/_bench/hold");
        Thread.sleep(10200);
        long before = count(ports[0]);
        CompletableFuture<HttpResponse<String>> probe = CLIENT.sendAsync(request(gateway, "/test"), HttpResponse.BodyHandlers.ofString());
        awaitCount(ports[0], before + 1);
        List<CompletableFuture<HttpResponse<String>>> requests = new ArrayList<>();
        for (int index = 0; index < 64; index++) {
            requests.add(CLIENT.sendAsync(request(gateway, "/test"), HttpResponse.BodyHandlers.ofString()));
        }
        for (CompletableFuture<HttpResponse<String>> response : requests) {
            check(response.get(10, java.util.concurrent.TimeUnit.SECONDS).body().equals(BODY), "healthy sibling response", null);
        }
        check(count(ports[0]) == before + 1 && !probe.isDone(), "exactly one held half-open request among 65 concurrent requests", checks);
        probe.cancel(true);
        Thread.sleep(300);
        for (int index = 0; index < 8; index++) {
            check(get(gateway, "/test").equals(BODY), "sibling after cancellation", null);
        }
        check(count(ports[0]) == before + 1, "cancellation reopens the wait window", checks);
        Thread.sleep(10200);
        CompletableFuture<HttpResponse<String>> recovery = CLIENT.sendAsync(request(gateway, "/test"), HttpResponse.BodyHandlers.ofString());
        awaitCount(ports[0], before + 2);
        get(ports[0], "/_bench/ok");
        check(recovery.get(10, java.util.concurrent.TimeUnit.SECONDS).body().equals(BODY), "canceled probe can be retried after the wait window", checks);
        HttpRequest specified = HttpRequest.newBuilder(URI.create("http://127.0.0.1:" + gateway + "/test"))
                .header(Constants.SPECIFY_DOMAIN, "127.0.0.1:" + ports[0]).GET().build();
        check(CLIENT.send(specified, HttpResponse.BodyHandlers.ofString()).body().equals(BODY), "successful probe restores explicit-target traffic", checks);
        get(gateway, "/_bench/config?size=1&state=blocked");
        get(ports[0], "/_bench/hold");
        Thread.sleep(10200);
        long removedBefore = count(ports[0]);
        CompletableFuture<HttpResponse<String>> removed = CLIENT.sendAsync(request(gateway, "/test"), HttpResponse.BodyHandlers.ofString());
        awaitCount(ports[0], removedBefore + 1);
        get(gateway, "/_bench/remove");
        check(!get(gateway, "/test").equals(BODY), "removed configuration rejects new traffic during an in-flight probe", checks);
        get(ports[0], "/_bench/ok");
        check(removed.get(10, java.util.concurrent.TimeUnit.SECONDS).body().equals(BODY), "in-flight probe can finish after configuration removal", checks);
        check(!get(gateway, "/test").equals(BODY), "late completion does not restore removed routing", checks);
        get(gateway, "/_bench/config?size=1&state=blocked");
        get(gateway, "/_bench/retry?count=2");
        get(ports[0], "/_bench/fail");
        Thread.sleep(10200);
        long failedBefore = count(ports[0]);
        check(!get(gateway, "/test").equals(BODY), "failed recovery returns an error", checks);
        check(count(ports[0]) == failedBefore + 1, "failed probe makes one attempt even with retry=2", checks);
        check(!get(gateway, "/test").equals(BODY) && count(ports[0]) == failedBefore + 1,
                "failed probe reopens and immediately blocks subsequent traffic", checks);
        get(ports[0], "/_bench/ok");
        Files.writeString(output, GSON.toJson(checks));
    }

    private static long count(final int port) throws Exception {
        Map<?, ?> value = GSON.fromJson(get(port, "/_bench/stats"), Map.class);
        return ((Number) value.get("requests")).longValue();
    }

    private static void awaitCount(final int port, final long expected) throws Exception {
        long deadline = System.nanoTime() + Duration.ofSeconds(5).toNanos();
        while (count(port) < expected && System.nanoTime() < deadline) {
            Thread.sleep(10);
        }
        if (count(port) != expected) {
            throw new IllegalStateException("expected upstream request count " + expected + " but saw " + count(port));
        }
    }

    private static void check(final boolean condition, final String description, final List<String> checks) {
        if (!condition) {
            throw new IllegalStateException(description);
        }
        if (checks != null) {
            checks.add(description);
        }
    }

    private static final class Endpoint {
        private volatile String mode = "ok";
        private volatile Sinks.Empty<Void> release = Sinks.empty();
        private final AtomicLong requests = new AtomicLong();
        private final AtomicLong active = new AtomicLong();
        private final AtomicLong maxActive = new AtomicLong();
    }
}
