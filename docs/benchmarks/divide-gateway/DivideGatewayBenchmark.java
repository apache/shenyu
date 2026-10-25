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
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
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
import org.springframework.web.server.adapter.WebHttpHandlerBuilder;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
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
            case "gateway" -> gateway(Path.of(args[1]), Path.of(args[2]));
            case "load" -> {
                load(Integer.parseInt(args[1]), Integer.parseInt(args[2]), Integer.parseInt(args[3]), Path.of(args[4]));
                System.exit(0);
            }
            case "verify" -> {
                verify(Integer.parseInt(args[1]), Path.of(args[2]), Path.of(args[3]));
                System.exit(0);
            }
            default -> throw new IllegalArgumentException("mode: upstream, gateway, load or verify");
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

    private static void gateway(final Path upstreamFile, final Path ready) throws Exception {
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
        List<ShenyuPlugin> plugins = List.of(new GlobalPlugin(exchange -> {
            ShenyuContext value = new ShenyuContext();
            value.setRpcType("http");
            value.setPath(exchange.getRequest().getURI().getRawPath());
            value.setRealUrl(exchange.getRequest().getURI().getRawPath());
            value.setHttpMethod(exchange.getRequest().getMethod().name());
            return value;
        }), new DividePlugin(), new URIPlugin(), new NettyHttpClientPlugin(netty, 1024 * 1024),
                new ResponsePlugin(Map.of("http", new NettyClientMessageWriter())));
        ShenyuWebHandler handler = new ShenyuWebHandler(plugins, null, config);
        ReactorHttpHandlerAdapter adapter = new ReactorHttpHandlerAdapter(WebHttpHandlerBuilder.webHandler(exchange -> {
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
        // Warm the load-generator JVM's connection and JIT before measuring.
        for (int index = 0; index < 100; index++) {
            get(port, "/test");
        }
        get(port, "/_bench/start");
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
            concurrent.acquire();
            long sent = System.nanoTime();
            CLIENT.sendAsync(request(port, "/test"), HttpResponse.BodyHandlers.ofString()).whenComplete((response, error) -> {
                long finished = System.nanoTime();
                latency[sample] = finished - sent;
                scheduledLatency[sample] = finished - due;
                if (error == null && response.statusCode() == 200 && response.body().equals(BODY)) {
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
        Map<String, Object> result = GSON.fromJson(get(port, "/_bench/metrics"), Map.class);
        Arrays.sort(latency);
        Arrays.sort(scheduledLatency);
        result.put("requests", count);
        result.put("successes", successes.get());
        result.put("errors", errors.get());
        result.put("elapsed_ns", elapsed);
        result.put("p99_ns", latency[count * 99 / 100]);
        result.put("scheduled_p99_ns", scheduledLatency[count * 99 / 100]);
        result.put("rate", rate);
        Files.writeString(output, GSON.toJson(result));
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
