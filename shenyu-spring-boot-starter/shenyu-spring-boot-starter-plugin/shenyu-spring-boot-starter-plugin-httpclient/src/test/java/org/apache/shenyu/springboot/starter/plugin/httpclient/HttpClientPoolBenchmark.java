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

package org.apache.shenyu.springboot.starter.plugin.httpclient;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.sun.management.UnixOperatingSystemMXBean;
import io.micrometer.core.instrument.Gauge;
import io.micrometer.core.instrument.Metrics;
import io.micrometer.core.instrument.simple.SimpleMeterRegistry;
import org.apache.shenyu.plugin.httpclient.config.HttpClientProperties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.autoconfigure.web.ServerProperties;
import reactor.core.Disposable;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.core.scheduler.Schedulers;
import reactor.netty.DisposableServer;
import reactor.netty.http.client.HttpClient;
import reactor.netty.http.server.HttpServer;
import reactor.netty.resources.ConnectionProvider;
import reactor.netty.resources.LoopResources;

import java.lang.management.BufferPoolMXBean;
import java.lang.management.ManagementFactory;
import java.time.Duration;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Opt-in HTTP/streaming pool fixture; never executed by the default test suite.
 */
public final class HttpClientPoolBenchmark {

    private static final Logger LOG = LoggerFactory.getLogger(HttpClientPoolBenchmark.class);

    private final SimpleMeterRegistry registry = new SimpleMeterRegistry();

    private final AtomicLong connections = new AtomicLong();

    private final AtomicInteger openConnections = new AtomicInteger();

    private final AtomicLong peakOpenConnections = new AtomicLong();

    private final AtomicLong peakPending = new AtomicLong();

    private final AtomicLong peakHeap = new AtomicLong();

    private final AtomicLong peakDirect = new AtomicLong();

    private final AtomicLong peakFileDescriptors = new AtomicLong(-1);

    private final ConcurrentLinkedQueue<Long> pendingNanos = new ConcurrentLinkedQueue<>();

    private final AtomicInteger pendingSamples = new AtomicInteger();

    private volatile long initialConnections;

    private volatile long initialCpu;

    private HttpClientPoolBenchmark() {
    }

    /**
     * Start the controlled upstream and minimal forwarding fixture.
     *
     * @param args unused; settings are bench.* system properties
     */
    public static void main(final String[] args) {
        new HttpClientPoolBenchmark().run();
    }

    private void run() {
        Metrics.addRegistry(registry);
        LoopResources upstreamLoops = LoopResources.create("bench-upstream", 1, 2, true);
        final LoopResources proxyLoops = LoopResources.create("bench-proxy", 1, 2, true);
        final LoopResources clientLoops = LoopResources.create("bench-client", 1, 2, true);
        final DisposableServer upstream = upstream(upstreamLoops);
        HttpClientProperties properties = new HttpClientProperties();
        properties.setKeepAlive(Boolean.getBoolean("bench.keepAlive"));
        properties.setReadTimeout(0);
        properties.setResponseTimeout(0L);
        properties.getPool().setType(HttpClientProperties.Pool.PoolType.valueOf(System.getProperty("bench.type", "FIXED")));
        properties.getPool().setMaxConnections(Integer.getInteger("bench.connections", 8));
        properties.getPool().setPendingAcquireMaxCount(Integer.getInteger("bench.pending"));
        properties.getPool().setAcquireTimeout(Long.getLong("bench.acquireTimeout", 250L));
        properties.getPool().setName("pool-benchmark");
        properties.getPool().setMetrics(true);
        HttpClient client = new HttpClientFactory(properties, null, new ServerProperties()) {
            @Override
            public void buildFixedConnectionPool(final HttpClientProperties.Pool pool, final ConnectionProvider.Builder builder) {
                super.buildFixedConnectionPool(pool, builder);
                observePending(builder);
            }

            @Override
            public void buildElasticConnectionPool(final ConnectionProvider.Builder builder) {
                super.buildElasticConnectionPool(builder);
                observePending(builder);
            }
        }.createInstance().runOn(clientLoops, false);
        if (Boolean.getBoolean("bench.aiControl")) {
            client.configuration().connectionProvider().dispose();
            // The unchanged AI starter uses HttpClient.create(), independently of the HTTP factory's provider.
            client = HttpClient.create().runOn(clientLoops, false);
        }
        HttpClient outbound = client;
        ObjectMapper mapper = new ObjectMapper();
        final Sinks.One<Void> stop = Sinks.one();
        DisposableServer proxy = HttpServer.create().host("127.0.0.1").port(Integer.getInteger("bench.port", 18080))
                .runOn(proxyLoops, false).handle((request, response) -> {
                    if (request.uri().equals("/stop")) {
                        return response.sendString(Mono.just("ok")).then().doFinally(ignored -> stop.tryEmitEmpty());
                    }
                    if (request.uri().equals("/reset")) {
                        reset();
                        return response.sendString(Mono.just("ok"));
                    }
                    if (request.uri().equals("/stats")) {
                        return response.header("Content-Type", "application/json").sendString(Mono.fromSupplier(() -> json(mapper, snapshot())));
                    }
                    return outbound.get().uri("http://127.0.0.1:" + upstream.port() + request.uri())
                            .response((remote, content) -> response.status(remote.status()).send(content.retain()).then())
                            .then().onErrorResume(error -> response.status(503).sendString(Mono.just(error.getClass().getSimpleName())).then());
                }).bindNow();
        ScheduledExecutorService sampler = Executors.newSingleThreadScheduledExecutor();
        sampler.scheduleAtFixedRate(this::sample, 0, 20, TimeUnit.MILLISECONDS);
        AtomicBoolean cleaned = new AtomicBoolean();
        Runnable cleanup = () -> {
            if (!cleaned.compareAndSet(false, true)) {
                return;
            }
            sampler.shutdownNow();
            proxy.disposeNow();
            outbound.configuration().connectionProvider().dispose();
            upstream.disposeNow();
            clientLoops.dispose();
            proxyLoops.dispose();
            upstreamLoops.dispose();
            Metrics.removeRegistry(registry);
            registry.close();
        };
        Runtime.getRuntime().addShutdownHook(new Thread(cleanup));
        Map<String, Object> ready = new LinkedHashMap<>();
        ready.put("proxy", "http://127.0.0.1:" + proxy.port());
        ready.put("upstream", "http://127.0.0.1:" + upstream.port());
        ready.put("pid", ProcessHandle.current().pid());
        ready.put("maxConnections", outbound.configuration().connectionProvider().maxConnections());
        LOG.info("POOL_BENCH_READY {}", json(mapper, ready));
        stop.asMono().block();
        cleanup.run();
    }

    private DisposableServer upstream(final LoopResources loops) {
        return HttpServer.create().host("127.0.0.1").port(0).runOn(loops, false).doOnChannelInit((observer, channel, address) -> {
            connections.incrementAndGet();
            openConnections.incrementAndGet();
            channel.closeFuture().addListener(ignored -> openConnections.decrementAndGet());
        }).handle((request, response) -> {
            if (request.uri().equals("/slow")) {
                return Mono.delay(Duration.ofMillis(100)).then(response.sendString(Mono.just("ok")).then());
            }
            if (request.uri().equals("/stream")) {
                return response.header("Content-Type", "text/event-stream")
                        .sendString(Flux.interval(Duration.ofMillis(100)).take(10).map(index -> "data: " + index + "\n\n"));
            }
            return response.sendString(Mono.just("ok"));
        }).bindNow();
    }

    private void observePending(final ConnectionProvider.Builder builder) {
        builder.pendingAcquireTimer((task, timeout) -> {
            long start = System.nanoTime();
            AtomicBoolean recorded = new AtomicBoolean();
            Runnable record = () -> {
                if (recorded.compareAndSet(false, true) && pendingSamples.incrementAndGet() <= 100000) {
                    pendingNanos.add(System.nanoTime() - start);
                }
            };
            Disposable timer = Schedulers.parallel().schedule(() -> {
                record.run();
                task.run();
            }, timeout.toNanos(), TimeUnit.NANOSECONDS);
            return () -> {
                timer.dispose();
                record.run();
            };
        });
    }

    private void reset() {
        initialConnections = connections.get();
        initialCpu = cpu();
        peakOpenConnections.set(0);
        peakPending.set(0);
        peakHeap.set(0);
        peakDirect.set(0);
        peakFileDescriptors.set(-1);
        pendingNanos.clear();
        pendingSamples.set(0);
    }

    private void sample() {
        peakOpenConnections.accumulateAndGet(openConnections.get(), Math::max);
        Gauge pending = registry.find("reactor.netty.connection.provider.pending.connections").gauge();
        if (Objects.nonNull(pending)) {
            peakPending.accumulateAndGet((long) pending.value(), Math::max);
        }
        peakHeap.accumulateAndGet(ManagementFactory.getMemoryMXBean().getHeapMemoryUsage().getUsed(), Math::max);
        long direct = ManagementFactory.getPlatformMXBeans(BufferPoolMXBean.class).stream()
                .filter(pool -> pool.getName().equals("direct")).mapToLong(BufferPoolMXBean::getMemoryUsed).sum();
        peakDirect.accumulateAndGet(direct, Math::max);
        if (ManagementFactory.getOperatingSystemMXBean() instanceof UnixOperatingSystemMXBean unix) {
            peakFileDescriptors.accumulateAndGet(unix.getOpenFileDescriptorCount(), Math::max);
        }
    }

    private Map<String, Object> snapshot() {
        sample();
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("newConnections", connections.get() - initialConnections);
        result.put("peakOpenUpstreamConnections", peakOpenConnections.get());
        result.put("peakPending", peakPending.get());
        result.put("peakHeapBytes", peakHeap.get());
        result.put("peakDirectBytes", peakDirect.get());
        result.put("peakFileDescriptors", peakFileDescriptors.get());
        result.put("cpuNanos", cpu() - initialCpu);
        List<Long> samples = new ArrayList<>(pendingNanos);
        result.put("pendingNanos", samples);
        result.put("pendingSamplesDropped", Math.max(0, pendingSamples.get() - samples.size()));
        return result;
    }

    private long cpu() {
        return ((com.sun.management.OperatingSystemMXBean) ManagementFactory.getOperatingSystemMXBean()).getProcessCpuTime();
    }

    private String json(final ObjectMapper mapper, final Object value) {
        try {
            return mapper.writeValueAsString(value);
        } catch (JsonProcessingException exception) {
            throw new IllegalStateException(exception);
        }
    }
}
