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

import com.sun.management.ThreadMXBean;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.base.circuitbreaker.UpstreamCircuitBreaker;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfSystemProperty;
import reactor.core.publisher.Mono;

import java.io.IOException;
import java.lang.management.GarbageCollectorMXBean;
import java.lang.management.ManagementFactory;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.locks.LockSupport;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Opt-in comparison of request-local filtering, selection and outcome recording.
 * See docs/divide-circuit-breaker-benchmark.md for the measurement boundary.
 */
@EnabledIfSystemProperty(named = "shenyu.divide.benchmark", matches = "true")
public final class DivideCircuitBreakerBenchmark {

    private static final String SELECTOR = "divide-benchmark";

    private final ThreadMXBean threads = (ThreadMXBean) ManagementFactory.getThreadMXBean();

    private final int iterations = Integer.getInteger("shenyu.benchmark.iterations", 100000);

    private final int warmup = Integer.getInteger("shenyu.benchmark.warmup", 50000);

    private final int rate = Integer.getInteger("shenyu.benchmark.rate", 50000);

    private volatile String consumed;

    @Test
    public void benchmark() throws IOException {
        assertTrue(threads.isThreadAllocatedMemorySupported());
        assertTrue(threads.isCurrentThreadCpuTimeSupported());
        assertTrue(iterations > 0 && warmup > 0 && rate > 0);
        threads.setThreadAllocatedMemoryEnabled(true);
        threads.setThreadCpuTimeEnabled(true);
        List<String> rows = new ArrayList<>();
        rows.add("variant,targets,state,sample,requests,successes,requests_per_second,successes_per_second,"
                + "allocated_bytes_per_request,cpu_ns_per_request,p99_ns,gc_count,gc_ms,key_builds_per_request,key_only_cpu_ns_per_request");
        for (int size : List.of(1, 10, 128)) {
            List<Upstream> targets = new ArrayList<>();
            for (int index = 0; index < size; index++) {
                targets.add(Upstream.builder().protocol("http://").url("127.0.0.1:" + (10000 + index)).weight(index + 1).build());
            }
            for (String state : List.of("healthy", "mixed", "blocked")) {
                try {
                    prepare(targets, state);
                    for (int index = 0; index < warmup; index++) {
                        request(targets, index, false);
                        request(targets, index, true);
                    }
                    for (int sample = 0; sample < 3; sample++) {
                        boolean optimizedFirst = sample % 2 != 0;
                        rows.add(measure(targets, state, sample, optimizedFirst));
                        rows.add(measure(targets, state, sample, !optimizedFirst));
                    }
                } finally {
                    reset(targets);
                }
            }
        }
        Path output = Path.of(System.getProperty("shenyu.benchmark.output", "target/divide-circuit-breaker.csv"));
        Files.createDirectories(output.toAbsolutePath().getParent());
        Files.write(output, rows);
        Files.writeString(Path.of(output + ".environment.txt"), "JDK: " + System.getProperty("java.runtime.version")
                + "\nOS: " + System.getProperty("os.name") + " " + System.getProperty("os.arch")
                + "\nProcessors: " + Runtime.getRuntime().availableProcessors()
                + "\nMax heap: " + Runtime.getRuntime().maxMemory()
                + "\nJVM args: " + ManagementFactory.getRuntimeMXBean().getInputArguments()
                + "\nWarmup requests per variant/case: " + warmup + "\nTarget request rate: " + rate + "\n");
    }

    private String measure(final List<Upstream> targets, final String state, final int sample, final boolean optimized) {
        prepare(targets, state);
        long[] latencies = new long[iterations];
        final long initialGcCount = gcCount();
        final long initialGcMillis = gcMillis();
        long allocated = threads.getThreadAllocatedBytes(Thread.currentThread().getId());
        long cpu = threads.getCurrentThreadCpuTime();
        long started = System.nanoTime();
        int successes = 0;
        for (int index = 0; index < iterations; index++) {
            long before = System.nanoTime();
            if (request(targets, index, optimized)) {
                successes++;
            }
            latencies[index] = System.nanoTime() - before;
            // Pace batches identically; latency measures service time, excluding this pause.
            if ((index + 1) % 100 == 0) {
                long wait = started + (index + 1) * 1000000000L / rate - System.nanoTime();
                if (wait > 0) {
                    LockSupport.parkNanos(wait);
                }
            }
        }
        final long elapsed = System.nanoTime() - started;
        cpu = threads.getCurrentThreadCpuTime() - cpu;
        allocated = threads.getThreadAllocatedBytes(Thread.currentThread().getId()) - allocated;
        final long gcCount = gcCount() - initialGcCount;
        final long gcMillis = gcMillis() - initialGcMillis;
        assertEquals("blocked".equals(state) ? 0 : iterations, successes);
        Arrays.sort(latencies);
        int keyBuilds = optimized ? targets.size() : "blocked".equals(state) ? targets.size() * 2 : targets.size() + 1;
        double keyCpu = measureKeyConstruction(targets, keyBuilds);
        return String.format(Locale.ROOT, "%s,%d,%s,%d,%d,%d,%.1f,%.1f,%.1f,%.1f,%d,%d,%d,%d,%.1f",
                optimized ? "optimized" : "baseline", targets.size(), state, sample, iterations, successes,
                iterations * 1000000000.0 / elapsed, successes * 1000000000.0 / elapsed,
                (double) allocated / iterations, (double) cpu / iterations, latencies[(iterations * 99) / 100],
                gcCount, gcMillis, keyBuilds, keyCpu);
    }

    private boolean request(final List<Upstream> targets, final int sequence, final boolean optimized) {
        // Both paths include the unchanged upstream-cache defensive copy.
        List<Upstream> snapshot = new ArrayList<>(targets);
        List<Upstream> allowed;
        CircuitBreakerCandidates candidates = null;
        if (optimized) {
            candidates = CircuitBreakerCandidates.filter(SELECTOR, snapshot);
            allowed = candidates.getUpstreams();
        } else {
            allowed = baseline(snapshot);
        }
        if (allowed.isEmpty()) {
            consumed = null;
            return false;
        }
        // Cycle through every position to include lookup cost, without network / LB noise.
        Upstream selected = allowed.get(sequence % allowed.size());
        String key = optimized ? candidates.getBreakerKey(selected) : UpstreamCircuitBreaker.buildKey(SELECTOR, selected);
        UpstreamCircuitBreaker.recordOutcome(Mono.empty(), key).block();
        consumed = key;
        return true;
    }

    private List<Upstream> baseline(final List<Upstream> upstreams) {
        // Original DividePlugin.filterByCircuitBreaker at 297ac6f84.
        List<Upstream> allowed = new ArrayList<>(upstreams.size());
        for (Upstream upstream : upstreams) {
            if (UpstreamCircuitBreaker.getInstance().isRequestAllowed(UpstreamCircuitBreaker.buildKey(SELECTOR, upstream))) {
                allowed.add(upstream);
            }
        }
        if (!allowed.isEmpty()) {
            return allowed;
        }
        for (Upstream upstream : upstreams) {
            if (UpstreamCircuitBreaker.getInstance().tryAcquireHalfOpenProbe(UpstreamCircuitBreaker.buildKey(SELECTOR, upstream))) {
                return Collections.singletonList(upstream);
            }
        }
        return Collections.emptyList();
    }

    private double measureKeyConstruction(final List<Upstream> targets, final int count) {
        // Separate key-only workload, not attribution of CPU in the full request.
        int repetitions = Math.max(20000, 2000000 / count);
        long cpu = threads.getCurrentThreadCpuTime();
        for (int index = 0; index < repetitions; index++) {
            for (int key = 0; key < count; key++) {
                consumed = UpstreamCircuitBreaker.buildKey(SELECTOR, targets.get(key % targets.size()));
            }
        }
        return (threads.getCurrentThreadCpuTime() - cpu) / (double) repetitions;
    }

    private void prepare(final List<Upstream> targets, final String state) {
        reset(targets);
        for (int index = 0; index < targets.size(); index++) {
            if ("blocked".equals(state) || "mixed".equals(state) && index % 2 != 0) {
                String key = UpstreamCircuitBreaker.buildKey(SELECTOR, targets.get(index));
                for (int failure = 0; failure < UpstreamCircuitBreaker.DEFAULT_FAILURE_THRESHOLD; failure++) {
                    UpstreamCircuitBreaker.getInstance().recordFailure(key);
                }
            }
        }
    }

    private void reset(final List<Upstream> targets) {
        targets.forEach(upstream -> UpstreamCircuitBreaker.getInstance().reset(UpstreamCircuitBreaker.buildKey(SELECTOR, upstream)));
    }

    private long gcCount() {
        return ManagementFactory.getGarbageCollectorMXBeans().stream().mapToLong(GarbageCollectorMXBean::getCollectionCount).sum();
    }

    private long gcMillis() {
        return ManagementFactory.getGarbageCollectorMXBeans().stream().mapToLong(GarbageCollectorMXBean::getCollectionTime).sum();
    }
}
