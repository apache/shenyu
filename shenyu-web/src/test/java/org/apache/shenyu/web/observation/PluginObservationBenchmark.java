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

package org.apache.shenyu.web.observation;

import com.sun.management.OperatingSystemMXBean;
import com.sun.management.ThreadMXBean;
import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import org.slf4j.LoggerFactory;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.metrics.PluginObservationSink;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.lang.management.ManagementFactory;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Objects;

/** Standalone same-workload LEGACY root probe; includes root timing/hooks, excludes network and DTO matching. */
public final class PluginObservationBenchmark {

    private PluginObservationBenchmark() {
    }

    /**
     * Run with baseline/disabled/sampled and the appropriate pinned runtime classpath.
     * @param args workload label
     */
    public static void main(final String[] args) {
        ((Logger) LoggerFactory.getLogger(Logger.ROOT_LOGGER_NAME)).setLevel(Level.ERROR);
        String mode = args.length == 0 ? "disabled" : args[0];
        ShenyuConfig config = new ShenyuConfig();
        config.getScheduler().setEnabled(false);
        final ShenyuPlugin pass = new Fixture();
        PluginObservationManager manager = "sampled".equals(mode)
                ? new PluginObservationManager(1024, 100, 128, 1048576, PluginObservationSink.NOOP) : null;
        try {
            ShenyuWebHandler handler = Objects.isNull(manager) ? new ShenyuWebHandler(List.of(pass, pass, pass), null, config)
                    : new ShenyuWebHandler(List.of(pass, pass, pass), null, config, manager);
            MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/probe").build());
            for (int index = 0; index < 20000; index++) {
                handler.handle(exchange).block();
            }
            for (int repeat = 0; repeat < 3; repeat++) {
                measure(mode, handler, exchange, repeat);
            }
            if (Objects.nonNull(manager)) {
                System.out.printf("delivery=%s%n", manager.stats());
            }
        } finally {
            if (Objects.nonNull(manager)) {
                manager.close();
            }
        }
        // This standalone probe also loads core-owned shared background executors.
        System.exit(0);
    }

    private static void measure(final String mode, final ShenyuWebHandler handler, final MockServerWebExchange exchange, final int repeat) {
        ThreadMXBean threads = (ThreadMXBean) ManagementFactory.getThreadMXBean();
        final OperatingSystemMXBean process = (OperatingSystemMXBean) ManagementFactory.getOperatingSystemMXBean();
        threads.setThreadAllocatedMemoryEnabled(true);
        int count = 100000;
        long[] latency = new long[count];
        long allocation = allocated(threads);
        long cpu = process.getProcessCpuTime();
        long started = System.nanoTime();
        for (int index = 0; index < count; index++) {
            long before = System.nanoTime();
            handler.handle(exchange).block();
            latency[index] = System.nanoTime() - before;
        }
        final long elapsed = System.nanoTime() - started;
        cpu = process.getProcessCpuTime() - cpu;
        allocation = allocated(threads) - allocation;
        Arrays.sort(latency);
        System.out.printf(Locale.ROOT, "%s,%d,opsPerSecond=%.1f,p95Nanos=%d,p99Nanos=%d,allocatedBytesPerOp=%.1f,processCpuNanosPerOp=%.1f%n",
                mode, repeat, count * 1000000000.0 / elapsed, latency[count * 95 / 100], latency[count * 99 / 100],
                allocation / (double) count, cpu / (double) count);
    }

    private static long allocated(final ThreadMXBean threads) {
        long total = 0;
        for (long id : threads.getAllThreadIds()) {
            total += Math.max(0, threads.getThreadAllocatedBytes(id));
        }
        return total;
    }

    private static final class Fixture implements ShenyuPlugin {

        @Override
        public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
            return chain.execute(exchange);
        }

        @Override
        public int getOrder() {
            return 0;
        }

        @Override
        public String named() {
            return "probe";
        }
    }
}
