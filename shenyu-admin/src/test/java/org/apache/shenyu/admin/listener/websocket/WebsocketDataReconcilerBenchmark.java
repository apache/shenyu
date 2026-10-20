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

package org.apache.shenyu.admin.listener.websocket;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import com.google.gson.JsonElement;
import com.google.gson.JsonParser;
import com.sun.management.ThreadMXBean;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.apache.shenyu.admin.config.properties.WebsocketSyncProperties;
import org.apache.shenyu.admin.service.NamespacePluginService;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.dto.BaseData;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.WebsocketData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfSystemProperty;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.lang.reflect.Proxy;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Opt-in, dependency-free comparison of reconciliation processing, with in-memory service loads.
 * See docs/websocket-reconciliation-benchmark.md for commands and the integration measurement boundary.
 */
@EnabledIfSystemProperty(named = "shenyu.reconciliation.benchmark", matches = "true")
public final class WebsocketDataReconcilerBenchmark {

    private static final String NAMESPACE = "benchmark-namespace";

    private static final List<ConfigGroupEnum> GROUPS = List.of(ConfigGroupEnum.PLUGIN, ConfigGroupEnum.SELECTOR, ConfigGroupEnum.RULE);

    private final ThreadMXBean threads = (ThreadMXBean) ManagementFactory.getThreadMXBean();

    private volatile long peakHeap;

    private final int warmup = Integer.getInteger("shenyu.benchmark.warmup", 20);

    private final int iterations = Integer.getInteger("shenyu.benchmark.iterations", 5);

    private final int samples = Integer.getInteger("shenyu.benchmark.samples", 3);

    @Test
    public void benchmark() throws IOException {
        assertTrue(threads.isThreadAllocatedMemorySupported());
        assertTrue(threads.isCurrentThreadCpuTimeSupported());
        assertTrue(warmup > 0 && iterations > 0 && samples > 0);
        threads.setThreadAllocatedMemoryEnabled(true);
        threads.setThreadCpuTimeEnabled(true);
        Logger logger = (Logger) LoggerFactory.getLogger(WebsocketDataReconciler.class);
        Level previous = logger.getLevel();
        logger.setLevel(Level.OFF);
        ScheduledThreadPoolExecutor sampler = new ScheduledThreadPoolExecutor(1, ShenyuThreadFactory.create("benchmark-heap", true));
        sampler.scheduleWithFixedDelay(this::sampleHeap, 0, 1, TimeUnit.MILLISECONDS);
        List<String> results = new ArrayList<>();
        results.add("variant,group,rows,handle_chars,changed,sample,iterations,wall_ns_per_tick,cpu_ns_per_tick,allocated_bytes_per_tick,"
                + "peak_observed_heap_bytes,gc_count,gc_ms,load_calls_per_tick,publishes_per_tick,payload_chars");
        try {
            for (int size : List.of(1000, 10000)) {
                for (int handleSize : List.of(256, 4096)) {
                    for (ConfigGroupEnum group : GROUPS) {
                        compare(group, size, handleSize, false, results);
                        compare(group, size, handleSize, true, results);
                    }
                }
            }
            Path output = Path.of(System.getProperty("shenyu.benchmark.output", "target/websocket-reconciliation-benchmark.csv"));
            Files.createDirectories(output.toAbsolutePath().getParent());
            Files.write(output, results);
            LoggerFactory.getLogger(getClass()).info("Reconciliation benchmark: JDK {}, processors {}, max heap {}, JVM args {}, CSV {}",
                    System.getProperty("java.runtime.version"), Runtime.getRuntime().availableProcessors(), Runtime.getRuntime().maxMemory(),
                    ManagementFactory.getRuntimeMXBean().getInputArguments(), output.toAbsolutePath());
        } finally {
            sampler.shutdownNow();
            logger.setLevel(previous);
        }
    }

    private void compare(final ConfigGroupEnum group, final int size, final int handleSize, final boolean changed, final List<String> results) {
        Fixture baseline = new Fixture(group, size, handleSize);
        Fixture canonical = new Fixture(group, size, handleSize);
        try (BenchmarkReconciler oldReconciler = new LegacyReconciler(baseline);
                BenchmarkReconciler newReconciler = new BenchmarkReconciler(canonical)) {
            oldReconciler.reconcileSafely();
            newReconciler.reconcileSafely();
            for (int index = 0; index < warmup; index++) {
                tick(oldReconciler, changed);
                tick(newReconciler, changed);
            }
            for (int sample = 0; sample < samples; sample++) {
                // Alternate order to expose, rather than consistently favor, JIT/thermal/order effects.
                if (sample % 2 == 0) {
                    results.add(measure("baseline", oldReconciler, size, handleSize, changed, sample));
                    results.add(measure("canonical", newReconciler, size, handleSize, changed, sample));
                } else {
                    results.add(measure("canonical", newReconciler, size, handleSize, changed, sample));
                    results.add(measure("baseline", oldReconciler, size, handleSize, changed, sample));
                }
                assertEquals(oldReconciler.getDigestCursor(), newReconciler.getDigestCursor());
                assertEquals(baseline.message, canonical.message);
            }
        }
    }

    private String measure(final String variant, final BenchmarkReconciler reconciler, final int size,
                           final int handleSize, final boolean changed, final int sample) {
        Fixture fixture = reconciler.fixture;
        fixture.loads = 0;
        fixture.publications = 0;
        System.gc();
        resetPeakHeap();
        final long gcCount = gcCount();
        final long gcMillis = gcMillis();
        long threadId = Thread.currentThread().getId();
        long allocated = threads.getThreadAllocatedBytes(threadId);
        long cpu = threads.getCurrentThreadCpuTime();
        long start = System.nanoTime();
        for (int index = 0; index < iterations; index++) {
            tick(reconciler, changed);
        }
        final long elapsed = System.nanoTime() - start;
        final long cpuElapsed = threads.getCurrentThreadCpuTime() - cpu;
        final long allocatedBytes = threads.getThreadAllocatedBytes(threadId) - allocated;
        sampleHeap();
        long observedHeap = peakHeap;
        assertEquals(3 * iterations, fixture.loads);
        assertEquals(changed ? iterations : 0, fixture.publications);
        return String.format(Locale.ROOT, "%s,%s,%d,%d,%s,%d,%d,%.0f,%.0f,%.0f,%d,%d,%d,%d,%d,%d",
                variant, fixture.group, size, handleSize, changed, sample, iterations, elapsed / (double) iterations,
                cpuElapsed / (double) iterations, allocatedBytes / (double) iterations, observedHeap,
                gcCount() - gcCount, gcMillis() - gcMillis, fixture.loads / iterations, fixture.publications / iterations, fixture.message.length());
    }

    private synchronized void sampleHeap() {
        peakHeap = Math.max(peakHeap, ManagementFactory.getMemoryMXBean().getHeapMemoryUsage().getUsed());
    }

    private synchronized void resetPeakHeap() {
        peakHeap = ManagementFactory.getMemoryMXBean().getHeapMemoryUsage().getUsed();
    }

    private void tick(final BenchmarkReconciler reconciler, final boolean changed) {
        if (changed) {
            reconciler.fixture.change();
        }
        reconciler.reconcileSafely();
    }

    private long gcCount() {
        return ManagementFactory.getGarbageCollectorMXBeans().stream().mapToLong(bean -> Math.max(0, bean.getCollectionCount())).sum();
    }

    private long gcMillis() {
        return ManagementFactory.getGarbageCollectorMXBeans().stream().mapToLong(bean -> Math.max(0, bean.getCollectionTime())).sum();
    }

    private static List<BaseData> rows(final ConfigGroupEnum group, final int size, final int handleSize) {
        String handle = "{\"value\":\"" + "x".repeat(handleSize - 12) + "\"}";
        List<BaseData> result = new ArrayList<>(size);
        for (int index = 0; index < size; index++) {
            String id = String.format(Locale.ROOT, "%08d", index);
            ConditionData condition = new ConditionData();
            condition.setParamType("uri");
            condition.setOperator("match");
            condition.setParamName("/");
            condition.setParamValue("/benchmark/" + id);
            BaseData row;
            switch (group) {
                case PLUGIN:
                    row = PluginData.builder().id(id).name("plugin-" + id).config(handle).enabled(true).sort(index).namespaceId(NAMESPACE).build();
                    break;
                case SELECTOR:
                    row = SelectorData.builder().id(id).name("selector-" + id).pluginId("plugin").pluginName("divide")
                            .handle(handle).conditionList(List.of(condition)).matchMode(0).type(1).sort(index).enabled(true).namespaceId(NAMESPACE).build();
                    break;
                case RULE:
                    row = RuleData.builder().id(id).name("rule-" + id).selectorId("selector").pluginName("divide")
                            .handle(handle).conditionDataList(List.of(condition)).matchMode(0).sort(index).enabled(true).namespaceId(NAMESPACE).build();
                    break;
                default:
                    throw new IllegalArgumentException(group.name());
            }
            result.add(row);
        }
        Collections.shuffle(result, new java.util.Random(7840));
        return result;
    }

    private static final class Fixture {

        private final ConfigGroupEnum group;

        private final List<BaseData> data;

        private final String messagePrefix;

        private String message;

        private int revision;

        private int loads;

        private int publications;

        private Fixture(final ConfigGroupEnum group, final int size, final int handleSize) {
            this.group = group;
            this.data = rows(group, size, handleSize);
            this.messagePrefix = "{\"groupType\":\"" + group.name() + "\"";
        }

        private void change() {
            data.get(0).setName("revision-" + (++revision % 2));
        }

        private List<?> load(final ConfigGroupEnum requested) {
            loads++;
            return requested == group ? data : Collections.emptyList();
        }

        private <T> T service(final Class<T> type, final ConfigGroupEnum requested) {
            return type.cast(Proxy.newProxyInstance(type.getClassLoader(), new Class<?>[]{type}, (proxy, method, args) -> load(requested)));
        }
    }

    private static class BenchmarkReconciler extends WebsocketDataReconciler implements AutoCloseable {

        private final Fixture fixture;

        protected BenchmarkReconciler(final Fixture fixture) {
            super(new WebsocketSyncProperties(), new ClusterProperties(), null, null,
                    fixture.service(NamespacePluginService.class, ConfigGroupEnum.PLUGIN),
                    fixture.service(SelectorService.class, ConfigGroupEnum.SELECTOR),
                    fixture.service(RuleService.class, ConfigGroupEnum.RULE), null, null, null, null);
            this.fixture = fixture;
        }

        @Override
        Set<String> activeNamespaceIds() {
            return Set.of(NAMESPACE);
        }

        @Override
        void push(final String namespaceId, final String message) {
            fixture.publications++;
            if (message.startsWith(fixture.messagePrefix)) {
                fixture.message = message;
            }
        }

        @Override
        public void close() {
            destroy();
        }
    }

    /**
     * Frozen pre-optimization algorithm; intentionally retains its join/parse/serialize steps for comparison.
     */
    private static final class LegacyReconciler extends BenchmarkReconciler {

        private LegacyReconciler(final Fixture fixture) {
            super(fixture);
        }

        @Override
        void reconcileSafely() {
            for (ConfigGroupEnum group : GROUPS) {
                String key = NAMESPACE + ":" + group.name();
                List<String> rows = super.fixture.load(group).stream().map(GsonUtils.getInstance()::toJson).sorted().collect(Collectors.toList());
                String digest = DigestUtils.md5Hex(String.join("\n", rows));
                if (digest.equals(getDigestCursor().get(key))) {
                    continue;
                }
                List<JsonElement> trees = rows.stream().map(JsonParser::parseString).collect(Collectors.toList());
                WebsocketData<?> message = new WebsocketData<>(group.name(), DataEventTypeEnum.REFRESH.name(), trees);
                message.setNamespaceId(NAMESPACE);
                message.setFullSnapshot(true);
                push(NAMESPACE, GsonUtils.getInstance().toJson(message));
                getDigestCursor().put(key, digest);
            }
        }
    }
}
