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

package org.apache.shenyu.admin.listener.http;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import com.sun.management.ThreadMXBean;
import com.sun.management.OperatingSystemMXBean;
import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.apache.shenyu.admin.config.properties.HttpSyncProperties;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.listener.DataChangedEventDispatcher;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.h2.jdbcx.JdbcDataSource;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfSystemProperty;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;
import org.springframework.transaction.support.TransactionTemplate;

import java.lang.management.ManagementFactory;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.locks.LockSupport;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Opt-in H2 producer benchmark. Measures real queries and HTTP cache serialization, with
 * in-process fetch latency; it does not measure a deployed gateway's business-request p99.
 */
@EnabledIfSystemProperty(named = "shenyu.http.benchmark", matches = "true")
class HttpCacheRebuildBenchmark {

    private static final ThreadMXBean THREADS = (ThreadMXBean) ManagementFactory.getThreadMXBean();

    private static final OperatingSystemMXBean PROCESS = (OperatingSystemMXBean) ManagementFactory.getOperatingSystemMXBean();

    private final int rows = Integer.getInteger("shenyu.http.benchmark.rows", 1000);

    private final int events = Integer.getInteger("shenyu.http.benchmark.events", 100);

    private final int warmup = Integer.getInteger("shenyu.http.benchmark.warmup", 5);

    private final int samples = Integer.getInteger("shenyu.http.benchmark.samples", 3);

    private final int handleChars = Integer.getInteger("shenyu.http.benchmark.handleChars", 256);

    @Test
    void benchmark() throws Exception {
        THREADS.setThreadAllocatedMemoryEnabled(true);
        Logger root = (Logger) LoggerFactory.getLogger(Logger.ROOT_LOGGER_NAME);
        Level previous = root.getLevel();
        root.setLevel(Level.WARN);
        List<String> output = new ArrayList<>();
        output.add("namespaces,groups,pace_ms,window_ms,sample,rows_per_key,handle_chars,events,queries,serializations,hashes,"
                + "allocated_bytes,cpu_ms,gc_count,gc_ms,elapsed_ms,edit_p99_ms,fetch_p99_ms,max_publish_lag_ms,final_convergence_ms");
        try {
            for (int namespaces : List.of(1, 2)) {
                for (int groups : List.of(1, 2)) {
                    for (int pace : List.of(0, 5, 25)) {
                        compare(namespaces, groups, pace, output);
                    }
                }
            }
            Path path = Path.of(System.getProperty("shenyu.http.benchmark.output", "target/http-cache-rebuild-benchmark.csv"));
            Files.createDirectories(path.toAbsolutePath().getParent());
            Files.write(path, output);
        } finally {
            root.setLevel(previous);
        }
        LoggerFactory.getLogger(getClass()).info("HTTP cache benchmark: JDK {}, processors {}, max heap {}, JVM args {}",
                System.getProperty("java.runtime.version"), Runtime.getRuntime().availableProcessors(),
                Runtime.getRuntime().maxMemory(), ManagementFactory.getRuntimeMXBean().getInputArguments());
    }

    private void compare(final int namespaces, final int groups, final int pace, final List<String> output) throws Exception {
        // Warm both variants independently; alternate measurement order to expose ordering effects.
        for (int sample = 0; sample < samples; sample++) {
            for (int window : sample % 2 == 0 ? List.of(0, 10) : List.of(10, 0)) {
                try (Fixture fixture = new Fixture(namespaces, groups, window)) {
                    for (int iteration = 0; iteration < warmup; iteration++) {
                        fixture.run(events, 0);
                    }
                    for (int iteration = 0; iteration < 20; iteration++) {
                        fixture.keys.forEach(key -> fixture.listener.fetchConfig(key.group, key.namespace));
                    }
                    output.add(fixture.measure(pace, window, sample));
                }
            }
        }
    }

    private static long gcCount() {
        return ManagementFactory.getGarbageCollectorMXBeans().stream().mapToLong(bean -> Math.max(0, bean.getCollectionCount())).sum();
    }

    private static long gcMillis() {
        return ManagementFactory.getGarbageCollectorMXBeans().stream().mapToLong(bean -> Math.max(0, bean.getCollectionTime())).sum();
    }

    private static double p99(final List<Long> samples) {
        List<Long> sorted = new ArrayList<>(samples);
        Collections.sort(sorted);
        return sorted.isEmpty() ? 0 : sorted.get(Math.min(sorted.size() - 1, (int) Math.ceil(sorted.size() * 0.99) - 1)) / 1_000_000.0;
    }

    private final class Fixture implements AutoCloseable {

        private final JdbcTemplate jdbc;

        private final TransactionTemplate transactions;

        private final MeasuredListener listener;

        private final DataChangedEventDispatcher dispatcher;

        private final List<Key> keys = new ArrayList<>();

        private final AtomicInteger queries = new AtomicInteger();

        private final Map<String, String> expected = new ConcurrentHashMap<>();

        private final Map<String, Long> committed = new ConcurrentHashMap<>();

        private int sequence;

        private Fixture(final int namespaces, final int groups, final int window) {
            JdbcDataSource source = new JdbcDataSource();
            source.setURL("jdbc:h2:mem:" + UUID.randomUUID() + ";DB_CLOSE_DELAY=-1");
            jdbc = new JdbcTemplate(source);
            transactions = new TransactionTemplate(new DataSourceTransactionManager(source));
            jdbc.execute("CREATE TABLE config (namespace_id VARCHAR, group_name VARCHAR, id INT, name VARCHAR, handle VARCHAR, PRIMARY KEY(namespace_id, group_name, id))");
            for (int namespace = 0; namespace < namespaces; namespace++) {
                for (ConfigGroupEnum group : List.of(ConfigGroupEnum.RULE, ConfigGroupEnum.SELECTOR).subList(0, groups)) {
                    Key key = new Key("namespace-" + namespace, group);
                    keys.add(key);
                    List<Object[]> batch = new ArrayList<>();
                    for (int row = 0; row < rows; row++) {
                        batch.add(new Object[]{key.namespace, group.name(), row, "initial", "x".repeat(handleChars)});
                    }
                    jdbc.batchUpdate("INSERT INTO config VALUES (?, ?, ?, ?, ?)", batch);
                }
            }
            HttpSyncProperties properties = new HttpSyncProperties();
            properties.setCacheRebuildWindow(Duration.ofMillis(window));
            listener = new MeasuredListener(properties, committed);
            listener.clearCache();
            RuleService rules = mock(RuleService.class);
            when(rules.listAllByNamespaceId(anyString())).thenAnswer(invocation -> loadRules(invocation.getArgument(0)));
            SelectorService selectors = mock(SelectorService.class);
            when(selectors.listAllByNamespaceId(anyString())).thenAnswer(invocation -> loadSelectors(invocation.getArgument(0)));
            ReflectionTestUtils.setField(listener, "ruleService", rules);
            ReflectionTestUtils.setField(listener, "selectorService", selectors);
            dispatcher = new DataChangedEventDispatcher(mock(ApplicationContext.class));
            ReflectionTestUtils.setField(dispatcher, "listeners", List.of(listener));
            ReflectionTestUtils.setField(dispatcher, "clusterProperties", new ClusterProperties());
        }

        private List<RuleData> loadRules(final String namespace) {
            queries.incrementAndGet();
            return jdbc.query("SELECT * FROM config WHERE namespace_id = ? AND group_name = 'RULE' ORDER BY id", (row, index) -> {
                RuleData data = new RuleData();
                data.setId(row.getString("id"));
                data.setName(row.getString("name"));
                data.setHandle(row.getString("handle"));
                data.setNamespaceId(namespace);
                return data;
            }, namespace);
        }

        private List<SelectorData> loadSelectors(final String namespace) {
            queries.incrementAndGet();
            return jdbc.query("SELECT * FROM config WHERE namespace_id = ? AND group_name = 'SELECTOR' ORDER BY id", (row, index) -> {
                SelectorData data = new SelectorData();
                data.setId(row.getString("id"));
                data.setName(row.getString("name"));
                data.setHandle(row.getString("handle"));
                data.setNamespaceId(namespace);
                return data;
            }, namespace);
        }

        private void write(final Key key) {
            String version = "version-" + ++sequence;
            transactions.executeWithoutResult(status -> {
                jdbc.update("UPDATE config SET name = ? WHERE namespace_id = ? AND group_name = ? AND id = 0", version, key.namespace, key.group.name());
                TransactionSynchronizationManager.registerSynchronization(new TransactionSynchronization() {
                    @Override
                    public void afterCommit() {
                        committed.put(version, System.nanoTime());
                        expected.put(key.cacheKey(), version);
                    }
                });
                if (key.group == ConfigGroupEnum.RULE) {
                    RuleData data = new RuleData();
                    data.setNamespaceId(key.namespace);
                    dispatcher.onApplicationEvent(new DataChangedEvent(key.group, DataEventTypeEnum.UPDATE, List.of(data)));
                } else {
                    SelectorData data = new SelectorData();
                    data.setNamespaceId(key.namespace);
                    dispatcher.onApplicationEvent(new DataChangedEvent(key.group, DataEventTypeEnum.UPDATE, List.of(data)));
                }
            });
        }

        private RunResult run(final int count, final int pace) throws Exception {
            List<Long> edits = new ArrayList<>();
            long next = System.nanoTime();
            for (int index = 0; index < count; index++) {
                LockSupport.parkNanos(Math.max(0, next - System.nanoTime()));
                long start = System.nanoTime();
                write(keys.get(index % keys.size()));
                edits.add(System.nanoTime() - start);
                next += TimeUnit.MILLISECONDS.toNanos(pace);
            }
            long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(10);
            while (!expected.entrySet().stream().allMatch(entry -> entry.getValue().equals(listener.published.get(entry.getKey())))) {
                assertTrue(System.nanoTime() < deadline, "cache did not converge");
                Thread.sleep(1);
            }
            long lag = expected.values().stream().mapToLong(version -> listener.publishTimes.get(version) - committed.get(version)).max().orElse(0);
            return new RunResult(edits, lag);
        }

        private String measure(final int pace, final int window, final int sample) throws Exception {
            // Drain any redundant generation from warmup before counters are reset.
            ScheduledBarrier.await(listener);
            queries.set(0);
            listener.serializations.set(0);
            listener.maxLag.set(0);
            AtomicBoolean reading = new AtomicBoolean(true);
            List<Long> fetches = new ArrayList<>();
            CountDownLatch readerDone = new CountDownLatch(1);
            Thread reader = new Thread(() -> {
                while (reading.get()) {
                    for (Key key : keys) {
                        long start = System.nanoTime();
                        listener.fetchConfig(key.group, key.namespace);
                        fetches.add(System.nanoTime() - start);
                    }
                    LockSupport.parkNanos(TimeUnit.MILLISECONDS.toNanos(2));
                }
                try {
                    readerDone.await();
                } catch (InterruptedException ex) {
                    Thread.currentThread().interrupt();
                }
            }, "http-benchmark-reader");
            reader.start();
            Map<Long, Long> before = threadAllocations();
            final long cpuBefore = PROCESS.getProcessCpuTime();
            final long gc = gcCount();
            final long gcTime = gcMillis();
            long start = System.nanoTime();
            RunResult result;
            try {
                result = run(events, pace);
                ScheduledBarrier.await(listener);
            } catch (Exception | AssertionError ex) {
                readerDone.countDown();
                throw ex;
            } finally {
                reading.set(false);
            }
            final long elapsed = System.nanoTime() - start;
            // The reader stays alive until sampled, to include its allocations and CPU.
            final long cpu = PROCESS.getProcessCpuTime() - cpuBefore;
            Map<Long, Long> after = threadAllocations();
            readerDone.countDown();
            reader.join(5000);
            long allocation = 0;
            for (Map.Entry<Long, Long> entry : after.entrySet()) {
                allocation += Math.max(0, entry.getValue() - before.getOrDefault(entry.getKey(), 0L));
            }
            assertEquals(queries.get(), listener.serializations.get());
            assertTrue(result.finalLag <= TimeUnit.MILLISECONDS.toNanos(250), "250ms final publication budget exceeded");
            return String.format(Locale.ROOT, "%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%.3f,%d,%d,%.3f,%.3f,%.3f,%.3f,%.3f",
                    keys.stream().map(key -> key.namespace).distinct().count(), keys.stream().map(key -> key.group).distinct().count(),
                    pace, window, sample, rows, handleChars, events, queries.get(), listener.serializations.get(), listener.serializations.get(),
                    allocation, cpu / 1_000_000.0, gcCount() - gc, gcMillis() - gcTime, elapsed / 1_000_000.0,
                    p99(result.edits), p99(fetches), listener.maxLag.get() / 1_000_000.0, result.finalLag / 1_000_000.0);
        }

        private Map<Long, Long> threadAllocations() {
            Map<Long, Long> result = new HashMap<>();
            for (long id : THREADS.getAllThreadIds()) {
                result.put(id, Math.max(0, THREADS.getThreadAllocatedBytes(id)));
            }
            return result;
        }

        @Override
        public void close() {
            listener.destroy();
            listener.clearCache();
            jdbc.execute("SHUTDOWN");
        }
    }

    private static final class ScheduledBarrier {

        private static void await(final MeasuredListener listener) throws Exception {
            HttpCacheRebuildCoordinator coordinator = (HttpCacheRebuildCoordinator) ReflectionTestUtils.getField(listener, "cacheRebuildCoordinator");
            if (java.util.Objects.nonNull(coordinator)) {
                Map<?, ?> pending = (Map<?, ?>) ReflectionTestUtils.getField(coordinator, "pending");
                long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(10);
                while (true) {
                    synchronized (pending) {
                        if (pending.isEmpty()) {
                            return;
                        }
                    }
                    assertTrue(System.nanoTime() < deadline, "pending rebuilds did not drain");
                    Thread.sleep(1);
                }
            }
        }
    }

    private static final class MeasuredListener extends HttpLongPollingDataChangedListener {

        private final Map<String, Long> committed;

        private final Map<String, String> published = new ConcurrentHashMap<>();

        private final Map<String, Long> publishTimes = new ConcurrentHashMap<>();

        private final AtomicInteger serializations = new AtomicInteger();

        private final AtomicLong maxLag = new AtomicLong();

        private MeasuredListener(final HttpSyncProperties properties, final Map<String, Long> committed) {
            super(properties);
            this.committed = committed;
        }

        @Override
        protected <T> void updateCache(final ConfigGroupEnum group, final List<T> data, final String namespaceId) {
            super.updateCache(group, data, namespaceId);
            serializations.incrementAndGet();
            String version = group == ConfigGroupEnum.RULE ? ((RuleData) data.get(0)).getName() : ((SelectorData) data.get(0)).getName();
            long now = System.nanoTime();
            if (java.util.Objects.isNull(publishTimes.putIfAbsent(version, now))) {
                maxLag.accumulateAndGet(now - committed.getOrDefault(version, now), Math::max);
            }
            published.put(buildCacheKey(namespaceId, group.name()), version);
        }

        private void clearCache() {
            CACHE.clear();
        }
    }

    private static final class Key {

        private final String namespace;

        private final ConfigGroupEnum group;

        private Key(final String namespace, final ConfigGroupEnum group) {
            this.namespace = namespace;
            this.group = group;
        }

        private String cacheKey() {
            return HttpLongPollingDataChangedListener.buildCacheKey(namespace, group.name());
        }
    }

    private static final class RunResult {

        private final List<Long> edits;

        private final long finalLag;

        private RunResult(final List<Long> edits, final long finalLag) {
            this.edits = edits;
            this.finalLag = finalLag;
        }
    }
}
