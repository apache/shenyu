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

package org.apache.shenyu.web.diagnostics;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.lang.management.ManagementFactory;
import java.lang.management.RuntimeMXBean;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * Orchestrates bounded, partial-tolerant diagnostic collection and archive assembly.
 *
 * <p>Each collector runs on a worker thread with its own time and size budget. A collector that
 * times out, is cancelled or throws becomes a {@link SectionStatus#FAILED} section instead of
 * failing the whole bundle, and exhausting the total budget marks the remaining sections
 * {@link SectionStatus#SKIPPED}. Simultaneous bundle jobs are limited; when the limit is reached
 * the request degrades to an overload bundle that says so honestly rather than queueing behind
 * gateway traffic.</p>
 */
public final class DiagnosticBundleService {

    private static final Logger LOG = LoggerFactory.getLogger(DiagnosticBundleService.class);

    private static final String UNKNOWN = "unknown";

    private final List<SectionCollector> collectors;

    private final DiagnosticProperties properties;

    private final DiagnosticBundleWriter writer;

    private final Semaphore jobs;

    /**
     * Instantiates a new bundle service.
     *
     * @param collectors the section collectors
     * @param properties the diagnostic bounds
     * @param writer     the archive writer
     */
    public DiagnosticBundleService(final List<SectionCollector> collectors, final DiagnosticProperties properties,
                                   final DiagnosticBundleWriter writer) {
        this.collectors = List.copyOf(collectors);
        this.properties = properties;
        this.writer = writer;
        this.jobs = new Semaphore(Math.max(1, properties.getMaxConcurrentJobs()));
    }

    /**
     * Collect all sections and assemble the archive.
     *
     * @return the bundle archive bytes
     */
    public byte[] bundle() {
        if (!jobs.tryAcquire()) {
            return writeOverloadBundle();
        }
        try {
            return writer.write(collect());
        } catch (IOException ex) {
            throw new UncheckedIOException("failed to assemble diagnostic bundle", ex);
        } finally {
            jobs.release();
        }
    }

    /**
     * Collect all sections into a manifest without archiving.
     *
     * @return the manifest carrying every section outcome
     */
    public DiagnosticManifest collect() {
        long generatedAt = System.currentTimeMillis();
        long deadlineNanos = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(properties.getTotalTimeoutMs());
        List<DiagnosticSection> sections = new ArrayList<>(collectors.size());
        ExecutorService executor = Executors.newSingleThreadExecutor(runnable -> {
            Thread thread = new Thread(runnable, "shenyu-diagnostics");
            thread.setDaemon(true);
            return thread;
        });
        try {
            for (SectionCollector collector : collectors) {
                sections.add(runCollector(executor, collector, deadlineNanos));
            }
        } finally {
            executor.shutdownNow();
        }
        return new DiagnosticManifest(generatedAt, instanceIdentity(), softwareVersions(), sections);
    }

    private DiagnosticSection runCollector(final ExecutorService executor, final SectionCollector collector, final long deadlineNanos) {
        String name = collector.name();
        long observedAt = System.currentTimeMillis();
        if (System.nanoTime() - deadlineNanos >= 0) {
            return DiagnosticSection.of(name, SectionStatus.SKIPPED, observedAt, 0L, "total budget exhausted");
        }
        DiagnosticBudget budget = new DiagnosticBudget(properties.getSectionTimeoutMs(), properties.getMaxSectionBytes());
        long startNanos = System.nanoTime();
        Future<DiagnosticSection> future = executor.submit(() -> collector.collect(budget));
        try {
            return capSection(future.get(properties.getSectionTimeoutMs(), TimeUnit.MILLISECONDS), startNanos);
        } catch (TimeoutException ex) {
            budget.cancel();
            future.cancel(true);
            return DiagnosticSection.of(name, SectionStatus.FAILED, observedAt, elapsedMs(startNanos), "section timeout");
        } catch (InterruptedException ex) {
            Thread.currentThread().interrupt();
            budget.cancel();
            future.cancel(true);
            return DiagnosticSection.of(name, SectionStatus.FAILED, observedAt, elapsedMs(startNanos), "interrupted");
        } catch (ExecutionException ex) {
            return DiagnosticSection.of(name, SectionStatus.FAILED, observedAt, elapsedMs(startNanos), failureReason(ex.getCause()));
        }
    }

    private DiagnosticSection capSection(final DiagnosticSection section, final long startNanos) {
        long durationMs = elapsedMs(startNanos);
        if (Objects.isNull(section)) {
            return DiagnosticSection.of(UNKNOWN, SectionStatus.FAILED, System.currentTimeMillis(), durationMs, "collector returned null");
        }
        byte[] content = section.getContent();
        if (content.length > properties.getMaxSectionBytes()) {
            // Cut on a character boundary: the cap is a byte count but the content is text, and a
            // half-encoded character would make the archived entry undecodable. A cap below one
            // whole character leaves no usable content, which is SKIPPED rather than empty OK.
            int limit = Utf8Boundary.limit(content, properties.getMaxSectionBytes());
            if (limit == 0) {
                return DiagnosticSection.of(section.getName(), SectionStatus.SKIPPED,
                        section.getObservedAtEpochMs(), durationMs, "section size cap");
            }
            byte[] capped = Arrays.copyOf(content, limit);
            return DiagnosticSection.truncated(section.getName(), section.getObservedAtEpochMs(), durationMs, capped, "section size cap");
        }
        return section;
    }

    private byte[] writeOverloadBundle() {
        long now = System.currentTimeMillis();
        List<DiagnosticSection> sections = new ArrayList<>(collectors.size());
        for (SectionCollector collector : collectors) {
            sections.add(DiagnosticSection.of(collector.name(), SectionStatus.SKIPPED, now, 0L, "concurrent collection limit reached"));
        }
        DiagnosticManifest manifest = new DiagnosticManifest(now, instanceIdentity(), softwareVersions(), sections);
        try {
            return writer.write(manifest);
        } catch (IOException ex) {
            throw new UncheckedIOException("failed to assemble overload diagnostic bundle", ex);
        }
    }

    private Map<String, Object> instanceIdentity() {
        Map<String, Object> instance = new LinkedHashMap<>();
        RuntimeMXBean runtime = ManagementFactory.getRuntimeMXBean();
        instance.put("bootTimeEpochMs", runtime.getStartTime());
        instance.put("uptimeMs", runtime.getUptime());
        instance.put("hostAddress", hostAddress());
        return instance;
    }

    private Map<String, String> softwareVersions() {
        Map<String, String> versions = new LinkedHashMap<>();
        String shenyuVersion = DiagnosticBundleService.class.getPackage().getImplementationVersion();
        versions.put("shenyu", Objects.isNull(shenyuVersion) ? UNKNOWN : shenyuVersion);
        versions.put("java", System.getProperty("java.version", UNKNOWN));
        versions.put("osName", System.getProperty("os.name", UNKNOWN));
        versions.put("osArch", System.getProperty("os.arch", UNKNOWN));
        return versions;
    }

    private String hostAddress() {
        try {
            return InetAddress.getLocalHost().getHostAddress();
        } catch (UnknownHostException ex) {
            LOG.debug("unable to resolve local host address for diagnostic manifest", ex);
            return UNKNOWN;
        }
    }

    private long elapsedMs(final long startNanos) {
        return TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - startNanos);
    }

    private String failureReason(final Throwable cause) {
        return Objects.isNull(cause) ? "collector failure" : cause.getClass().getSimpleName();
    }
}
