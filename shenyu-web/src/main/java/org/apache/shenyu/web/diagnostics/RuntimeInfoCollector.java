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

import org.apache.shenyu.common.utils.GsonUtils;

import java.lang.management.ManagementFactory;
import java.lang.management.MemoryUsage;
import java.lang.management.OperatingSystemMXBean;
import java.lang.management.RuntimeMXBean;
import java.lang.management.ThreadMXBean;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

/**
 * Collects bounded JVM, OS, memory and thread counters for the local gateway.
 *
 * <p>Only aggregate counters are published. No heap dump, thread dump or environment variable
 * is collected, matching the issue's explicit exclusion of high-volume or secret-bearing data
 * from the default bundle.</p>
 */
public final class RuntimeInfoCollector implements SectionCollector {

    private static final String NAME = "runtime";

    @Override
    public String name() {
        return NAME;
    }

    @Override
    public DiagnosticSection collect(final DiagnosticBudget budget) {
        final long observedAt = System.currentTimeMillis();
        final long startNanos = System.nanoTime();
        if (budget.shouldStop()) {
            return DiagnosticSection.of(NAME, SectionStatus.SKIPPED, observedAt, 0L, "budget exhausted before start");
        }
        Map<String, Object> runtime = new LinkedHashMap<>();
        runtime.put("jvm", jvm());
        runtime.put("os", os());
        runtime.put("memory", memory());
        runtime.put("threads", threads());
        String json = GsonUtils.getInstance().toJson(runtime);
        return DiagnosticSection.ok(NAME, observedAt, elapsedMs(startNanos), json);
    }

    private Map<String, Object> jvm() {
        RuntimeMXBean runtime = ManagementFactory.getRuntimeMXBean();
        Map<String, Object> jvm = new LinkedHashMap<>();
        jvm.put("vmName", runtime.getVmName());
        jvm.put("vmVersion", runtime.getVmVersion());
        jvm.put("specVersion", runtime.getSpecVersion());
        jvm.put("bootTimeEpochMs", runtime.getStartTime());
        jvm.put("uptimeMs", runtime.getUptime());
        return jvm;
    }

    private Map<String, Object> os() {
        OperatingSystemMXBean os = ManagementFactory.getOperatingSystemMXBean();
        Map<String, Object> info = new LinkedHashMap<>();
        info.put("name", os.getName());
        info.put("arch", os.getArch());
        info.put("availableProcessors", os.getAvailableProcessors());
        return info;
    }

    private Map<String, Object> memory() {
        MemoryUsage heap = ManagementFactory.getMemoryMXBean().getHeapMemoryUsage();
        MemoryUsage nonHeap = ManagementFactory.getMemoryMXBean().getNonHeapMemoryUsage();
        Map<String, Object> memory = new LinkedHashMap<>();
        memory.put("heapUsedBytes", heap.getUsed());
        memory.put("heapCommittedBytes", heap.getCommitted());
        memory.put("heapMaxBytes", heap.getMax());
        memory.put("nonHeapUsedBytes", nonHeap.getUsed());
        return memory;
    }

    private Map<String, Object> threads() {
        ThreadMXBean threads = ManagementFactory.getThreadMXBean();
        Map<String, Object> info = new LinkedHashMap<>();
        info.put("count", threads.getThreadCount());
        info.put("peak", threads.getPeakThreadCount());
        info.put("daemon", threads.getDaemonThreadCount());
        return info;
    }

    private long elapsedMs(final long startNanos) {
        return TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - startNanos);
    }
}
