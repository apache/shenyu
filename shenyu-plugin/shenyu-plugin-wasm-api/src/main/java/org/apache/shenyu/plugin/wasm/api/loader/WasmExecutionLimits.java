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

package org.apache.shenyu.plugin.wasm.api.loader;

/**
 * Immutable, startup-only execution and per-instance linear-memory limits.
 */
public final class WasmExecutionLimits {

    private final long timeoutMillis;

    private final long initializationTimeoutMillis;

    private final int maxMemoryPages;

    /**
     * Create limits. Zero disables the corresponding limit.
     *
     * @param timeoutMillis export deadline, including pool wait
     * @param initializationTimeoutMillis deadline for compilation/initialization
     * @param maxMemoryPages maximum 64 KiB linear-memory pages per instance
     */
    public WasmExecutionLimits(final long timeoutMillis, final long initializationTimeoutMillis, final int maxMemoryPages) {
        if (timeoutMillis < 0 || initializationTimeoutMillis < 0 || maxMemoryPages < 0 || maxMemoryPages > 65536) {
            throw new IllegalArgumentException("WASM deadlines must be nonnegative and memory pages must be between 0 and 65536");
        }
        this.timeoutMillis = timeoutMillis;
        this.initializationTimeoutMillis = initializationTimeoutMillis;
        this.maxMemoryPages = maxMemoryPages;
    }

    /**
     * Resolve global JVM properties with an optional fully-qualified plugin-class override.
     *
     * @param pluginClass resource-owning plugin/handler class
     * @return immutable limits
     */
    public static WasmExecutionLimits fromSystemProperties(final Class<?> pluginClass) {
        String prefix = "shenyu.wasm.";
        String override = prefix + pluginClass.getName() + ".";
        long timeout = Long.parseLong(property(prefix, override, "execution-timeout-millis", "0"));
        long initialization = Long.parseLong(property(prefix, override, "initialization-timeout-millis", timeout > 0 ? "10000" : "0"));
        int pages = Integer.parseInt(property(prefix, override, "max-memory-pages", "0"));
        return new WasmExecutionLimits(timeout, initialization, pages);
    }

    private static String property(final String prefix, final String override, final String name, final String fallback) {
        return System.getProperty(override + name, System.getProperty(prefix + name, fallback));
    }

    /**
     * Get the export deadline.
     * @return milliseconds, or zero when disabled
     */
    public long getTimeoutMillis() {
        return timeoutMillis;
    }

    /**
     * Get the construction deadline.
     * @return milliseconds, or zero when disabled
     */
    public long getInitializationTimeoutMillis() {
        return initializationTimeoutMillis;
    }

    /**
     * Get the per-instance linear-memory ceiling.
     * @return pages, or zero when disabled
     */
    public int getMaxMemoryPages() {
        return maxMemoryPages;
    }
}

