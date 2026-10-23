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

import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Configuration for the opt-in gateway diagnostic bundle facility.
 *
 * <p>The feature is disabled by default. Every bound is a hard ceiling that keeps a
 * support bundle small and cheap: an oversized or slow collection degrades to a partial
 * bundle rather than growing without limit or blocking gateway traffic.</p>
 */
@ConfigurationProperties(prefix = "shenyu.diagnostics")
public class DiagnosticProperties {

    private static final int DEFAULT_MAX_TOTAL_BYTES = 4 * 1024 * 1024;

    private static final int DEFAULT_MAX_SECTION_BYTES = 512 * 1024;

    private boolean enabled;

    private int maxTotalBytes = DEFAULT_MAX_TOTAL_BYTES;

    private int maxSectionBytes = DEFAULT_MAX_SECTION_BYTES;

    private long sectionTimeoutMs = 2000L;

    private long totalTimeoutMs = 8000L;

    private int maxConcurrentJobs = 2;

    /**
     * Whether the diagnostic bundle endpoint is exposed. Disabled by default.
     *
     * @return true if enabled
     */
    public boolean isEnabled() {
        return enabled;
    }

    /**
     * Set whether the diagnostic bundle endpoint is exposed.
     *
     * @param enabled the enabled flag
     */
    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
    }

    /**
     * Get the maximum total uncompressed bundle size in bytes.
     *
     * @return the max total bytes
     */
    public int getMaxTotalBytes() {
        return maxTotalBytes;
    }

    /**
     * Set the maximum total uncompressed bundle size in bytes.
     *
     * @param maxTotalBytes the max total bytes
     */
    public void setMaxTotalBytes(final int maxTotalBytes) {
        this.maxTotalBytes = maxTotalBytes;
    }

    /**
     * Get the maximum size of a single section in bytes.
     *
     * @return the max section bytes
     */
    public int getMaxSectionBytes() {
        return maxSectionBytes;
    }

    /**
     * Set the maximum size of a single section in bytes.
     *
     * @param maxSectionBytes the max section bytes
     */
    public void setMaxSectionBytes(final int maxSectionBytes) {
        this.maxSectionBytes = maxSectionBytes;
    }

    /**
     * Get the per-section collection timeout in milliseconds.
     *
     * @return the section timeout ms
     */
    public long getSectionTimeoutMs() {
        return sectionTimeoutMs;
    }

    /**
     * Set the per-section collection timeout in milliseconds.
     *
     * <p>The value bounds both the budget handed to a collector and the wait for its result, and
     * it is capped by the remaining total collection time. Values below one millisecond, including
     * zero, still allow a one millisecond attempt; they do not mean "no per-section limit".</p>
     *
     * @param sectionTimeoutMs the section timeout ms
     */
    public void setSectionTimeoutMs(final long sectionTimeoutMs) {
        this.sectionTimeoutMs = sectionTimeoutMs;
    }

    /**
     * Get the total collection timeout across all sections in milliseconds.
     *
     * <p>It bounds only the time spent starting and waiting for section collectors: each
     * collector's own budget and the wait for its result are capped by the remaining total time.
     * Manifest identity lookup and archive assembly happen after collection and are not covered
     * by this bound.</p>
     *
     * @return the total timeout ms
     */
    public long getTotalTimeoutMs() {
        return totalTimeoutMs;
    }

    /**
     * Set the total collection timeout across all sections in milliseconds.
     *
     * @param totalTimeoutMs the total timeout ms
     */
    public void setTotalTimeoutMs(final long totalTimeoutMs) {
        this.totalTimeoutMs = totalTimeoutMs;
    }

    /**
     * Get the maximum number of simultaneous bundle collection jobs.
     *
     * @return the max concurrent jobs
     */
    public int getMaxConcurrentJobs() {
        return maxConcurrentJobs;
    }

    /**
     * Set the maximum number of simultaneous bundle collection jobs.
     *
     * @param maxConcurrentJobs the max concurrent jobs
     */
    public void setMaxConcurrentJobs(final int maxConcurrentJobs) {
        this.maxConcurrentJobs = maxConcurrentJobs;
    }
}
