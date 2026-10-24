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

import java.nio.charset.StandardCharsets;
import java.util.Arrays;

/**
 * An immutable, already-redacted diagnostic section.
 *
 * <p>Content is stored as bytes so the writer never has to re-encode it. The byte
 * array is defensively copied on the way in and out; callers cannot mutate the state
 * of a published section.</p>
 */
public final class DiagnosticSection {

    private final String name;

    private final SectionStatus status;

    private final long observedAtEpochMs;

    private final long durationMs;

    private final byte[] content;

    private final String reason;

    private DiagnosticSection(final String name, final SectionStatus status, final long observedAtEpochMs,
                              final long durationMs, final byte[] content, final String reason) {
        this.name = name;
        this.status = status;
        this.observedAtEpochMs = observedAtEpochMs;
        this.durationMs = durationMs;
        this.content = Arrays.copyOf(content, content.length);
        this.reason = reason;
    }

    /**
     * Create a complete section.
     *
     * @param name              the section name
     * @param observedAtEpochMs the observation timestamp in epoch milliseconds
     * @param durationMs        the collection duration in milliseconds
     * @param content           the already-redacted UTF-8 content
     * @return the section
     */
    public static DiagnosticSection ok(final String name, final long observedAtEpochMs, final long durationMs, final byte[] content) {
        return new DiagnosticSection(name, SectionStatus.OK, observedAtEpochMs, durationMs, content, null);
    }

    /**
     * Create a complete section from a UTF-8 string.
     *
     * @param name              the section name
     * @param observedAtEpochMs the observation timestamp in epoch milliseconds
     * @param durationMs        the collection duration in milliseconds
     * @param content           the already-redacted content
     * @return the section
     */
    public static DiagnosticSection ok(final String name, final long observedAtEpochMs, final long durationMs, final String content) {
        return ok(name, observedAtEpochMs, durationMs, content.getBytes(StandardCharsets.UTF_8));
    }

    /**
     * Create a section that carries an explicit status and reason with no content.
     *
     * @param name              the section name
     * @param status            the section status
     * @param observedAtEpochMs the observation timestamp in epoch milliseconds
     * @param durationMs        the collection duration in milliseconds
     * @param reason            a short, non-secret reason
     * @return the section
     */
    public static DiagnosticSection of(final String name, final SectionStatus status, final long observedAtEpochMs,
                                       final long durationMs, final String reason) {
        return new DiagnosticSection(name, status, observedAtEpochMs, durationMs, new byte[0], reason);
    }

    /**
     * Create a section whose content was cut to satisfy the size budget.
     *
     * @param name              the section name
     * @param observedAtEpochMs the observation timestamp in epoch milliseconds
     * @param durationMs        the collection duration in milliseconds
     * @param content           the truncated, already-redacted content
     * @param reason            a short, non-secret reason
     * @return the section
     */
    public static DiagnosticSection truncated(final String name, final long observedAtEpochMs, final long durationMs,
                                              final byte[] content, final String reason) {
        return new DiagnosticSection(name, SectionStatus.TRUNCATED, observedAtEpochMs, durationMs, content, reason);
    }

    /**
     * Get the section name.
     *
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * Get the section status.
     *
     * @return the status
     */
    public SectionStatus getStatus() {
        return status;
    }

    /**
     * Get the observation timestamp in epoch milliseconds.
     *
     * @return the observed at epoch ms
     */
    public long getObservedAtEpochMs() {
        return observedAtEpochMs;
    }

    /**
     * Get the collection duration in milliseconds.
     *
     * @return the duration ms
     */
    public long getDurationMs() {
        return durationMs;
    }

    /**
     * Get a copy of the section content.
     *
     * @return the content bytes
     */
    public byte[] getContent() {
        return Arrays.copyOf(content, content.length);
    }

    /**
     * Get the content size in bytes.
     *
     * @return the size bytes
     */
    public int getSizeBytes() {
        return content.length;
    }

    /**
     * Get the non-secret reason, may be {@code null} for {@link SectionStatus#OK}.
     *
     * @return the reason
     */
    public String getReason() {
        return reason;
    }
}
