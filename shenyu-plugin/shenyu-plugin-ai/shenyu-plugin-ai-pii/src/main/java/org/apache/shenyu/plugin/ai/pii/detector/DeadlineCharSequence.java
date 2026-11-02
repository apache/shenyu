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

package org.apache.shenyu.plugin.ai.pii.detector;

import java.util.Objects;

/**
 * A CharSequence that aborts a regex scan once a deadline elapsed. The regular
 * expression engine reads the scanned text almost exclusively through
 * {@link #charAt(int)} and {@link #length()}, so checking the clock every
 * {@link #CHECK_INTERVAL} reads bounds the runtime of any pattern without a
 * per pattern review: a pathological backtracking pattern is stopped by the
 * deadline instead of pinning the worker thread.
 *
 * <p>An instance is created for a single scan and is not shared between
 * requests, so the read counter needs no synchronization.</p>
 */
public final class DeadlineCharSequence implements CharSequence {

    private static final int CHECK_INTERVAL = 64;

    private final CharSequence delegate;

    private final String detectorType;

    private final long deadlineNanos;

    private int reads;

    /**
     * Wrap the text for a scan that must end by the deadline.
     *
     * @param detectorType the detector type, reported by the timeout exception
     * @param delegate the scanned text
     * @param deadlineNanos the absolute deadline in nanoseconds
     */
    private DeadlineCharSequence(final String detectorType, final CharSequence delegate, final long deadlineNanos) {
        this.detectorType = detectorType;
        this.delegate = delegate;
        this.deadlineNanos = deadlineNanos;
    }

    /**
     * Wrap the text for a scan that must end by the deadline.
     *
     * @param detectorType the detector type, reported by the timeout exception
     * @param delegate the scanned text
     * @param deadlineNanos the absolute deadline in nanoseconds, a value in the
     *                     past aborts on the first checked read
     * @return the deadline aware sequence
     */
    public static CharSequence of(final String detectorType, final CharSequence delegate, final long deadlineNanos) {
        return new DeadlineCharSequence(detectorType, Objects.requireNonNull(delegate), deadlineNanos);
    }

    @Override
    public int length() {
        checkDeadline();
        return delegate.length();
    }

    @Override
    public char charAt(final int index) {
        if (++reads >= CHECK_INTERVAL) {
            reads = 0;
            checkDeadline();
        }
        return delegate.charAt(index);
    }

    @Override
    public CharSequence subSequence(final int start, final int end) {
        return new DeadlineCharSequence(detectorType, delegate.subSequence(start, end), deadlineNanos);
    }

    @Override
    public String toString() {
        return delegate.toString();
    }

    private void checkDeadline() {
        if (System.nanoTime() > deadlineNanos) {
            throw new PiiDetectorTimeoutException(detectorType);
        }
    }
}
