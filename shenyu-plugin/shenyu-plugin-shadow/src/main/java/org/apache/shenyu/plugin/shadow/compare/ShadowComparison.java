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

package org.apache.shenyu.plugin.shadow.compare;

import java.util.Objects;

/**
 * One bounded, redacted comparison result.
 *
 * <p>The detail names what differed (a status pair, a header name or a JSON
 * path) but never carries response values, so reports can be exported without
 * leaking payloads or credentials.</p>
 */
public final class ShadowComparison {

    private static final int MAX_DETAIL_LENGTH = 200;

    private final ShadowOutcome outcome;

    private final String detail;

    public ShadowComparison(final ShadowOutcome outcome, final String detail) {
        this.outcome = Objects.requireNonNull(outcome);
        this.detail = truncate(detail);
    }

    /**
     * Build a match result.
     *
     * @return the comparison result
     */
    public static ShadowComparison match() {
        return new ShadowComparison(ShadowOutcome.MATCH, "");
    }

    /**
     * Build a normalized result.
     *
     * @param detail what disappeared after normalization
     * @return the comparison result
     */
    public static ShadowComparison normalized(final String detail) {
        return new ShadowComparison(ShadowOutcome.NORMALIZED, detail);
    }

    /**
     * Build a mismatch result.
     *
     * @param detail what differed, without response values
     * @return the comparison result
     */
    public static ShadowComparison mismatch(final String detail) {
        return new ShadowComparison(ShadowOutcome.MISMATCH, detail);
    }

    /**
     * Build a truncated result.
     *
     * @param detail why the comparison was incomplete
     * @return the comparison result
     */
    public static ShadowComparison truncated(final String detail) {
        return new ShadowComparison(ShadowOutcome.TRUNCATED, detail);
    }

    /**
     * Build a shadow failure result.
     *
     * @param detail why the shadow call failed
     * @return the comparison result
     */
    public static ShadowComparison failed(final String detail) {
        return new ShadowComparison(ShadowOutcome.SHADOW_FAILED, detail);
    }

    /**
     * Gets the outcome.
     *
     * @return the outcome
     */
    public ShadowOutcome getOutcome() {
        return outcome;
    }

    /**
     * Gets the bounded detail.
     *
     * @return the bounded detail
     */
    public String getDetail() {
        return detail;
    }

    private static String truncate(final String detail) {
        if (Objects.isNull(detail)) {
            return "";
        }
        return detail.length() > MAX_DETAIL_LENGTH ? detail.substring(0, MAX_DETAIL_LENGTH) : detail;
    }
}
