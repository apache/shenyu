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

package org.apache.shenyu.common.dto.convert.rule;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * The shadow traffic plugin rule handle.
 *
 * <p>The handle describes which requests may be mirrored to an independent
 * shadow destination. Every bound is explicit so a single rule can never
 * introduce unbounded work, memory or connection usage on the gateway path.</p>
 */
public class ShadowHandle {

    private static final int DEFAULT_MAX_CONCURRENT = 8;

    private static final int DEFAULT_QUEUE_CAPACITY = 64;

    private static final int DEFAULT_TIMEOUT_MILLIS = 1000;

    private static final int DEFAULT_MAX_BODY_BYTES = 64 * 1024;

    private static final double DEFAULT_SAMPLE_RATE = 0D;

    /**
     * Whether this rule mirrors selected requests.
     */
    private Boolean enabled = Boolean.FALSE;

    /**
     * Absolute shadow destination, for example {@code http://candidate:8080}.
     * Only http and https are accepted and user info is rejected.
     */
    private String target;

    /**
     * HTTP methods eligible for shadowing. Anything outside this list is never
     * mirrored, so a write is mirrored only after an operator names its method.
     */
    private List<String> methods = Arrays.asList("GET", "HEAD", "OPTIONS", "TRACE");

    /**
     * Fraction of eligible requests that are mirrored, in [0, 1].
     */
    private Double sampleRate = DEFAULT_SAMPLE_RATE;

    /**
     * Maximum shadow calls in flight for this rule.
     */
    private Integer maxConcurrent = DEFAULT_MAX_CONCURRENT;

    /**
     * Maximum queued shadow calls for this rule. Overflow drops the sample.
     * A value of 0 keeps at most one queued call instead of disabling the queue.
     */
    private Integer queueCapacity = DEFAULT_QUEUE_CAPACITY;

    /**
     * Budget for one shadow attempt, connection setup included.
     */
    private Integer timeoutMillis = DEFAULT_TIMEOUT_MILLIS;

    /**
     * Maximum request body bytes copied for a shadow call. A larger body is
     * reported as truncated instead of sending a partial request.
     */
    private Integer maxRequestBodyBytes = DEFAULT_MAX_BODY_BYTES;

    /**
     * Maximum response body bytes copied for comparison. A larger response is
     * reported as truncated instead of compared partially.
     */
    private Integer maxResponseBodyBytes = DEFAULT_MAX_BODY_BYTES;

    /**
     * Response headers compared between the primary and shadow responses.
     */
    private List<String> compareHeaders = Arrays.asList("Content-Type");

    /**
     * JSON paths ignored during body comparison, for example {@code $.timestamp}.
     * A difference that disappears after ignoring the paths is reported as
     * normalized rather than as a mismatch.
     */
    private List<String> ignoreJsonPaths = Collections.emptyList();

    /**
     * Request headers forwarded from the primary request. Cookies and
     * authorization headers are forwarded only when named here.
     */
    private List<String> forwardHeaders = Arrays.asList("Accept", "Content-Type");

    /**
     * Static headers added to every shadow request, for example shadow-only
     * credentials. Values are never written to logs.
     */
    private Map<String, String> extraHeaders;

    /**
     * Gets enabled.
     *
     * @return whether this rule mirrors selected requests
     */
    public Boolean getEnabled() {
        return enabled;
    }

    /**
     * Sets enabled.
     *
     * @param enabled whether this rule mirrors selected requests
     */
    public void setEnabled(final Boolean enabled) {
        this.enabled = enabled;
    }

    /**
     * Gets target.
     *
     * @return the absolute shadow destination
     */
    public String getTarget() {
        return target;
    }

    /**
     * Sets target.
     *
     * @param target the absolute shadow destination
     */
    public void setTarget(final String target) {
        this.target = target;
    }

    /**
     * Gets methods.
     *
     * @return the eligible HTTP methods
     */
    public List<String> getMethods() {
        return methods;
    }

    /**
     * Sets methods.
     *
     * @param methods the eligible HTTP methods
     */
    public void setMethods(final List<String> methods) {
        this.methods = methods;
    }

    /**
     * Gets sample rate.
     *
     * @return the fraction of eligible requests that are mirrored
     */
    public Double getSampleRate() {
        return sampleRate;
    }

    /**
     * Sets sample rate.
     *
     * @param sampleRate the fraction of eligible requests that are mirrored
     */
    public void setSampleRate(final Double sampleRate) {
        this.sampleRate = sampleRate;
    }

    /**
     * Gets max concurrent.
     *
     * @return the maximum shadow calls in flight
     */
    public Integer getMaxConcurrent() {
        return maxConcurrent;
    }

    /**
     * Sets max concurrent.
     *
     * @param maxConcurrent the maximum shadow calls in flight
     */
    public void setMaxConcurrent(final Integer maxConcurrent) {
        this.maxConcurrent = maxConcurrent;
    }

    /**
     * Gets queue capacity.
     *
     * @return the maximum queued shadow calls
     */
    public Integer getQueueCapacity() {
        return queueCapacity;
    }

    /**
     * Sets queue capacity.
     *
     * @param queueCapacity the maximum queued shadow calls
     */
    public void setQueueCapacity(final Integer queueCapacity) {
        this.queueCapacity = queueCapacity;
    }

    /**
     * Gets timeout millis.
     *
     * @return the budget for one shadow attempt
     */
    public Integer getTimeoutMillis() {
        return timeoutMillis;
    }

    /**
     * Sets timeout millis.
     *
     * @param timeoutMillis the budget for one shadow attempt
     */
    public void setTimeoutMillis(final Integer timeoutMillis) {
        this.timeoutMillis = timeoutMillis;
    }

    /**
     * Gets max request body bytes.
     *
     * @return the maximum request body bytes copied for a shadow call
     */
    public Integer getMaxRequestBodyBytes() {
        return maxRequestBodyBytes;
    }

    /**
     * Sets max request body bytes.
     *
     * @param maxRequestBodyBytes the maximum request body bytes copied for a shadow call
     */
    public void setMaxRequestBodyBytes(final Integer maxRequestBodyBytes) {
        this.maxRequestBodyBytes = maxRequestBodyBytes;
    }

    /**
     * Gets max response body bytes.
     *
     * @return the maximum response body bytes copied for comparison
     */
    public Integer getMaxResponseBodyBytes() {
        return maxResponseBodyBytes;
    }

    /**
     * Sets max response body bytes.
     *
     * @param maxResponseBodyBytes the maximum response body bytes copied for comparison
     */
    public void setMaxResponseBodyBytes(final Integer maxResponseBodyBytes) {
        this.maxResponseBodyBytes = maxResponseBodyBytes;
    }

    /**
     * Gets compare headers.
     *
     * @return the response headers compared between the two responses
     */
    public List<String> getCompareHeaders() {
        return compareHeaders;
    }

    /**
     * Sets compare headers.
     *
     * @param compareHeaders the response headers compared between the two responses
     */
    public void setCompareHeaders(final List<String> compareHeaders) {
        this.compareHeaders = compareHeaders;
    }

    /**
     * Gets ignore json paths.
     *
     * @return the JSON paths ignored during body comparison
     */
    public List<String> getIgnoreJsonPaths() {
        return ignoreJsonPaths;
    }

    /**
     * Sets ignore json paths.
     *
     * @param ignoreJsonPaths the JSON paths ignored during body comparison
     */
    public void setIgnoreJsonPaths(final List<String> ignoreJsonPaths) {
        this.ignoreJsonPaths = ignoreJsonPaths;
    }

    /**
     * Gets forward headers.
     *
     * @return the request headers forwarded from the primary request
     */
    public List<String> getForwardHeaders() {
        return forwardHeaders;
    }

    /**
     * Sets forward headers.
     *
     * @param forwardHeaders the request headers forwarded from the primary request
     */
    public void setForwardHeaders(final List<String> forwardHeaders) {
        this.forwardHeaders = forwardHeaders;
    }

    /**
     * Gets extra headers.
     *
     * @return the static headers added to every shadow request
     */
    public Map<String, String> getExtraHeaders() {
        return extraHeaders;
    }

    /**
     * Sets extra headers.
     *
     * @param extraHeaders the static headers added to every shadow request
     */
    public void setExtraHeaders(final Map<String, String> extraHeaders) {
        this.extraHeaders = extraHeaders;
    }

    @Override
    public String toString() {
        return "ShadowHandle{" + "enabled=" + enabled + ", target='" + target + '\''
                + ", methods=" + methods + ", sampleRate=" + sampleRate
                + ", maxConcurrent=" + maxConcurrent + ", queueCapacity=" + queueCapacity
                + ", timeoutMillis=" + timeoutMillis + ", maxRequestBodyBytes=" + maxRequestBodyBytes
                + ", maxResponseBodyBytes=" + maxResponseBodyBytes + ", compareHeaders=" + compareHeaders
                + ", ignoreJsonPaths=" + ignoreJsonPaths
                + ", forwardHeaders=" + forwardHeaders + '}';
    }
}
