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

package org.apache.shenyu.plugin.logging.common.config;

import org.apache.shenyu.plugin.logging.common.sampler.Sampler;

import java.util.Collections;
import java.util.Set;

/**
 * generic global config.
 */
public class GenericGlobalConfig {

    /**
     * Default response body limit in bytes (512 KiB).
     */
    public static final int DEFAULT_MAX_RESPONSE_BODY = 524288;

    /**
     * Default request body limit in bytes (512 KiB).
     */
    public static final int DEFAULT_MAX_REQUEST_BODY = 524288;

    private boolean maskCredentialHeaders = true;

    private Set<String> maskedHeaders = Collections.emptySet();

    private String sampleRate = "1";

    private Sampler sampler;

    /**
     * default 512KB.
     */
    private int maxResponseBody = DEFAULT_MAX_RESPONSE_BODY;

    /**
     * default 512kb.
     */
    private int maxRequestBody = DEFAULT_MAX_REQUEST_BODY;

    /**
     * default 50000.
     */
    private int bufferQueueSize = 50000;

    /**
     * Whether credential headers are masked in logs.
     *
     * @return masking enabled
     */
    public boolean isMaskCredentialHeaders() {
        return maskCredentialHeaders;
    }

    /**
     * Enable or disable credential header masking.
     *
     * @param maskCredentialHeaders masking enabled
     */
    public void setMaskCredentialHeaders(final boolean maskCredentialHeaders) {
        this.maskCredentialHeaders = maskCredentialHeaders;
    }

    /**
     * Get additional header names to mask, in addition to the built-in credentials.
     *
     * @return additional header names
     */
    public Set<String> getMaskedHeaders() {
        return maskedHeaders;
    }

    /**
     * Set additional header names to mask.
     *
     * @param maskedHeaders additional header names
     */
    public void setMaskedHeaders(final Set<String> maskedHeaders) {
        this.maskedHeaders = maskedHeaders;
    }

    /**
     * get sampler.
     *
     * @return sampler
     */
    public Sampler getSampler() {
        return sampler;
    }

    /**
     * set sampler.
     *
     * @param sampler sampler
     */
    public void setSampler(final Sampler sampler) {
        this.sampler = sampler;
    }

    /**
     * get sample rate.
     *
     * @return sample
     */
    public String getSampleRate() {
        return sampleRate;
    }

    /**
     * set sample rate.
     *
     * @param sampleRate rate
     */
    public void setSampleRate(final String sampleRate) {
        this.sampleRate = sampleRate;
    }

    /**
     * get max response body.
     *
     * @return get max response body
     */
    public int getMaxResponseBody() {
        return maxResponseBody;
    }

    /**
     * set max response body.
     *
     * @param maxResponseBody max response body
     */
    public void setMaxResponseBody(final int maxResponseBody) {
        this.maxResponseBody = maxResponseBody;
    }

    /**
     * get max request body.
     *
     * @return max request body
     */
    public int getMaxRequestBody() {
        return maxRequestBody;
    }

    /**
     * set max request body.
     *
     * @param maxRequestBody max request body
     */
    public void setMaxRequestBody(final int maxRequestBody) {
        this.maxRequestBody = maxRequestBody;
    }

    /**
     * get buffer queue size.
     *
     * @return buffer queue size
     */
    public int getBufferQueueSize() {
        return bufferQueueSize;
    }

    /**
     * set buffer queue size.
     *
     * @param bufferQueueSize buffer queue size
     */
    public void setBufferQueueSize(final int bufferQueueSize) {
        this.bufferQueueSize = bufferQueueSize;
    }
}
