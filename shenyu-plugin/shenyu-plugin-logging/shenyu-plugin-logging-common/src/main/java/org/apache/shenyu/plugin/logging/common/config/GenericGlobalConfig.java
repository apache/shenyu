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

/**
 * generic global config.
 */
public class GenericGlobalConfig {

    private String sampleRate = "1";

    /**
     * default 512KB.
     */
    private int maxResponseBody = 524288;

    /**
     * default 512kb.
     */
    private int maxRequestBody = 524288;

    /**
     * default 50000.
     */
    private int bufferQueueSize = 50000;

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
