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

package org.apache.shenyu.plugin.logging.elasticsearch.config;

/**
 * log collect config, include elasticsearch config.
 * Host and port must be included, and others are optional.
 * We should operate the configuration through admin instead of the configuration file.
 */
public class LogCollectConfig {

    private GlobalLogConfig globalLogConfig;

    /**
     * get global log config.
     *
     * @return global log config
     */
    public GlobalLogConfig getGlobalLogConfig() {
        return globalLogConfig;
    }

    /**
     * set global log config.
     *
     * @param globalLogConfig global log config.
     */
    public void setGlobalLogConfig(final GlobalLogConfig globalLogConfig) {
        this.globalLogConfig = globalLogConfig;
    }

    /**
     * global log config.
     */
    public static class GlobalLogConfig {

        private String host;

        private String port;

        private String sampleRate = "1";

        private String compressAlg;

        /**
         * default 512KB.
         */
        private int maxResponseBody = 524288;

        /**
         * default 512kb.
         */
        private int maxRequestBody = 524288;

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
         * whether compress.
         *
         * @return compress or not
         */
        public String getCompressAlg() {
            return compressAlg;
        }

        /**
         * set compress.
         *
         * @param compressAlg compress alg.
         */
        public void setCompressAlg(final String compressAlg) {
            this.compressAlg = compressAlg;
        }

        /**
         * get max response org.apache.shenyu.plugin.logging.body.
         *
         * @return get max response org.apache.shenyu.plugin.logging.body
         */
        public int getMaxResponseBody() {
            return maxResponseBody;
        }

        /**
         * set max response org.apache.shenyu.plugin.logging.body.
         *
         * @param maxResponseBody max response org.apache.shenyu.plugin.logging.body
         */
        public void setMaxResponseBody(final int maxResponseBody) {
            this.maxResponseBody = maxResponseBody;
        }

        /**
         * get max request org.apache.shenyu.plugin.logging.body.
         *
         * @return max request org.apache.shenyu.plugin.logging.body
         */
        public int getMaxRequestBody() {
            return maxRequestBody;
        }

        /**
         * set max request org.apache.shenyu.plugin.logging.body.
         *
         * @param maxRequestBody max request org.apache.shenyu.plugin.logging.body
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

        /**
         * get host.
         *
         * @return host
         */
        public String getHost() {
            return host;
        }

        /**
         * set host.
         *
         * @param host host name
         */
        public void setHost(final String host) {
            this.host = host;
        }

        /**
         * get port.
         *
         * @return port
         */
        public String getPort() {
            return port;
        }

        /**
         * set port.
         *
         * @param port port name
         */
        public void setPort(final String port) {
            this.port = port;
        }

    }

    /**
     * api log config.
     */
    public static class LogApiConfig {

        private String index;

        /**
         * 0 means never sample, 1 means always sample. Minimum probability is 0.01, or 1% of logging
         */
        private String sampleRate;

        /**
         * get sample rate.
         *
         * @return sample rate
         */
        public String getSampleRate() {
            return sampleRate;
        }

        /**
         * set sample rate.
         *
         * @param sampleRate sample rate
         */
        public void setSampleRate(final String sampleRate) {
            this.sampleRate = sampleRate;
        }

        /**
         * get index name.
         *
         * @return index name
         */
        public String getIndex() {
            return index;
        }

        /**
         * set index name.
         *
         * @param index index name
         */
        public void setIndex(final String index) {
            this.index = index;
        }

    }

}
