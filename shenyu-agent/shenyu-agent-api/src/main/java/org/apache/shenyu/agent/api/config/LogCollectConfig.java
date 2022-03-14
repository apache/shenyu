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

package org.apache.shenyu.agent.api.config;

import java.util.Map;

/**
 * log collect config.
 */
public class LogCollectConfig {

    private LogFieldSwitchConfig logFieldSwitchConfig;

    private GlobalLogConfig globalLogConfig;

    private Map<String, LogApiConfig> logApiSwitchConfigMap;

    /**
     * get LogFieldSwitchConfig.
     *
     * @return LogFieldSwitchConfig
     */
    public LogFieldSwitchConfig getLogFieldSwitchConfig() {
        return logFieldSwitchConfig;
    }

    /**
     * set LogFieldSwitchConfig.
     *
     * @param logFieldSwitchConfig LogFieldSwitchConfig
     */
    public void setLogFieldSwitchConfig(final LogFieldSwitchConfig logFieldSwitchConfig) {
        this.logFieldSwitchConfig = logFieldSwitchConfig;
    }

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
     * get log api switch config map.
     *
     * @return log api switch config map
     */
    public Map<String, LogApiConfig> getLogApiSwitchConfigMap() {
        return logApiSwitchConfigMap;
    }

    /**
     * set log api switch config.
     *
     * @param logApiSwitchConfigMap log api switch config
     */
    public void setLogApiSwitchConfigMap(final Map<String, LogApiConfig> logApiSwitchConfigMap) {
        this.logApiSwitchConfigMap = logApiSwitchConfigMap;
    }

    /**
     * global log config.
     */
    public static class GlobalLogConfig {
        private String topic;

        private String sampleRate = "1";

        private boolean compress;

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
        public boolean isCompress() {
            return compress;
        }

        /**
         * set compress.
         *
         * @param compress true: compress, false not compress
         */
        public void setCompress(final boolean compress) {
            this.compress = compress;
        }

        /**
         * get message queue topic.
         *
         * @return message queue topic
         */
        public String getTopic() {
            return topic;
        }

        /**
         * topic,used for message queue.
         *
         * @param topic mq topic
         */
        public void setTopic(final String topic) {
            this.topic = topic;
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

    /**
     * api log config.
     */
    public static class LogApiConfig {

        /**
         * 0 means never sample, 1 means always sample. Minimum probability is 0.01, or 1% of logging
         */
        private String sampleRate;

        /**
         * This topic is useful if you use message queuing to collect logs.
         */
        private String topic;

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
         * get mq topic.
         *
         * @return mq topic
         */
        public String getTopic() {
            return topic;
        }

        /**
         * set  mq topic.
         *
         * @param topic mq topic
         */
        public void setTopic(final String topic) {
            this.topic = topic;
        }
    }


    /**
     * log field switch config, default value is true.
     */
    public static class LogFieldSwitchConfig {

        private boolean requestHeader = true;

        private boolean responseHeader = true;

        private boolean requestBody = true;

        private boolean responseBody = true;

        /**
         * whether to collect request header.
         *
         * @return true: collect , false:not collect
         */
        public boolean isRequestHeader() {
            return requestHeader;
        }


        /**
         * set  whether to collect request header.
         *
         * @param requestHeader whether collect
         */
        public void setRequestHeader(final boolean requestHeader) {
            this.requestHeader = requestHeader;
        }

        /**
         * whether collect response header.
         *
         * @return collect or not
         */
        public boolean isResponseHeader() {
            return responseHeader;
        }

        /**
         * whether collect response header.
         *
         * @param responseHeader boolean
         */
        public void setResponseHeader(final boolean responseHeader) {
            this.responseHeader = responseHeader;
        }


        /**
         * whether collect request body.
         *
         * @return collect or not
         */
        public boolean isRequestBody() {
            return requestBody;
        }

        /**
         * whether collect request body.
         *
         * @param requestBody boolean
         */
        public void setRequestBody(final boolean requestBody) {
            this.requestBody = requestBody;
        }

        /**
         * whether collect response body.
         *
         * @return collect or not
         */
        public boolean isResponseBody() {
            return responseBody;
        }

        /**
         * whether collect response body.
         *
         * @param responseBody boolean
         */
        public void setResponseBody(final boolean responseBody) {
            this.responseBody = responseBody;
        }

    }
}
