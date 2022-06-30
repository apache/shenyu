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

package org.apache.shenyu.plugin.aliyun.sls.config;

/**
 * LogCollectConfig.
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

        /**
         * aliyun sls address.
         * please seee https://help.aliyun.com/document_detail/29008.htm?spm=a2c4g.11186623.0.0.cc7a3de5dJNKOe#reference-wgx-pwq-zdb
         */
        private String host;

        /**
         * aliyun access id.
         */
        private String accessId;

        /**
         * aliyun accessKey.
         */
        private String accessKey;

        /**
         * aliyun sls project name.
         */
        private String projectName = "shenyu-gateway";

        /**
         * aliyun sls logstore name.
         */
        private String logStoreName = "shenyu-gateway-logstore";

        /**
         * aliyun sls ttl.
         */
        private Integer ttlInDay = 3;

        /**
         * aliyun shard.
         */
        private Integer shardCount = 10;

        /**
         * aliyun sls topic.
         * aliyun query by topic
         */
        private String topic = "shenyu-gateway-topic";

        private String sampleRate = "1";

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
         * get access id.
         *
         * @return accessId
         */
        public String getAccessId() {
            return accessId;
        }

        /**
         * set accessId.
         *
         * @param accessId accessId
         */
        public void setAccessId(final String accessId) {
            this.accessId = accessId;
        }

        /**
         * get accessKey.
         *
         * @return accessKey
         */
        public String getAccessKey() {
            return accessKey;
        }

        /**
         * set accessKey.
         *
         * @param accessKey accessKey
         */
        public void setAccessKey(final String accessKey) {
            this.accessKey = accessKey;
        }

        /**
         * get project name.
         *
         * @return project name
         */
        public String getProjectName() {
            return projectName;
        }

        /**
         * set project.
         *
         * @param projectName project name
         */
        public void setProjectName(final String projectName) {
            this.projectName = projectName;
        }

        /**
         * get logStore name.
         *
         * @return logStore name
         */
        public String getLogStoreName() {
            return logStoreName;
        }

        /**
         * set logStore name.
         *
         * @param logStoreName logStoreName
         */
        public void setLogStoreName(final String logStoreName) {
            this.logStoreName = logStoreName;
        }

        /**
         * get ttl.
         *
         * @return ttl
         */
        public Integer getTtlInDay() {
            return ttlInDay;
        }

        /**
         * set ttl.
         *
         * @param ttlInDay ttl
         */
        public void setTtlInDay(final Integer ttlInDay) {
            this.ttlInDay = ttlInDay;
        }

        /**
         * get aliyun shard.
         *
         * @return shard count
         */
        public Integer getShardCount() {
            return shardCount;
        }

        /**
         * set aliyun shard.
         *
         * @param shardCount shardCount
         */
        public void setShardCount(final Integer shardCount) {
            this.shardCount = shardCount;
        }

        /**
         * get aliyun search topic.
         *
         * @return topic
         */
        public String getTopic() {
            return topic;
        }

        /**
         * set aliyun search toic.
         *
         * @param topic topic
         */
        public void setTopic(final String topic) {
            this.topic = topic;
        }
    }
}
