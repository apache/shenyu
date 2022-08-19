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

package org.apache.shenyu.plugin.tencent.cls.config;

import org.apache.shenyu.plugin.logging.common.config.GenericGlobalConfig;

import java.util.Optional;

/**
 * LogCollectConfig.
 */
public class TencentLogCollectConfig {

    public static final TencentLogCollectConfig INSTANCE = new TencentLogCollectConfig();

    private TencentClsLogConfig tencentClsLogConfig;

    /**
     * get Tencent cls log config.
     *
     * @return global log config
     */
    public TencentClsLogConfig getTencentClsLogConfig() {
        return Optional.ofNullable(tencentClsLogConfig).orElse(new TencentClsLogConfig());
    }

    /**
     * set Tencent cls log config.
     *
     * @param aliyunSlsLogConfig Tencent cls log config.
     */
    public void setTencentClsLogConfig(final TencentClsLogConfig aliyunSlsLogConfig) {
        this.tencentClsLogConfig = aliyunSlsLogConfig;
    }

    /**
     * global log config.
     * please seee https://github.com/TencentCloud/tencentcloud-cls-sdk-java/blob/main/README.md#%E9%85%8D%E7%BD%AE%E5%8F%82%E6%95%B0%E8%AF%A6%E8%A7%A3
     */
    public static class TencentClsLogConfig extends GenericGlobalConfig {

        /**
         * Tencent cls endpoint.
         * please seee https://cloud.tencent.com/document/product/614/18940
         */
        private String endpoint;

        /**
         * tencent secretId.
         */
        private String secretId;

        /**
         * tencent secretKey.
         */
        private String secretKey;

        /**
         * tencent totalSizeInBytes.
         */
        private String totalSizeInBytes;

        /**
         * tencent maxSendThreadCount.
         */
        private String maxSendThreadCount;

        /**
         * tencent maxBlockSec.
         */
        private String maxBlockSec;

        /**
         * tencent maxBatchSize.
         */
        private String maxBatchSize;

        /**
         * tencent maxBatchCount.
         */
        private String maxBatchCount;

        /**
         * tencent lingerMs.
         */
        private String lingerMs;

        /**
         * tencent retries.
         */
        private String retries;

        /**
         * tencent maxReservedAttempts.
         */
        private String maxReservedAttempts;

        /**
         * tencent baseRetryBackoffMs.
         */
        private String baseRetryBackoffMs;

        /**
         * tencent maxRetryBackoffMs.
         */
        private String maxRetryBackoffMs;

        /**
         * tencent sendThreadCount.
         */
        private Integer sendThreadCount = 1;

        /**
         * Tencent cls topic.
         * aliyun query by topic
         */
        private String topic = "shenyu-topic";
        
        /**
         * getEndpoint.
         * 
         * @return endpoint
         */
        public String getEndpoint() {
            return endpoint;
        }

        /**
         * set accessId.
         *
         * @param endpoint endpoint
         */
        public void setEndpoint(String endpoint) {
            this.endpoint = endpoint;
        }
        
        /**
         * getSecretId.
         * 
         * @return secretId
         */
        public String getSecretId() {
            return secretId;
        }

        /**
         * set secretId.
         *
         * @param secretId secretId
         */
        public void setSecretId(String secretId) {
            this.secretId = secretId;
        }

        /**
         * getSecretKey.
         *
         * @return secretKey
         */
        public String getSecretKey() {
            return secretKey;
        }

        /**
         * set secretKey.
         *
         * @param secretKey secretKey
         */
        public void setSecretKey(String secretKey) {
            this.secretKey = secretKey;
        }

        /**
         * getTotalSizeInBytes.
         *
         * @return totalSizeInBytes
         */
        public String getTotalSizeInBytes() {
            return totalSizeInBytes;
        }

        /**
         * set totalSizeInBytes.
         *
         * @param totalSizeInBytes totalSizeInBytes
         */
        public void setTotalSizeInBytes(String totalSizeInBytes) {
            this.totalSizeInBytes = totalSizeInBytes;
        }

        /**
         * getMaxSendThreadCount.
         *
         * @return maxSendThreadCount
         */
        public String getMaxSendThreadCount() {
            return maxSendThreadCount;
        }

        /**
         * set maxSendThreadCount.
         *
         * @param maxSendThreadCount maxSendThreadCount
         */
        public void setMaxSendThreadCount(String maxSendThreadCount) {
            this.maxSendThreadCount = maxSendThreadCount;
        }

        /**
         * getMaxBlockSec.
         *
         * @return maxBlockSec
         */
        public String getMaxBlockSec() {
            return maxBlockSec;
        }

        /**
         * set maxBlockSec.
         *
         * @param maxBlockSec maxBlockSec
         */
        public void setMaxBlockSec(String maxBlockSec) {
            this.maxBlockSec = maxBlockSec;
        }

        /**
         * getMaxBatchSize.
         *
         * @return maxBatchSize
         */
        public String getMaxBatchSize() {
            return maxBatchSize;
        }

        /**
         * set maxBatchSize.
         *
         * @param maxBatchSize maxBatchSize
         */
        public void setMaxBatchSize(String maxBatchSize) {
            this.maxBatchSize = maxBatchSize;
        }

        /**
         * getMaxBatchCount.
         *
         * @return maxBatchCount
         */
        public String getMaxBatchCount() {
            return maxBatchCount;
        }

        /**
         * set maxBatchCount.
         *
         * @param maxBatchCount maxBatchCount
         */
        public void setMaxBatchCount(String maxBatchCount) {
            this.maxBatchCount = maxBatchCount;
        }

        /**
         * getLingerMs.
         *
         * @return lingerMs
         */
        public String getLingerMs() {
            return lingerMs;
        }

        /**
         * set lingerMs.
         *
         * @param lingerMs lingerMs
         */
        public void setLingerMs(String lingerMs) {
            this.lingerMs = lingerMs;
        }

        /**
         * getRetries.
         *
         * @return retries
         */
        public String getRetries() {
            return retries;
        }

        /**
         * set retries.
         *
         * @param retries retries
         */
        public void setRetries(String retries) {
            this.retries = retries;
        }

        /**
         * getMaxReservedAttempts.
         *
         * @return maxReservedAttempts
         */
        public String getMaxReservedAttempts() {
            return maxReservedAttempts;
        }

        /**
         * set maxReservedAttempts.
         *
         * @param maxReservedAttempts maxReservedAttempts
         */
        public void setMaxReservedAttempts(String maxReservedAttempts) {
            this.maxReservedAttempts = maxReservedAttempts;
        }

        /**
         * getBaseRetryBackoffMs.
         *
         * @return baseRetryBackoffMs
         */
        public String getBaseRetryBackoffMs() {
            return baseRetryBackoffMs;
        }

        /**
         * set baseRetryBackoffMs.
         *
         * @param baseRetryBackoffMs baseRetryBackoffMs
         */
        public void setBaseRetryBackoffMs(String baseRetryBackoffMs) {
            this.baseRetryBackoffMs = baseRetryBackoffMs;
        }

        /**
         * getMaxRetryBackoffMs.
         *
         * @return maxRetryBackoffMs
         */
        public String getMaxRetryBackoffMs() {
            return maxRetryBackoffMs;
        }

        /**
         * set maxRetryBackoffMs.
         *
         * @param maxRetryBackoffMs maxRetryBackoffMs
         */
        public void setMaxRetryBackoffMs(String maxRetryBackoffMs) {
            this.maxRetryBackoffMs = maxRetryBackoffMs;
        }

        /**
         * get send thread count.
         *
         * @return send thread count
         */
        public Integer getSendThreadCount() {
            return sendThreadCount;
        }

        /**
         * send thread count.
         *
         * @param sendThreadCount send thread count
         */
        public void setSendThreadCount(final Integer sendThreadCount) {
            this.sendThreadCount = sendThreadCount;
        }

        /**
         * getTopic.
         *
         * @return topic
         */
        public String getTopic() {
            return topic;
        }

        /**
         * set topic.
         *
         * @param topic topic
         */
        public void setTopic(String topic) {
            this.topic = topic;
        }
    }
}
