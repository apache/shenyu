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

import java.util.Objects;
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
     * @param tencentClsLogConfig Tencent cls log config.
     */
    public void setTencentClsLogConfig(final TencentClsLogConfig tencentClsLogConfig) {
        this.tencentClsLogConfig = tencentClsLogConfig;
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
        private String totalSizeInBytes = "104857600";

        /**
         * tencent maxSendThreadCount.
         */
        private String maxSendThreadCount = String.valueOf(Math.max(Runtime.getRuntime().availableProcessors(), 1));

        /**
         * tencent maxBlockSec.
         */
        private String maxBlockSec = "60000";

        /**
         * tencent maxBatchSize.
         */
        private String maxBatchSize = 512 * 1024 + "";

        /**
         * tencent maxBatchCount.
         */
        private String maxBatchCount = "4096";

        /**
         * tencent lingerMs.
         */
        private String lingerMs = "2000";

        /**
         * tencent retries.
         */
        private String retries = "10";

        /**
         * tencent maxReservedAttempts.
         */
        private String maxReservedAttempts = "11";

        /**
         * tencent baseRetryBackoffMs.
         */
        private String baseRetryBackoffMs = "100";

        /**
         * tencent maxRetryBackoffMs.
         */
        private String maxRetryBackoffMs = "50000";

        /**
         * tencent sendThreadCount.
         */
        private Integer sendThreadCount = 1;

        /**
         * Tencent cls topic.
         * tencent query by topic
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
        public void setEndpoint(final String endpoint) {
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
        public void setSecretId(final String secretId) {
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
        public void setSecretKey(final String secretKey) {
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
        public void setTotalSizeInBytes(final String totalSizeInBytes) {
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
        public void setMaxSendThreadCount(final String maxSendThreadCount) {
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
        public void setMaxBlockSec(final String maxBlockSec) {
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
        public void setMaxBatchSize(final String maxBatchSize) {
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
        public void setMaxBatchCount(final String maxBatchCount) {
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
        public void setLingerMs(final String lingerMs) {
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
        public void setRetries(final String retries) {
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
        public void setMaxReservedAttempts(final String maxReservedAttempts) {
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
        public void setBaseRetryBackoffMs(final String baseRetryBackoffMs) {
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
        public void setMaxRetryBackoffMs(final String maxRetryBackoffMs) {
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
        public void setTopic(final String topic) {
            this.topic = topic;
        }

        @Override
        public boolean equals(final Object o) {
            if (this == o) {
                return Boolean.TRUE;
            }

            if (o == null || getClass() != o.getClass()) {
                return Boolean.FALSE;
            }

            TencentClsLogConfig that = (TencentClsLogConfig) o;
            return Objects.equals(getSecretId(), that.getSecretId())
                    && Objects.equals(getSecretKey(), that.getSecretKey())
                    && Objects.equals(getEndpoint(), that.getEndpoint())
                    && Objects.equals(getTopic(), that.getTopic())
                    && Objects.equals(getSendThreadCount(), that.getSendThreadCount())
                    && Objects.equals(getTotalSizeInBytes(), that.getTotalSizeInBytes())
                    && Objects.equals(getMaxSendThreadCount(), that.getMaxSendThreadCount())
                    && Objects.equals(getMaxBlockSec(), that.getMaxBlockSec())
                    && Objects.equals(getMaxBatchSize(), that.getMaxBatchSize())
                    && Objects.equals(getMaxBatchCount(), that.getMaxBatchCount())
                    && Objects.equals(getLingerMs(), that.getLingerMs())
                    && Objects.equals(getRetries(), that.getRetries())
                    && Objects.equals(getMaxReservedAttempts(), that.getMaxReservedAttempts())
                    && Objects.equals(getBaseRetryBackoffMs(), that.getBaseRetryBackoffMs())
                    && Objects.equals(getMaxRetryBackoffMs(), that.getMaxRetryBackoffMs());
        }

        @Override
        public int hashCode() {
            return Objects.hash(secretId, secretKey, endpoint, topic, sendThreadCount, totalSizeInBytes,
                    maxSendThreadCount, maxBlockSec, maxBatchSize, maxBatchCount, lingerMs, retries,
                    maxReservedAttempts, baseRetryBackoffMs, maxRetryBackoffMs);
        }
    }
}
