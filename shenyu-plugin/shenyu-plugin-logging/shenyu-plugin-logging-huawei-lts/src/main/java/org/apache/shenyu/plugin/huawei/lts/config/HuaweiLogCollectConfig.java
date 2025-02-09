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

package org.apache.shenyu.plugin.huawei.lts.config;

import org.apache.shenyu.plugin.logging.common.config.GenericGlobalConfig;

import java.util.Objects;
import java.util.Optional;

/**
 * LogCollectConfig.
 */
public class HuaweiLogCollectConfig {
    public static final HuaweiLogCollectConfig INSTANCE = new HuaweiLogCollectConfig();

    private HuaweiLtsLogConfig huaweiLtsLogConfig;

    /**
     * get Huawei lts log config.
     *
     * @return global log config
     */
    public HuaweiLtsLogConfig getHuaweiLogCollectConfig() {
        return Optional.ofNullable(huaweiLtsLogConfig).orElse(new HuaweiLtsLogConfig());
    }

    /**
     * set Huawei lts log config.
     *
     * @param huaweiLtsLogConfig Tencent cls log config.
     */
    public void setHuaweiLtsLogConfig(final HuaweiLtsLogConfig huaweiLtsLogConfig) {
        this.huaweiLtsLogConfig = huaweiLtsLogConfig;
    }

    /**
     * global log config.
     * please see https://support.huaweicloud.com/usermanual-lts/lts_03_1003.html
     */
    public static class HuaweiLtsLogConfig extends GenericGlobalConfig {

        /**
         * huawei projectId.
         */
        private String projectId;

        /**
         * huawei logGroupId.
         */
        private String logGroupId;

        /**
         * huawei logStreamId.
         */
        private String logStreamId;

        /**
         * huawei accessKeyId.
         */
        private String accessKeyId;

        /**
         * huawei accessKeySecret.
         */
        private String accessKeySecret;

        /**
         * huawei regionName.
         */
        private String regionName;

        /**
         * huawei totalSizeInBytes.
         */
        private Integer totalSizeInBytes = 104857600;

        /**
         * huawei maxBlockMs.
         */
        private Long maxBlockMs = 0L;

        /**
         * huawei ioThreadCount.
         */
        private Integer ioThreadCount = 1;

        /**
         * huawei batchSizeThresholdInBytes.
         */
        private Integer batchSizeThresholdInBytes = 524288;

        /**
         * huawei batchCountThreshold.
         */
        private Integer batchCountThreshold = 4096;

        /**
         * huawei lingerMs.
         */
        private Integer lingerMs = 2000;

        /**
         * huawei retries.
         */
        private Integer retries = 1;

        /**
         * huawei baseRetryBackoffMs.
         */
        private Long baseRetryBackoffMs = 100L;

        /**
         * huawei maxRetryBackoffMs.
         */
        private Long maxRetryBackoffMs = 100L;

        /**
         * huawei enableLocalTest.
         */
        private String enableLocalTest = "true";

        /**
         * huawei setGiveUpExtraLongSingleLog.
         */
        private String setGiveUpExtraLongSingleLog = "true";

        /**
         * getProjectId.
         *
         * @return projectId
         */
        public String getProjectId() {
            return projectId;
        }

        /**
         * set projectId.
         *
         * @param projectId projectId
         */
        public void setProjectId(final String projectId) {
            this.projectId = projectId;
        }

        /**
         * getLogGroupId.
         *
         * @return logGroupId
         */
        public String getLogGroupId() {
            return logGroupId;
        }

        /**
         * set logGroupId.
         *
         * @param logGroupId logGroupId
         */
        public void setLogGroupId(final String logGroupId) {
            this.logGroupId = logGroupId;
        }

        /**
         * getLogStreamId.
         *
         * @return logStreamId
         */
        public String getLogStreamId() {
            return logStreamId;
        }

        /**
         * set logStreamId.
         *
         * @param logStreamId logStreamId
         */
        public void setLogStreamId(final String logStreamId) {
            this.logStreamId = logStreamId;
        }

        /**
         * getAccessKeyId.
         *
         * @return accessKeyId
         */
        public String getAccessKeyId() {
            return accessKeyId;
        }

        /**
         * set accessKeyId.
         *
         * @param accessKeyId accessKeyId
         */
        public void setAccessKeyId(final String accessKeyId) {
            this.accessKeyId = accessKeyId;
        }

        /**
         * getAccessKeySecret.
         *
         * @return accessKeySecret
         */
        public String getAccessKeySecret() {
            return accessKeySecret;
        }

        /**
         * set accessKeySecret.
         *
         * @param accessKeySecret accessKeySecret
         */
        public void setAccessKeySecret(final String accessKeySecret) {
            this.accessKeySecret = accessKeySecret;
        }

        /**
         * getRegionName.
         *
         * @return regionName
         */
        public String getRegionName() {
            return regionName;
        }

        /**
         * set regionName.
         *
         * @param regionName regionName
         */
        public void setRegionName(final String regionName) {
            this.regionName = regionName;
        }

        /**
         * getTotalSizeInBytes.
         *
         * @return totalSizeInBytes
         */
        public Integer getTotalSizeInBytes() {
            return totalSizeInBytes;
        }

        /**
         * set totalSizeInBytes.
         *
         * @param totalSizeInBytes totalSizeInBytes
         */
        public void setTotalSizeInBytes(final Integer totalSizeInBytes) {
            this.totalSizeInBytes = totalSizeInBytes;
        }

        /**
         * getMaxBlockMs.
         *
         * @return maxBlockMs
         */
        public Long getMaxBlockMs() {
            return maxBlockMs;
        }

        /**
         * set maxBlockMs.
         *
         * @param maxBlockMs maxBlockMs
         */
        public void setMaxBlockMs(final Long maxBlockMs) {
            this.maxBlockMs = maxBlockMs;
        }

        /**
         * getIoThreadCount.
         *
         * @return ioThreadCount
         */
        public Integer getIoThreadCount() {
            return ioThreadCount;
        }

        /**
         * set ioThreadCount.
         *
         * @param ioThreadCount ioThreadCount
         */
        public void setIoThreadCount(final Integer ioThreadCount) {
            this.ioThreadCount = ioThreadCount;
        }

        /**
         * getBatchSizeThresholdInBytes.
         *
         * @return batchSizeThresholdInBytes
         */
        public Integer getBatchSizeThresholdInBytes() {
            return batchSizeThresholdInBytes;
        }

        /**
         * set batchSizeThresholdInBytes.
         *
         * @param batchSizeThresholdInBytes batchSizeThresholdInBytes
         */
        public void setBatchSizeThresholdInBytes(final Integer batchSizeThresholdInBytes) {
            this.batchSizeThresholdInBytes = batchSizeThresholdInBytes;
        }

        /**
         * getBatchCountThreshold.
         *
         * @return batchCountThreshold
         */
        public Integer getBatchCountThreshold() {
            return batchCountThreshold;
        }

        /**
         * set batchCountThreshold.
         *
         * @param batchCountThreshold batchCountThreshold
         */
        public void setBatchCountThreshold(final Integer batchCountThreshold) {
            this.batchCountThreshold = batchCountThreshold;
        }

        /**
         * getLingerMs.
         *
         * @return lingerMs
         */
        public Integer getLingerMs() {
            return lingerMs;
        }

        /**
         * set lingerMs.
         *
         * @param lingerMs lingerMs
         */
        public void setLingerMs(final Integer lingerMs) {
            this.lingerMs = lingerMs;
        }

        /**
         * getRetries.
         *
         * @return retries
         */
        public Integer getRetries() {
            return retries;
        }

        /**
         * set retries.
         *
         * @param retries retries
         */
        public void setRetries(final Integer retries) {
            this.retries = retries;
        }

        /**
         * getBaseRetryBackoffMs.
         *
         * @return baseRetryBackoffMs
         */
        public Long getBaseRetryBackoffMs() {
            return baseRetryBackoffMs;
        }

        /**
         * set baseRetryBackoffMs.
         *
         * @param baseRetryBackoffMs baseRetryBackoffMs
         */
        public void setBaseRetryBackoffMs(final Long baseRetryBackoffMs) {
            this.baseRetryBackoffMs = baseRetryBackoffMs;
        }

        /**
         * getMaxRetryBackoffMs.
         *
         * @return maxRetryBackoffMs
         */
        public Long getMaxRetryBackoffMs() {
            return maxRetryBackoffMs;
        }

        /**
         * set maxRetryBackoffMs.
         *
         * @param maxRetryBackoffMs maxRetryBackoffMs
         */
        public void setMaxRetryBackoffMs(final Long maxRetryBackoffMs) {
            this.maxRetryBackoffMs = maxRetryBackoffMs;
        }

        /**
         * getEnableLocalTest.
         *
         * @return enableLocalTest
         */
        public String getEnableLocalTest() {
            return enableLocalTest;
        }

        /**
         * set enableLocalTest.
         *
         * @param enableLocalTest enableLocalTest
         */
        public void setEnableLocalTest(final String enableLocalTest) {
            this.enableLocalTest = enableLocalTest;
        }

        /**
         * getSetGiveUpExtraLongSingleLog.
         *
         * @return setGiveUpExtraLongSingleLog
         */
        public String getSetGiveUpExtraLongSingleLog() {
            return setGiveUpExtraLongSingleLog;
        }

        /**
         * set setGiveUpExtraLongSingleLog.
         *
         * @param setGiveUpExtraLongSingleLog setGiveUpExtraLongSingleLog
         */
        public void setSetGiveUpExtraLongSingleLog(final String setGiveUpExtraLongSingleLog) {
            this.setGiveUpExtraLongSingleLog = setGiveUpExtraLongSingleLog;
        }

        @Override
        public boolean equals(final Object o) {
            if (this == o) {
                return Boolean.TRUE;
            }
            if (Objects.isNull(o) || getClass() != o.getClass()) {
                return Boolean.FALSE;
            }
            HuaweiLtsLogConfig that = (HuaweiLtsLogConfig) o;
            return Objects.equals(getProjectId(), that.getProjectId())
                    && Objects.equals(getLogGroupId(), that.getLogGroupId())
                    && Objects.equals(getLogStreamId(), that.getLogStreamId())
                    && Objects.equals(getAccessKeyId(), that.getAccessKeyId())
                    && Objects.equals(getAccessKeySecret(), that.getAccessKeySecret())
                    && Objects.equals(getRegionName(), that.getRegionName())
                    && Objects.equals(getTotalSizeInBytes(), that.getTotalSizeInBytes())
                    && Objects.equals(getMaxBlockMs(), that.getMaxBlockMs())
                    && Objects.equals(getIoThreadCount(), that.getIoThreadCount())
                    && Objects.equals(getBatchSizeThresholdInBytes(), that.getBatchSizeThresholdInBytes())
                    && Objects.equals(getBatchCountThreshold(), that.getBatchCountThreshold())
                    && Objects.equals(getLingerMs(), that.getLingerMs())
                    && Objects.equals(getRetries(), that.getRetries())
                    && Objects.equals(getBaseRetryBackoffMs(), that.getBaseRetryBackoffMs())
                    && Objects.equals(getMaxRetryBackoffMs(), that.getMaxRetryBackoffMs())
                    && Objects.equals(getEnableLocalTest(), that.getEnableLocalTest())
                    && Objects.equals(getSetGiveUpExtraLongSingleLog(), that.getSetGiveUpExtraLongSingleLog());
        }

        @Override
        public int hashCode() {
            return Objects.hash(projectId, logGroupId, logStreamId, accessKeyId, accessKeySecret,
                    regionName, totalSizeInBytes, maxBlockMs, ioThreadCount,
                    batchSizeThresholdInBytes, batchCountThreshold, lingerMs,
                    retries, baseRetryBackoffMs, maxRetryBackoffMs, enableLocalTest,
                    setGiveUpExtraLongSingleLog);
        }
    }
}
