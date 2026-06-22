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

package org.apache.shenyu.plugin.record.config;

import java.util.Objects;
import java.util.Optional;

/**
 * Configuration holder for the http record plugin.
 *
 * <p>Stores the shared record config that is pushed from the admin server
 * and read by both the gateway plugin and the collector thread.</p>
 */
public class HttpRecordCollectConfig {

    public static final HttpRecordCollectConfig INSTANCE = new HttpRecordCollectConfig();

    private static final int DEFAULT_MAX_BODY_SIZE = 524288;

    private static final int DEFAULT_BATCH_SIZE = 100;

    private static final long DEFAULT_BATCH_INTERVAL_MS = 5000L;

    private volatile RecordConfig recordConfig;

    /**
     * Get record config.
     *
     * @return the record config, defaults to a new instance if not set
     */
    public RecordConfig getRecordConfig() {
        return Optional.ofNullable(recordConfig).orElse(new RecordConfig());
    }

    /**
     * Set record config.
     *
     * @param recordConfig the record config to set
     */
    public void setRecordConfig(final RecordConfig recordConfig) {
        this.recordConfig = recordConfig;
    }

    /**
     * Record config properties.
     */
    public static class RecordConfig {

        private Integer maxBodySize = DEFAULT_MAX_BODY_SIZE;

        private int batchSize = DEFAULT_BATCH_SIZE;

        private long batchIntervalMs = DEFAULT_BATCH_INTERVAL_MS;

        /**
         * Get max body size in bytes.
         *
         * @return the max body size
         */
        public Integer getMaxBodySize() {
            return maxBodySize;
        }

        /**
         * Set max body size in bytes.
         *
         * @param maxBodySize the max body size
         */
        public void setMaxBodySize(final Integer maxBodySize) {
            this.maxBodySize = maxBodySize;
        }

        /**
         * Get batch size for uploading records.
         *
         * @return the batch size
         */
        public int getBatchSize() {
            return batchSize;
        }

        /**
         * Set batch size for uploading records.
         *
         * @param batchSize the batch size
         */
        public void setBatchSize(final int batchSize) {
            this.batchSize = batchSize;
        }

        /**
         * Get batch interval in milliseconds.
         *
         * @return the batch interval in milliseconds
         */
        public long getBatchIntervalMs() {
            return batchIntervalMs;
        }

        /**
         * Set batch interval in milliseconds.
         *
         * @param batchIntervalMs the batch interval in milliseconds
         */
        public void setBatchIntervalMs(final long batchIntervalMs) {
            this.batchIntervalMs = batchIntervalMs;
        }

        @Override
        public boolean equals(final Object o) {
            if (this == o) {
                return true;
            }
            if (Objects.isNull(o) || getClass() != o.getClass()) {
                return false;
            }
            RecordConfig that = (RecordConfig) o;
            return Objects.equals(maxBodySize, that.maxBodySize)
                    && Objects.equals(batchSize, that.batchSize)
                    && Objects.equals(batchIntervalMs, that.batchIntervalMs);
        }

        @Override
        public int hashCode() {
            return Objects.hash(maxBodySize, batchSize, batchIntervalMs);
        }
    }
}
