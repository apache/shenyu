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

package org.apache.shenyu.common.dto.convert.plugin;

import java.io.Serializable;
import java.util.Objects;

/**
 * The type grpc register config.
 */
public class GrpcRegisterConfig implements Serializable {

    private static final long serialVersionUID = -4496044027526152878L;

    private String threadpool;

    /**
     * Maximum number of aggregated response items retained by a single gRPC call
     * (including SERVER_STREAMING / BIDI_STREAMING). A value that is {@code null}
     * or not positive means unlimited, which preserves the historical behavior.
     */
    private Long maxAggregatedResultCount;

    /**
     * Maximum accumulated payload bytes (UTF-8) of the aggregated response items retained
     * by a single gRPC call. This bounds the serialized payload bytes, not the exact JVM heap
     * footprint. A value that is {@code null} or not positive means unlimited, which preserves
     * the historical behavior.
     */
    private Long maxAggregatedResultBytes;

    /**
     * get threadpool.
     *
     * @return threadpool
     */
    public String getThreadpool() {
        return threadpool;
    }

    /**
     * set threadpool.
     *
     * @param threadpool threadpool
     */
    public void setThreadpool(final String threadpool) {
        this.threadpool = threadpool;
    }

    /**
     * get maxAggregatedResultCount.
     *
     * @return maxAggregatedResultCount
     */
    public Long getMaxAggregatedResultCount() {
        return maxAggregatedResultCount;
    }

    /**
     * set maxAggregatedResultCount.
     *
     * @param maxAggregatedResultCount maxAggregatedResultCount
     */
    public void setMaxAggregatedResultCount(final Long maxAggregatedResultCount) {
        this.maxAggregatedResultCount = maxAggregatedResultCount;
    }

    /**
     * get maxAggregatedResultBytes.
     *
     * @return maxAggregatedResultBytes
     */
    public Long getMaxAggregatedResultBytes() {
        return maxAggregatedResultBytes;
    }

    /**
     * set maxAggregatedResultBytes.
     *
     * @param maxAggregatedResultBytes maxAggregatedResultBytes
     */
    public void setMaxAggregatedResultBytes(final Long maxAggregatedResultBytes) {
        this.maxAggregatedResultBytes = maxAggregatedResultBytes;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        GrpcRegisterConfig that = (GrpcRegisterConfig) o;
        return Objects.equals(threadpool, that.threadpool)
                && Objects.equals(maxAggregatedResultCount, that.maxAggregatedResultCount)
                && Objects.equals(maxAggregatedResultBytes, that.maxAggregatedResultBytes);
    }

    @Override
    public int hashCode() {
        return Objects.hash(threadpool, maxAggregatedResultCount, maxAggregatedResultBytes);
    }

    @Override
    public String toString() {
        return "GrpcRegisterConfig{"
                + "threadpool='"
                + threadpool
                + '\''
                + ", maxAggregatedResultCount="
                + maxAggregatedResultCount
                + ", maxAggregatedResultBytes="
                + maxAggregatedResultBytes
                + '}';
    }
}
