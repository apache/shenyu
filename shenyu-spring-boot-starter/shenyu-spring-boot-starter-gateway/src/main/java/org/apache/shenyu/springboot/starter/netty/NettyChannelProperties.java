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

package org.apache.shenyu.springboot.starter.netty;

import io.netty.buffer.ByteBufAllocator;
import io.netty.buffer.PooledByteBufAllocator;
import io.netty.buffer.UnpooledByteBufAllocator;
import io.netty.channel.DefaultMessageSizeEstimator;
import io.netty.channel.MessageSizeEstimator;

import java.util.Objects;

/**
 * Netty channel properties.
 */
public class NettyChannelProperties {

    private static final String UN_POOLED = "unpooled";

    private static final String POOLED = "pooled";

    private Integer connectTimeoutMillis = 10000;

    private Integer writeBufferHighWaterMark = 65536;

    private Integer writeBufferLowWaterMark = 32768;

    private Integer writeSpinCount = 16;

    private Boolean autoRead = false;

    /**
     * options: unpooled or pooled.
     */
    private String allocType = POOLED;

    private Boolean soReuseAddr = false;

    private Integer soRcvBuf = 87380;

    /**
     * message estimator.
     */
    private Integer messageSizeEstimator = 8;

    /**
     * single event executor.
     */
    private Boolean singleEventExecutorPerGroup = true;

    /**
     * get connectTimeoutMillis.
     *
     * @return connectTimeoutMillis
     */
    public Integer getConnectTimeoutMillis() {
        return connectTimeoutMillis;
    }

    /**
     * set connectTimeoutMillis.
     *
     * @param connectTimeoutMillis the connection timeout millis
     */
    public void setConnectTimeoutMillis(final Integer connectTimeoutMillis) {
        this.connectTimeoutMillis = connectTimeoutMillis;
    }

    /**
     * get writeBufferHighWaterMark.
     *
     * @return writeBufferHighWaterMark
     */
    public Integer getWriteBufferHighWaterMark() {
        return writeBufferHighWaterMark;
    }

    /**
     * set writeBufferHighWaterMark.
     *
     * @param writeBufferHighWaterMark write buffer high water mark
     */
    public void setWriteBufferHighWaterMark(final Integer writeBufferHighWaterMark) {
        this.writeBufferHighWaterMark = writeBufferHighWaterMark;
    }

    /**
     * get writeBufferLowWaterMark.
     *
     * @return writeBufferLowWaterMark
     */
    public Integer getWriteBufferLowWaterMark() {
        return writeBufferLowWaterMark;
    }

    /**
     * set writeBufferLowWaterMark.
     *
     * @param writeBufferLowWaterMark write buffer low water mark
     */
    public void setWriteBufferLowWaterMark(final Integer writeBufferLowWaterMark) {
        this.writeBufferLowWaterMark = writeBufferLowWaterMark;
    }

    /**
     * get writeSpinCount.
     *
     * @return writeSpinCount
     */
    public Integer getWriteSpinCount() {
        return writeSpinCount;
    }

    /**
     * set writeSpinCount.
     *
     * @param writeSpinCount WRITE_SPIN_COUNT
     */
    public void setWriteSpinCount(final Integer writeSpinCount) {
        this.writeSpinCount = writeSpinCount;
    }

    /**
     * get autoRead.
     *
     * @return autoRead
     */
    public Boolean isAutoRead() {
        return autoRead;
    }

    /**
     * set autoRead.
     *
     * @param autoRead AUTO_READ
     */
    public void setAutoRead(final Boolean autoRead) {
        this.autoRead = autoRead;
    }

    /**
     * get allocator.
     *
     * @return ByteBufAllocator
     */
    public ByteBufAllocator getAllocator() {
        return UN_POOLED.equals(getAllocType()) ? UnpooledByteBufAllocator.DEFAULT : PooledByteBufAllocator.DEFAULT;
    }

    /**
     * get allocator type.
     *
     * @return allocator type
     */
    public String getAllocType() {
        return allocType;
    }

    /**
     * set allocator type.
     *
     * @param allocType allocator type
     */
    public void setAllocType(final String allocType) {
        this.allocType = allocType;
    }

    /**
     * get SoReuseaddr.
     *
     * @return soReuseAddr
     */
    public Boolean isSoReuseAddr() {
        return soReuseAddr;
    }

    /**
     * set setSoReuseAddr.
     *
     * @param soReuseAddr SO_REUSEADDR
     */
    public void setSoReuseAddr(final Boolean soReuseAddr) {
        this.soReuseAddr = soReuseAddr;
    }

    /**
     * get soRcvBuf.
     *
     * @return soRcvBuf
     */
    public Integer getSoRcvBuf() {
        return soRcvBuf;
    }

    /**
     * set soRcvBuf.
     *
     * @param soRcvBuf SO_RCVBUF
     */
    public void setSoRcvBuf(final Integer soRcvBuf) {
        this.soRcvBuf = soRcvBuf;
    }

    /**
     * message size estimator.
     *
     * @return MessageSizeEstimator
     */
    public MessageSizeEstimator getMessageSizeEstimatorBuild() {
        return Objects.isNull(messageSizeEstimator) || messageSizeEstimator == 8 ? DefaultMessageSizeEstimator.DEFAULT
                : new DefaultMessageSizeEstimator(this.getMessageSizeEstimator());
    }

    /**
     * get message size estimator.
     *
     * @return message size estimator
     */
    public Integer getMessageSizeEstimator() {
        return messageSizeEstimator;
    }

    /**
     * set message size estimator.
     *
     * @param messageSizeEstimator messageSizeEstimator
     */
    public void setMessageSizeEstimator(final Integer messageSizeEstimator) {
        this.messageSizeEstimator = messageSizeEstimator;
    }

    /**
     * single event executor.
     *
     * @return single event executor state
     */
    public Boolean getSingleEventExecutorPerGroup() {
        return singleEventExecutorPerGroup;
    }

    /**
     * set single event executor.
     *
     * @param singleEventExecutorPerGroup single executor
     */
    public void setSingleEventExecutorPerGroup(final Boolean singleEventExecutorPerGroup) {
        this.singleEventExecutorPerGroup = singleEventExecutorPerGroup;
    }
}
