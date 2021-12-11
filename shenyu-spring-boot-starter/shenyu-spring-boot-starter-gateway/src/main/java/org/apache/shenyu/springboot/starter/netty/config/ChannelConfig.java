package org.apache.shenyu.springboot.starter.netty.config;

import io.netty.buffer.ByteBufAllocator;
import io.netty.buffer.PooledByteBufAllocator;
import io.netty.buffer.UnpooledByteBufAllocator;

/**
 * Netty channel config.
 */
public class ChannelConfig {

    private int connectTimeoutMillis = 10000;

    private int writeBufferHighWaterMark = 65536;

    private int writeBufferLowWaterMark = 32768;

    private int writeSpinCount = 16;

    private boolean autoRead = true;

    private String allocType = "pooled";

    /**
     * get connectTimeoutMillis.
     *
     * @return connectTimeoutMillis
     */
    public int getConnectTimeoutMillis() {
        return connectTimeoutMillis;
    }

    /**
     * get writeBufferHighWaterMark.
     *
     * @return writeBufferHighWaterMark
     */
    public int getWriteBufferHighWaterMark() {
        return writeBufferHighWaterMark;
    }

    /**
     * get writeBufferLowWaterMark.
     *
     * @return writeBufferLowWaterMark
     */
    public int getWriteBufferLowWaterMark() {
        return writeBufferLowWaterMark;
    }

    /**
     * get writeSpinCount.
     *
     * @return writeSpinCount
     */
    public int getWriteSpinCount() {
        return writeSpinCount;
    }

    /**
     * get autoRead.
     *
     * @return autoRead
     */
    public boolean isAutoRead() {
        return autoRead;
    }

    /**
     * get allocator.
     *
     * @return ByteBufAllocator
     */
    public ByteBufAllocator getAllocator() {
        return "unpooled".equals(allocType) ? UnpooledByteBufAllocator.DEFAULT : PooledByteBufAllocator.DEFAULT;
    }

    /**
     * set connectTimeoutMillis.
     *
     * @param connectTimeoutMillis CONNECT_TIMEOUT_MILLIS
     */
    public void setConnectTimeoutMillis(final int connectTimeoutMillis) {
        this.connectTimeoutMillis = connectTimeoutMillis;
    }

    /**
     * set writeBufferHighWaterMark.
     *
     * @param writeBufferHighWaterMark write buffer high water mark
     */
    public void setWriteBufferHighWaterMark(final int writeBufferHighWaterMark) {
        this.writeBufferHighWaterMark = writeBufferHighWaterMark;
    }

    /**
     * set writeBufferLowWaterMark.
     *
     * @param writeBufferLowWaterMark write buffer low water mark
     */
    public void setWriteBufferLowWaterMark(final int writeBufferLowWaterMark) {
        this.writeBufferLowWaterMark = writeBufferLowWaterMark;
    }

    /**
     * set writeSpinCount.
     *
     * @param writeSpinCount WRITE_SPIN_COUNT
     */
    public void setWriteSpinCount(final int writeSpinCount) {
        this.writeSpinCount = writeSpinCount;
    }

    /**
     * set autoRead.
     *
     * @param autoRead AUTO_READ
     */
    public void setAutoRead(final boolean autoRead) {
        this.autoRead = autoRead;
    }

    /**
     * set allocator type.
     *
     * @param allocType allocator type
     */
    public void setAllocType(final String allocType) {
        this.allocType = allocType;
    }
}
