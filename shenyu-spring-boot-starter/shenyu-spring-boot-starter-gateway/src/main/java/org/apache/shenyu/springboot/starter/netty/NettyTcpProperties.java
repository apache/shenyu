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

/**
 * The netty tcp configuration properties.
 */
public class NettyTcpProperties {

    private int selectCount = 1;

    private int workerCount = Runtime.getRuntime().availableProcessors() << 1;

    private ServerSocketChannelConfig serverSocketChannelConfig = new ServerSocketChannelConfig();

    private SocketChannelConfig socketChannelConfig = new SocketChannelConfig();

    /**
     * get select count.
     *
     * @return selectCount
     */
    public int getSelectCount() {
        return selectCount;
    }

    /**
     * get workerCount.
     *
     * @return workerCount
     */
    public int getWorkerCount() {
        return workerCount;
    }

    /**
     * get serverSocketChannelConfig.
     *
     * @return serverSocketChannelConfig
     */
    public ServerSocketChannelConfig getServerSocketChannelConfig() {
        return serverSocketChannelConfig;
    }

    /**
     * get socketChannelConfig.
     *
     * @return socketChannelConfig
     */
    public SocketChannelConfig getSocketChannelConfig() {
        return socketChannelConfig;
    }

    /**
     * set selectCount.
     *
     * @param selectCount select count
     */
    public void setSelectCount(final int selectCount) {
        this.selectCount = selectCount;
    }

    /**
     * set workerCount.
     *
     * @param workerCount worker count
     */
    public void setWorkerCount(final int workerCount) {
        this.workerCount = workerCount;
    }

    /**
     * set serverSocketChannelConfig.
     *
     * @param serverSocketChannelConfig server socket channel config
     */
    public void setServerSocketChannelConfig(final ServerSocketChannelConfig serverSocketChannelConfig) {
        this.serverSocketChannelConfig = serverSocketChannelConfig;
    }

    /**
     * set socketChannelConfig.
     *
     * @param socketChannelConfig socket channel config
     */
    public void setSocketChannelConfig(final SocketChannelConfig socketChannelConfig) {
        this.socketChannelConfig = socketChannelConfig;
    }

    static class ChannelConfig {

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
         * get allocator type.
         *
         * @return allocator type
         */
        public String getAllocType() {
            return allocType;
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

    static class ServerSocketChannelConfig extends ChannelConfig {

        private int soRcvbuf = 87380;

        private int soBacklog = 128;

        private boolean soReuseaddr;

        /**
         * get soRcvbuf.
         *
         * @return soRcvbuf
         */
        public int getSoRcvbuf() {
            return soRcvbuf;
        }

        /**
         * get soBacklog.
         *
         * @return soBacklog
         */
        public int getSoBacklog() {
            return soBacklog;
        }

        /**
         * get SoReuseaddr.
         *
         * @return soReuseaddr
         */
        public boolean isSoReuseaddr() {
            return soReuseaddr;
        }

        /**
         * set soRcvbuf.
         *
         * @param soRcvbuf SO_RCVBUF
         */
        public void setSoRcvbuf(final int soRcvbuf) {
            this.soRcvbuf = soRcvbuf;
        }

        /**
         * set soBacklog.
         *
         * @param soBacklog SO_BACKLOG
         */
        public void setSoBacklog(final int soBacklog) {
            this.soBacklog = soBacklog;
        }

        /**
         * ser setSoReuseaddr.
         *
         * @param soReuseaddr SO_REUSEADDR
         */
        public void setSoReuseaddr(final boolean soReuseaddr) {
            this.soReuseaddr = soReuseaddr;
        }
    }

    static class SocketChannelConfig extends ChannelConfig {

        private boolean soKeepalive;

        private boolean soReuseaddr;

        private int soLinger = -1;

        private boolean tcpNodelay = true;

        private int soRcvbuf = 87380;

        private int soSndbuf = 16384;

        private int ipTos;

        private boolean allowHalfClosure;

        /**
         * get soKeepalive.
         *
         * @return soKeepalive
         */
        public boolean isSoKeepalive() {
            return soKeepalive;
        }

        /**
         * get SoReuseaddr.
         *
         * @return soReuseaddr
         */
        public boolean isSoReuseaddr() {
            return soReuseaddr;
        }

        /**
         * get soLinger.
         *
         * @return soLinger
         */
        public int getSoLinger() {
            return soLinger;
        }

        /**
         * get tcpNodelay.
         *
         * @return tcpNodelay
         */
        public boolean isTcpNodelay() {
            return tcpNodelay;
        }

        /**
         * get soRcvbuf.
         *
         * @return soRcvbuf
         */
        public int getSoRcvbuf() {
            return soRcvbuf;
        }

        /**
         * get soSndbuf.
         *
         * @return soSndbuf
         */
        public int getSoSndbuf() {
            return soSndbuf;
        }

        /**
         * get ipTos.
         * @return ipTos
         */
        public int getIpTos() {
            return ipTos;
        }

        /**
         * get isAllowHalfClosure.
         *
         * @return isAllowHalfClosure
         */
        public boolean isAllowHalfClosure() {
            return allowHalfClosure;
        }

        /**
         * set soKeepalive.
         *
         * @param soKeepalive SO_KEEPALIVE
         */
        public void setSoKeepalive(final boolean soKeepalive) {
            this.soKeepalive = soKeepalive;
        }

        /**
         * ser setSoReuseaddr.
         *
         * @param soReuseaddr SO_REUSEADDR
         */
        public void setSoReuseaddr(final boolean soReuseaddr) {
            this.soReuseaddr = soReuseaddr;
        }

        /**
         * set soLinger.
         *
         * @param soLinger SO_LINGER
         */
        public void setSoLinger(final int soLinger) {
            this.soLinger = soLinger;
        }

        /**
         * set tcpNodelay.
         *
         * @param tcpNodelay TCP_NODELAY
         */
        public void setTcpNodelay(final boolean tcpNodelay) {
            this.tcpNodelay = tcpNodelay;
        }

        /**
         * set soRcvbuf.
         *
         * @param soRcvbuf SO_RCVBUF
         */
        public void setSoRcvbuf(final int soRcvbuf) {
            this.soRcvbuf = soRcvbuf;
        }

        /**
         * set soSndbuf.
         *
         * @param soSndbuf SO_SNDBUF
         */
        public void setSoSndbuf(final int soSndbuf) {
            this.soSndbuf = soSndbuf;
        }

        /**
         * set ipTos.
         *
         * @param ipTos IP_TOS
         */
        public void setIpTos(final int ipTos) {
            this.ipTos = ipTos;
        }

        /**
         * set allowHalfClosure.
         *
         * @param allowHalfClosure ALLOW_HALF_CLOSURE
         */
        public void setAllowHalfClosure(final boolean allowHalfClosure) {
            this.allowHalfClosure = allowHalfClosure;
        }
    }
}
