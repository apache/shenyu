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

/**
 * The netty tcp configuration properties.
 */
public class NettyTcpProperties {

    private int selectCount = 1;

    private int workerCount = Runtime.getRuntime().availableProcessors() << 1;

    private int connectTimeoutMillis = 10000;

    private int writeBufferHighWaterMark = 65536;

    private int writeBufferLowWaterMark = 32768;

    private int writeSpinCount = 16;

    private boolean autoRead = true;

    private boolean tcpNodelay = true;

    private boolean soKeepalive;

    private boolean soReuseaddr;

    private int soLinger = -1;

    private int soBacklog = 128;

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
     * get soKeepalive.
     *
     * @return soKeepalive
     */
    public boolean isSoKeepalive() {
        return soKeepalive;
    }

    /**
     * get isSoReuseaddr.
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
     * get soBacklog.
     *
     * @return soBacklog
     */
    public int getSoBacklog() {
        return soBacklog;
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
     * set connectTimeoutMillis.
     *
     * @param connectTimeoutMillis connect timeout millis
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
     * @param writeSpinCount write spin count
     */
    public void setWriteSpinCount(final int writeSpinCount) {
        this.writeSpinCount = writeSpinCount;
    }

    /**
     * set autoRead.
     *
     * @param autoRead auto read
     */
    public void setAutoRead(final boolean autoRead) {
        this.autoRead = autoRead;
    }

    /**
     * set tcpNodelay.
     *
     * @param tcpNodelay tcp no delay
     */
    public void setTcpNodelay(final boolean tcpNodelay) {
        this.tcpNodelay = tcpNodelay;
    }

    /**
     * set soKeepalive.
     *
     * @param soKeepalive tcp keepalive
     */
    public void setSoKeepalive(final boolean soKeepalive) {
        this.soKeepalive = soKeepalive;
    }

    /**
     * ser setSoReuseaddr.
     *
     * @param soReuseaddr reuse addr
     */
    public void setSoReuseaddr(final boolean soReuseaddr) {
        this.soReuseaddr = soReuseaddr;
    }

    /**
     * set soLinger.
     *
     * @param soLinger linger
     */
    public void setSoLinger(final int soLinger) {
        this.soLinger = soLinger;
    }

    /**
     * set soBacklog.
     *
     * @param soBacklog tcp backlog
     */
    public void setSoBacklog(final int soBacklog) {
        this.soBacklog = soBacklog;
    }
}
