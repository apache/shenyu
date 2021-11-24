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

package org.apache.shenyu.bootstrap.configuration;

/**
 * The netty tcp config.
 */
public class NettyTcpConfig {

    private int selectCount;

    private int workerCount;

    private Integer connectTimeoutMillis;

    private Integer writeBufferHighWaterMark;

    private Integer writeBufferLowWaterMark;

    private Integer writeSpinCount;

    private Boolean autoRead;

    private Boolean tcpNodelay;

    private Boolean soKeepalive;

    private Boolean soReuseaddr;

    private Integer soLinger;

    private Integer soBacklog;

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
    public Integer getWriteSpinCount() {
        return writeSpinCount;
    }

    /**
     * get autoRead.
     *
     * @return autoRead
     */
    public Boolean isAutoRead() {
        return autoRead;
    }
}
