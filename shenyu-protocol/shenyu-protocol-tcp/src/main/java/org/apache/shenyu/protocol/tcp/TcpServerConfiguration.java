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

package org.apache.shenyu.protocol.tcp;

import java.util.Properties;

/**
 * tcp server configuration.
 */
public class TcpServerConfiguration {

    private int port = 9500;

    private int bossGroupThreadCount = 1;

    private int maxPayloadSize = 65536;

    private int workerGroupThreadCount = 12;

    private String leakDetectorLevel = "DISABLED";

    private Properties props;

    /**
     * get port.
     *
     * @return port
     */
    public int getPort() {
        return port;
    }

    /**
     * set port.
     *
     * @param port port
     */
    public void setPort(final int port) {
        this.port = port;
    }

    /**
     * get bossGroupThreadCount.
     *
     * @return bossGroupThreadCount
     */
    public int getBossGroupThreadCount() {
        return bossGroupThreadCount;
    }

    /**
     * set bossGroupThreadCount.
     *
     * @param bossGroupThreadCount bossGroupThreadCount
     */
    public void setBossGroupThreadCount(final int bossGroupThreadCount) {
        this.bossGroupThreadCount = bossGroupThreadCount;
    }

    /**
     * get maxPayloadSize.
     *
     * @return maxPayloadSize
     */
    public int getMaxPayloadSize() {
        return maxPayloadSize;
    }

    /**
     * set maxPayloadSize.
     *
     * @param maxPayloadSize maxPayloadSize
     */
    public void setMaxPayloadSize(final int maxPayloadSize) {
        this.maxPayloadSize = maxPayloadSize;
    }

    /**
     * get workerGroupThreadCount.
     *
     * @return workerGroupThreadCount
     */
    public int getWorkerGroupThreadCount() {
        return workerGroupThreadCount;
    }

    /**
     * set workerGroupThreadCount.
     *
     * @param workerGroupThreadCount workerGroupThreadCount
     */
    public void setWorkerGroupThreadCount(final int workerGroupThreadCount) {
        this.workerGroupThreadCount = workerGroupThreadCount;
    }

    /**
     * get leakDetectorLevel.
     *
     * @return leakDetectorLevel
     */
    public String getLeakDetectorLevel() {
        return leakDetectorLevel;
    }

    /**
     * set leakDetectorLevel.
     *
     * @param leakDetectorLevel leakDetectorLevel
     */
    public void setLeakDetectorLevel(final String leakDetectorLevel) {
        this.leakDetectorLevel = leakDetectorLevel;
    }

    public Properties getProps() {
        return props;
    }

    public void setProps(Properties props) {
        this.props = props;
    }
}
