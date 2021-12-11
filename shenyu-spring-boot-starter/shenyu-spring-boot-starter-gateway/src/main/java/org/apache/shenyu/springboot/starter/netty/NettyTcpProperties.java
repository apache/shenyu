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

import org.apache.shenyu.springboot.starter.netty.config.ServerSocketChannelConfig;
import org.apache.shenyu.springboot.starter.netty.config.SocketChannelConfig;

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
}
