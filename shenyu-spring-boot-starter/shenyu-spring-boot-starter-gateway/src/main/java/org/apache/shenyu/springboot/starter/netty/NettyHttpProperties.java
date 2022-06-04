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
 * The netty tcp properties.
 */
public class NettyHttpProperties {

    private Boolean webServerFactoryEnabled = true;

    private Integer selectCount = 1;

    private Integer workerCount = Runtime.getRuntime().availableProcessors() << 1;

    private Boolean accessLog = false;

    private ServerSocketChannelProperties serverSocketChannel = new ServerSocketChannelProperties();

    private SocketChannelProperties socketChannel = new SocketChannelProperties();

    /**
     * get webServerFactoryEnabled.
     *
     * @return webServerFactoryEnabled
     */
    public Boolean isWebServerFactoryEnabled() {
        return webServerFactoryEnabled;
    }

    /**
     * set webServerFactoryEnabled.
     * set to false, user can custom the netty tcp server config.
     *
     * @param webServerFactoryEnabled web server factory enabled
     */
    public void setWebServerFactoryEnabled(final Boolean webServerFactoryEnabled) {
        this.webServerFactoryEnabled = webServerFactoryEnabled;
    }

    /**
     * get select count.
     *
     * @return selectCount
     */
    public Integer getSelectCount() {
        return selectCount;
    }

    /**
     * set selectCount.
     *
     * @param selectCount select count
     */
    public void setSelectCount(final Integer selectCount) {
        this.selectCount = selectCount;
    }

    /**
     * get workerCount.
     *
     * @return workerCount
     */
    public Integer getWorkerCount() {
        return workerCount;
    }

    /**
     * set workerCount.
     *
     * @param workerCount worker count
     */
    public void setWorkerCount(final Integer workerCount) {
        this.workerCount = workerCount;
    }

    /**
     * get serverSocketChannel.
     *
     * @return serverSocketChannel
     */
    public ServerSocketChannelProperties getServerSocketChannel() {
        return serverSocketChannel;
    }

    /**
     * set serverSocketChannel.
     *
     * @param serverSocketChannel server socket channel config
     */
    public void setServerSocketChannel(final ServerSocketChannelProperties serverSocketChannel) {
        this.serverSocketChannel = serverSocketChannel;
    }

    /**
     * set socketChannel.
     *
     * @param socketChannel socket channel config
     */
    public void setSocketChannel(final SocketChannelProperties socketChannel) {
        this.socketChannel = socketChannel;
    }

    /**
     * get socketChannel.
     *
     * @return socketChannel
     */
    public SocketChannelProperties getSocketChannel() {
        return socketChannel;
    }

    /**
     * get access log state.
     *
     * @return access log state
     */
    public Boolean getAccessLog() {
        return accessLog;
    }

    /**
     * set access log state.
     *
     * @param accessLog access log state
     */
    public void setAccessLog(final Boolean accessLog) {
        this.accessLog = accessLog;
    }

    public static class ServerSocketChannelProperties extends NettyChannelProperties {

        private Integer soBacklog = 128;

        /**
         * get soBacklog.
         *
         * @return soBacklog
         */
        public Integer getSoBacklog() {
            return soBacklog;
        }

        /**
         * set soBacklog.
         *
         * @param soBacklog SO_BACKLOG
         */
        public void setSoBacklog(final Integer soBacklog) {
            this.soBacklog = soBacklog;
        }
    }

    public static class SocketChannelProperties extends NettyChannelProperties {

        private Boolean soKeepAlive = false;

        private Integer soLinger = -1;

        private Boolean tcpNoDelay = true;

        private Integer soSndBuf = 16384;

        private Integer ipTos = 0;

        private Boolean allowHalfClosure = false;

        /**
         * get soKeepAlive.
         *
         * @return soKeepAlive
         */
        public Boolean isSoKeepAlive() {
            return soKeepAlive;
        }

        /**
         * set soKeepAlive.
         *
         * @param soKeepAlive SO_KEEPALIVE
         */
        public void setSoKeepAlive(final Boolean soKeepAlive) {
            this.soKeepAlive = soKeepAlive;
        }

        /**
         * get soLinger.
         *
         * @return soLinger
         */
        public Integer getSoLinger() {
            return soLinger;
        }

        /**
         * set soLinger.
         *
         * @param soLinger SO_LINGER
         */
        public void setSoLinger(final Integer soLinger) {
            this.soLinger = soLinger;
        }

        /**
         * get tcpNoDelay.
         *
         * @return tcpNoDelay
         */
        public Boolean isTcpNoDelay() {
            return tcpNoDelay;
        }

        /**
         * set tcpNoDelay.
         *
         * @param tcpNoDelay TCP_NODELAY
         */
        public void setTcpNoDelay(final Boolean tcpNoDelay) {
            this.tcpNoDelay = tcpNoDelay;
        }

        /**
         * get soSndBuf.
         *
         * @return soSndBuf
         */
        public Integer getSoSndBuf() {
            return soSndBuf;
        }

        /**
         * set soSndBuf.
         *
         * @param soSndBuf SO_SNDBUF
         */
        public void setSoSndBuf(final Integer soSndBuf) {
            this.soSndBuf = soSndBuf;
        }

        /**
         * get ipTos.
         * @return ipTos
         */
        public Integer getIpTos() {
            return ipTos;
        }

        /**
         * set ipTos.
         *
         * @param ipTos IP_TOS
         */
        public void setIpTos(final Integer ipTos) {
            this.ipTos = ipTos;
        }

        /**
         * get isAllowHalfClosure.
         *
         * @return isAllowHalfClosure
         */
        public Boolean isAllowHalfClosure() {
            return allowHalfClosure;
        }

        /**
         * set allowHalfClosure.
         *
         * @param allowHalfClosure ALLOW_HALF_CLOSURE
         */
        public void setAllowHalfClosure(final Boolean allowHalfClosure) {
            this.allowHalfClosure = allowHalfClosure;
        }
    }
}
