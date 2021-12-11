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

package org.apache.shenyu.springboot.starter.netty.config;

/**
 * Netty server socket channel config.
 */
public class ServerSocketChannelConfig extends ChannelConfig {

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
