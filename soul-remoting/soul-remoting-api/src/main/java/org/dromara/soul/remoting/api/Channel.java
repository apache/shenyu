/*
 *
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 */

package org.dromara.soul.remoting.api;

import java.net.SocketAddress;

/**
 * Channel
 * CreateDate: 2019/10/11 16:07
 *
 * @author sixh
 */
public interface Channel {

    /**
     * Is open boolean.
     *
     * @return the boolean
     */
    boolean isOpen();

    /**
     * Local address socket address.
     *
     * @return the socket address
     */
    SocketAddress localAddress();

    /**
     * Remote address socket address.
     *
     * @return the socket address
     */
    SocketAddress remoteAddress();

    /**
     * Write and flush channel handler.
     *
     * @param message the message
     * @return the channel handler
     */
    ChannelFuture send(Object message);

    /**
     * Is close boolean.
     *
     * @return the boolean
     */
    boolean isClose();

    /**
     * Close channel future.
     *
     * @return the channel future
     */
    ChannelFuture close();
}
