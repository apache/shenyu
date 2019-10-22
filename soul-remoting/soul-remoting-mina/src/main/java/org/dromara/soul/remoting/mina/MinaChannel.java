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

package org.dromara.soul.remoting.mina;

import java.net.SocketAddress;

import org.apache.mina.core.future.IoFuture;
import org.apache.mina.core.future.IoFutureListener;
import org.apache.mina.core.future.WriteFuture;
import org.apache.mina.core.session.IoSession;
import org.dromara.soul.remoting.api.Channel;
import org.dromara.soul.remoting.api.ChannelFuture;

/**
 * MinaChannel
 * CreateDate: 2019/10/15 17:48
 *
 * @author sixh
 */
public class MinaChannel implements Channel {

    private IoSession session;

    /**
     * Instantiates a new Mina channel.
     *
     * @param session the session
     */
    public MinaChannel(IoSession session) {
        this.session = session;
    }

    @Override
    public boolean isOpen() {
        return this.session.isConnected();
    }

    @Override
    public SocketAddress localAddress() {
        return this.session.getLocalAddress();
    }

    @Override
    public SocketAddress remoteAddress() {
        return this.session.getRemoteAddress();
    }

    @Override
    public ChannelFuture send(Object message) {
        try {
            WriteFuture future = this.session.write(message);
            return new MinaChannelFuture(future);
        } catch (Throwable e) {
            throw new RuntimeException("Failed to send message " + message + " to " + remoteAddress() + ", cause: " + e.getMessage());
        }
    }

    @Override
    public boolean isClose() {
        return !isOpen();
    }

    @Override
    public ChannelFuture close() {
        return new MinaChannelFuture(this.session.closeNow());
    }

    @Override
    public String getId() {
        return String.valueOf(session.getId());
    }
}
