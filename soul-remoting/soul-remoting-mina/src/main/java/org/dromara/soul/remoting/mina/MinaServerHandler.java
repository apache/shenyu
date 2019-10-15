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

import java.util.concurrent.TimeUnit;
import org.apache.mina.core.service.IoHandlerAdapter;
import org.apache.mina.core.session.IoSession;
import org.dromara.soul.common.Attribute;
import org.dromara.soul.common.Const;
import org.dromara.soul.remoting.api.Channel;
import org.dromara.soul.remoting.api.ChannelCache;
import org.dromara.soul.remoting.api.ChannelCacheListener;
import org.dromara.soul.remoting.api.ChannelHandler;

/**
 * MinaServerHandler
 * CreateDate: 2019/10/15 17:40
 *
 * @author sixh
 */
public class MinaServerHandler extends IoHandlerAdapter implements ChannelCacheListener {

    private ChannelHandler handler;

    private Attribute attribute;

    private ChannelCache channelCache;

    /**
     * Instantiates a new Mina server handler.
     *
     * @param attribute the attribute
     * @param handler   the handler
     */
    public MinaServerHandler(Attribute attribute, ChannelHandler handler) {
        if (attribute == null) {
            throw new IllegalArgumentException("attribute is null");
        }
        if (handler == null) {
            throw new IllegalArgumentException("handler is null");
        }
        this.handler = handler;
        this.attribute = attribute;
        //超时处理.
        Integer timeOut = this.attribute.getProperty(Const.NET_TIMEOUT_KEY, 3);
        channelCache = new ChannelCache(timeOut, TimeUnit.SECONDS, "minaChannelCache", this);
    }

    @Override
    public void sessionOpened(IoSession session) throws Exception {
        MinaChannel channel = new MinaChannel(session);
        handler.connected(channel);
        channelCache.put(channel.getId(), channel);
    }

    @Override
    public void sessionClosed(IoSession session) throws Exception {
        MinaChannel channel = new MinaChannel(session);
        handler.disconnected(channel);
        channelCache.remove(channel.getId());
    }

    @Override
    public void exceptionCaught(IoSession session, Throwable cause) throws Exception {
        handler.exceptionCaught(new MinaChannel(session), cause);
    }

    @Override
    public void messageReceived(IoSession session, Object message) throws Exception {
        handler.received(new MinaChannel(session), message);
    }

    @Override
    public void messageSent(IoSession session, Object message) throws Exception {
        handler.sent(new MinaChannel(session), message);
    }

    @Override
    public void timeout(Channel channel) {
        handler.timeout(channel);
        channel.close();
    }

}
