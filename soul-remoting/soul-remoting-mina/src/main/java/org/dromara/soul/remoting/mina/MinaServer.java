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

import java.io.IOException;
import java.util.Collection;
import java.util.HashSet;
import java.util.Map;

import org.apache.mina.core.filterchain.DefaultIoFilterChainBuilder;
import org.apache.mina.core.session.IoSession;
import org.apache.mina.http.HttpServerCodec;
import org.apache.mina.transport.socket.SocketAcceptor;
import org.apache.mina.transport.socket.nio.NioSocketAcceptor;
import org.dromara.soul.common.Attribute;
import org.dromara.soul.remoting.api.AbstractNetServer;
import org.dromara.soul.remoting.api.Channel;
import org.dromara.soul.remoting.api.ChannelHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * MinaServer
 * CreateDate: 2019/10/15 17:21
 *
 * @author sixh
 */
public class MinaServer extends AbstractNetServer {
    private Logger logger = LoggerFactory.getLogger(MinaServer.class);
    private SocketAcceptor acceptor;
    private MinaServerHandler minaServerHandler;

    /**
     * Instantiates a new Abstract net server.
     *
     * @param attribute the properties
     * @param handler   the handler
     */
    public MinaServer(Attribute attribute, ChannelHandler handler) {
        super(attribute, handler);
    }

    @Override
    protected void close() {
        if (acceptor != null) {
            acceptor.unbind();
        }
    }

    @Override
    public void bind() {
        // set thread pool.
        minaServerHandler = new MinaServerHandler(getAttribute(), this);
        acceptor = new NioSocketAcceptor(getIoThreads());
        MinaCodec minaCodec = new MinaCodec();
        DefaultIoFilterChainBuilder filterChain = acceptor.getFilterChain();
        filterChain.addFirst("encode", new MinaCodec().getEncode());
        filterChain.addLast("http_codec", new HttpServerCodec());
        filterChain.addLast("decode", new MinaCodec().getEncode());
        acceptor.setHandler(minaServerHandler);
        try {
            acceptor.bind(this.bindSocketAddress());
        } catch (IOException e) {
            logger.error("web server bind error", e);
        }
    }

    @Override
    public Collection<Channel> getChannels() {
        Map<Long, IoSession> sessions = acceptor.getManagedSessions();
        Collection<Channel> channels = new HashSet<>();
        for (IoSession session : sessions.values()) {
            if (session.isConnected()) {
                channels.add(new MinaChannel(session));
            }
        }
        return channels;
    }
}
