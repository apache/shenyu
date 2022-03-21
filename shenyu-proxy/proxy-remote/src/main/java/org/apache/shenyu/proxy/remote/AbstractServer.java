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

package org.apache.shenyu.proxy.remote;

/**
 * AbstractServer .
 * start server abstraction.
 */
public abstract class AbstractServer implements Server, ChannelHandler {
    
    /**
     * Handle related handlers.
     */
    private final ChannelHandler channelHandler;
    
    /**
     * Instantiates a new Abstract server.
     *
     * @param channelHandler the channel handler
     */
    public AbstractServer(final ChannelHandler channelHandler) {
        this.channelHandler = channelHandler;
    }
    
    /**
     * start a server.
     *
     * @param config 启动的config处理.
     */
    @Override
    public void start(final ServerConfig config) {
        this.start0(config.getPort());
    }
    
    /**
     * send a message.
     *
     * @param channel the channel
     * @param message the message
     */
    @Override
    public void sent(final Channel channel, final Object message) {
        channelHandler.sent(channel, message);
    }
    
    /**
     * Handling of received messages.
     *
     * @param channel the channel
     * @param message the message
     */
    @Override
    public void receive(final Channel channel, final Object message) {
        channelHandler.receive(channel, message);
    }
    
    /**
     * connect.
     *
     * @param channel the channel
     */
    @Override
    public void connection(final Channel channel) {
        channelHandler.connection(channel);
    }
    
    /**
     * Disconnect.
     *
     * @param channel the channel
     */
    @Override
    public void disConnection(final Channel channel) {
        channelHandler.disConnection(channel);
    }
    
    /**
     * An exception occurred.
     *
     * @param channel   the channel
     * @param throwable the throwable
     */
    @Override
    public void caught(final Channel channel, final Throwable throwable) {
        channelHandler.caught(channel, throwable);
    }
    
    /**
     * start a server.
     *
     * @param port the port
     */
    protected abstract void start0(int port);
}
