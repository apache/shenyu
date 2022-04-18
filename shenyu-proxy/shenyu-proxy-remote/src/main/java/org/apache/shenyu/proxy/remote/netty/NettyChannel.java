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

package org.apache.shenyu.proxy.remote.netty;

import io.netty.channel.ChannelFutureListener;
import org.apache.shenyu.proxy.remote.Channel;
import org.apache.shenyu.proxy.remote.ChannelFuture;

import java.net.SocketAddress;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * NettyChannel .
 */
public class NettyChannel implements Channel {
    
    private static final ConcurrentMap<io.netty.channel.Channel, NettyChannel> CHANNEL_CACHE = new ConcurrentHashMap<>();
    
    private final io.netty.channel.Channel channel;
    
    /**
     * Instantiates a new Netty channel.
     *
     * @param channel the channel
     */
    public NettyChannel(final io.netty.channel.Channel channel) {
        this.channel = channel;
    }
    
    /**
     * send.
     * Send data to network channel.
     *
     * @param message the message
     * @return the channel future
     */
    @Override
    public ChannelFuture send(final Object message) {
        io.netty.channel.ChannelFuture channelFuture = channel.writeAndFlush(message);
        return new NettyChannelFuture(channelFuture);
    }
    
    /**
     * Determine if it is connected.
     *
     * @return boolean boolean
     */
    @Override
    public boolean isConnected() {
        return channel.isActive();
    }
    
    /**
     * remote address.
     *
     * @return socket address
     */
    @Override
    public SocketAddress remoteAddress() {
        return channel.remoteAddress();
    }
    
    /**
     * Local address socket address.
     *
     * @return the socket address
     */
    @Override
    public SocketAddress localAddress() {
        return channel.localAddress();
    }
    
    /**
     * Is opened boolean.
     *
     * @return the boolean
     */
    @Override
    public boolean isOpened() {
        return channel.isOpen();
    }
    
    /**
     * Is close boolean.
     *
     * @return the boolean
     */
    @Override
    public boolean isClose() {
        return !this.isOpened();
    }
    
    /**
     * Close.
     */
    @Override
    public void close() {
        channel.close().addListener((ChannelFutureListener) channelFuture -> {
            removeChannel(channel);
        });
    }
    
    /**
     * Get a channel, create a Netty Channel if it doesn't exist.
     *
     * @param channel the channel nettyChannel.
     * @return the or add channel
     */
    public static NettyChannel getOrAddChannel(final io.netty.channel.Channel channel) {
        NettyChannel nettyChannel = CHANNEL_CACHE.get(channel);
        
        if (nettyChannel == null) {
            NettyChannel ret = new NettyChannel(channel);
            if (channel.isActive()) {
                nettyChannel = CHANNEL_CACHE.putIfAbsent(channel, ret);
            }
            if (nettyChannel == null) {
                nettyChannel = ret;
            }
        }
        return nettyChannel;
    }
    
    /**
     * If the channel has been disconnected, it is removed from the cache.
     *
     * @param channel the channel
     */
    public static void removeChannel(final io.netty.channel.Channel channel) {
        if (channel != null && !channel.isActive()) {
            CHANNEL_CACHE.remove(channel);
        }
    }
}
