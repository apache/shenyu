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

import org.apache.shenyu.proxy.remote.ChannelFuture;

import java.util.function.Consumer;

/**
 * NettyChannelFuture .
 * proxy wap channel future.
 */
public class NettyChannelFuture implements ChannelFuture {
    
    private io.netty.channel.ChannelFuture future;
    
    /**
     * Instantiates a new Netty channel future..
     *
     * @param future future.
     */
    public NettyChannelFuture(final io.netty.channel.ChannelFuture future) {
        this.future = future;
    }
    
    /**
     * Complete.
     *
     * @param consumer the consumer
     */
    @Override
    public void onComplete(final Consumer<ChannelFuture> consumer) {
    
    }
    
    /**
     * Is done boolean.
     *
     * @return the boolean
     */
    @Override
    public boolean isDone() {
        return false;
    }
    
    /**
     * Cause thread.
     *
     * @return the thread
     */
    @Override
    public Throwable cause() {
        return null;
    }
    
    /**
     * Is connected boolean.
     *
     * @return the boolean
     */
    @Override
    public boolean isConnected() {
        return false;
    }
}
