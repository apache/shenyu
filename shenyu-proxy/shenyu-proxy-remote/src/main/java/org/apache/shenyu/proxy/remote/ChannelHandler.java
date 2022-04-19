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
 * ChannelHandler .
 * channel handler Processed event notifications.
 */
public interface ChannelHandler {
    
    /**
     * send a message.
     *
     * @param channel the channel
     * @param message the message
     */
    void sent(Channel channel, Object message);
    
    /**
     * Handling of received messages.
     *
     * @param channel the channel
     * @param message the message
     */
    void receive(Channel channel, Object message);
    
    /**
     * connect.
     *
     * @param channel the channel
     */
    void connection(Channel channel);
    
    /**
     * Disconnect.
     *
     * @param channel the channel
     */
    void disConnection(Channel channel);
    
    /**
     * An exception occurred.
     *
     * @param channel   the channel
     * @param throwable the throwable
     */
    void caught(Channel channel, Throwable throwable);
}
