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

package org.apache.shenyu.protocol.mqtt.repositories;

import io.netty.channel.Channel;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * channel repository.
 */
public class ChannelRepository implements BaseRepository<Channel, String> {

    private static final Map<Channel, String> CHANNEL_FACTORY = new ConcurrentHashMap<>();

    private static final Map<String, Channel> CLIENT_FACTORY = new ConcurrentHashMap<>();

    @Override
    public void add(final Channel channel, final String clientId) {
        CHANNEL_FACTORY.put(channel, clientId);
        CLIENT_FACTORY.put(clientId, channel);
    }

    @Override
    public void remove(final Channel channel) {
        CHANNEL_FACTORY.remove(channel);
    }

    @Override
    public String get(final Channel channel) {
        return CHANNEL_FACTORY.get(channel);
    }

    /**
     * Transfers the ownership of a client id to the new channel, returning the
     * channel that owned it before, if any (#7918). A duplicate clientId must
     * replace the existing connection instead of coexisting with it.
     *
     * @param channel the channel of the new connection
     * @param clientId the announced client identifier
     * @return the previously owning channel, or null when the client was not connected
     */
    public synchronized Channel takeover(final Channel channel, final String clientId) {
        final Channel previous = CLIENT_FACTORY.put(clientId, channel);
        CHANNEL_FACTORY.put(channel, clientId);
        return previous;
    }

    /**
     * Whether the channel still owns the client id. Cleanup of session-scoped
     * state must be fenced on this, so a stale connection closing late cannot
     * discard the state of the connection that replaced it (#7918).
     *
     * @param clientId the client identifier
     * @param channel the channel closing
     * @return true when the channel is the current owner of the client id
     */
    public boolean isOwner(final String clientId, final Channel channel) {
        return CLIENT_FACTORY.get(clientId) == channel;
    }
}
