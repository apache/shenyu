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
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Channel and client-id ownership registry.
 *
 * <p>The ownership index is written only by {@link #takeover}: ownership of a
 * client id is transferred atomically with the duplicate-clientId takeover, and
 * {@link #remove} clears it with a conditional two-argument remove so a replaced
 * connection closing late cannot evict the connection that took over (#7918).</p>
 *
 * <p>Note that on a takeover the replaced connection's stored will is discarded
 * on purpose: the client is alive on the new connection, so publishing the will
 * of the old one would be wrong. Other brokers publish it instead — this is a
 * deliberate deviation, not a spec-mandated choice.</p>
 */
public class ChannelRepository implements BaseRepository<Channel, String> {

    private final Map<Channel, String> channelFactory = new ConcurrentHashMap<>();

    private final Map<String, Channel> clientFactory = new ConcurrentHashMap<>();

    @Override
    public void add(final Channel channel, final String clientId) {
        channelFactory.put(channel, clientId);
    }

    @Override
    public void remove(final Channel channel) {
        final String clientId = channelFactory.remove(channel);
        if (Objects.nonNull(clientId)) {
            // only clear ownership if this channel still holds it: a stale channel
            // closing late must not evict the connection that took over (#7918)
            clientFactory.remove(clientId, channel);
        }
    }

    @Override
    public String get(final Channel channel) {
        return channelFactory.get(channel);
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
        final Channel previous = clientFactory.put(clientId, channel);
        channelFactory.put(channel, clientId);
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
        return clientFactory.get(clientId) == channel;
    }
}
