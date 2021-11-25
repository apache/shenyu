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
public class SimpleChannelRepository implements ChannelRepository {

    private static final Map<String, Channel> CHANNEL_FACTORY = new ConcurrentHashMap<>();

    @Override
    public void add(final String clientId, final Channel channel) {
        SimpleChannelRepository.CHANNEL_FACTORY.put(clientId, channel);
    }

    @Override
    public void remove(final String clientId) {
        SimpleChannelRepository.CHANNEL_FACTORY.remove(clientId);
    }

    @Override
    public Map<String, Channel> getFactory() {
        return SimpleChannelRepository.CHANNEL_FACTORY;
    }

    @Override
    public Channel get(final String clientId) {
        return SimpleChannelRepository.CHANNEL_FACTORY.get(clientId);
    }

    /**
     * is contain clientId.
     * @param clientId clientId
     * @return true contain, false not contain.
     */
    public boolean containClientId(final String clientId) {
        return SimpleChannelRepository.CHANNEL_FACTORY.containsKey(clientId);
    }

    /**
     * channel size.
     * @return channel size.
     */
    public int size() {
        return SimpleChannelRepository.CHANNEL_FACTORY.size();
    }

    /**
     * is active.
     * @param clientId clientId
     * @return is active.
     */
    public boolean isActive(final String clientId) {
        return get(clientId).isActive();
    }
}
