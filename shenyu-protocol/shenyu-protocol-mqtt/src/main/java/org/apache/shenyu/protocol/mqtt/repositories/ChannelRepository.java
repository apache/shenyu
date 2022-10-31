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
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;

/**
 * channel repository.
 */
public class ChannelRepository implements BaseRepository<Channel, String> {

    private static final Map<Channel, String> CHANNEL_FACTORY = new ConcurrentHashMap<>();

    @Override
    public void add(final Channel channel, final String clientId) {
        CompletableFuture.runAsync(() -> CHANNEL_FACTORY.put(channel, clientId));
    }

    @Override
    public void remove(final Channel channel) {
        CHANNEL_FACTORY.remove(channel);
    }

    @Override
    public String get(final Channel channel) {
        return CHANNEL_FACTORY.get(channel);
    }

}
