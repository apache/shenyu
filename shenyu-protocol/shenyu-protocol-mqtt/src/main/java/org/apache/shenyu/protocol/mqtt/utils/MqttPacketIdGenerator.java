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

package org.apache.shenyu.protocol.mqtt.utils;

import io.netty.channel.Channel;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Allocates packet identifiers for outbound messages from each channel's own id space.
 */
public final class MqttPacketIdGenerator {

    private static final int MIN_PACKET_ID = 1;

    private static final int MAX_PACKET_ID = 0xFFFF;

    private static final Map<Channel, AtomicInteger> CHANNEL_PACKET_ID_FACTORY = new ConcurrentHashMap<>();

    private MqttPacketIdGenerator() {
    }

    /**
     * get next packet id of the channel.
     * @param channel channel
     * @return next packet id
     */
    public static int next(final Channel channel) {
        AtomicInteger packetId = CHANNEL_PACKET_ID_FACTORY.computeIfAbsent(channel, key -> new AtomicInteger());
        return packetId.updateAndGet(current -> current >= MAX_PACKET_ID ? MIN_PACKET_ID : current + 1);
    }

    /**
     * remove the channel packet id.
     * @param channel channel
     */
    public static void remove(final Channel channel) {
        CHANNEL_PACKET_ID_FACTORY.remove(channel);
    }

}
