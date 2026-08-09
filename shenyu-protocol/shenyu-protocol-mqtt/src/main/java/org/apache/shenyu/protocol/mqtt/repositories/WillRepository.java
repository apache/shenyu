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
 * Stores Last Will and Testament for connected clients.
 * Will is set on CONNECT and cleared on graceful DISCONNECT.
 * On ungraceful disconnect (channelInactive with will present), the will is published.
 */
public class WillRepository implements BaseRepository<Channel, WillRepository.WillEntry> {

    private static final Map<Channel, WillEntry> WILL_FACTORY = new ConcurrentHashMap<>();

    @Override
    public void add(final Channel channel, final WillEntry willEntry) {
        WILL_FACTORY.put(channel, willEntry);
    }

    @Override
    public void remove(final Channel channel) {
        WILL_FACTORY.remove(channel);
    }

    @Override
    public WillEntry get(final Channel channel) {
        return WILL_FACTORY.get(channel);
    }

    /**
     * Holds the will message fields from a CONNECT payload.
     */
    public static class WillEntry {

        private final String topic;

        private final byte[] message;

        private final int qos;

        private final boolean retain;

        public WillEntry(final String topic, final byte[] message, final int qos, final boolean retain) {
            this.topic = topic;
            this.message = message;
            this.qos = qos;
            this.retain = retain;
        }

        public String getTopic() {
            return topic;
        }

        public byte[] getMessage() {
            return message;
        }

        public int getQos() {
            return qos;
        }

        public boolean isRetain() {
            return retain;
        }
    }
}