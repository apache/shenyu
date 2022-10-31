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
import io.netty.handler.codec.mqtt.MqttTopicSubscription;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CopyOnWriteArraySet;

/**
 * Topic and channel association.
 */
public class SubscribeRepository implements BaseRepository<List<String>, List<Channel>> {

    private static final Logger LOG = LoggerFactory.getLogger(SubscribeRepository.class);

    private static final Map<String, List<Channel>> TOPIC_CHANNEL_FACTORY = new ConcurrentHashMap<>();

    @Override
    public void add(final List<String> topics, final List<Channel> channels) {
        CompletableFuture.runAsync(() -> topics.parallelStream().forEach(s -> {
            List<Channel> list = get(s);
            list.addAll(channels);
            TOPIC_CHANNEL_FACTORY.put(s, list);
        }));
    }

    /**
     * add subscribe channel.
     * @param channel channel
     * @param mqttTopicSubscription mqtt subscription info
     */
    public void add(final Channel channel, final List<MqttTopicSubscription> mqttTopicSubscription) {
        CompletableFuture.runAsync(() -> mqttTopicSubscription.parallelStream().forEach(s -> {
            List<Channel> channels = get(s.topicName());
            channels.add(channel);
            TOPIC_CHANNEL_FACTORY.put(s.topicName(), channels);
        }));
    }

    @Override
    public void remove(final List<String> topics) {
        CompletableFuture.runAsync(() -> topics.parallelStream().forEach(TOPIC_CHANNEL_FACTORY::remove));
    }

    /**
     * remove subscribe channel.
     * @param topics topics
     * @param channel channel
     */
    public void remove(final List<String> topics, final Channel channel) {
        CompletableFuture.runAsync(() -> topics.parallelStream().forEach(topic -> TOPIC_CHANNEL_FACTORY.get(topic).remove(channel)));
    }

    @Override
    public List<Channel> get(final List<String> topics) {
        Set<Channel> channels = new CopyOnWriteArraySet<>();
        topics.parallelStream().forEach(s -> channels.addAll(TOPIC_CHANNEL_FACTORY.get(s)));
        return new CopyOnWriteArrayList<>(channels);
    }

    /**
     * get Channels.
     * @param topic topic
     * @return Channels
     */
    public List<Channel> get(final String topic) {
        return TOPIC_CHANNEL_FACTORY.getOrDefault(topic, new CopyOnWriteArrayList<>());
    }

}
