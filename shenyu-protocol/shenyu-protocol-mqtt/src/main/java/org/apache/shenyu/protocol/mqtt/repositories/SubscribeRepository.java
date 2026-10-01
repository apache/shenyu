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
import io.netty.handler.codec.mqtt.MqttQoS;
import io.netty.handler.codec.mqtt.MqttTopicSubscription;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Topic and channel association.
 */
public class SubscribeRepository implements BaseRepository<List<String>, Map<Channel, MqttQoS>> {

    private static final Logger LOG = LoggerFactory.getLogger(SubscribeRepository.class);

    private static final Map<String, Map<Channel, MqttQoS>> TOPIC_CHANNEL_FACTORY = new ConcurrentHashMap<>();

    @Override
    public void add(final List<String> topics, final Map<Channel, MqttQoS> channelQos) {
        CompletableFuture.runAsync(() -> topics.parallelStream().forEach(topic ->
                channelQos.forEach((channel, qos) -> TOPIC_CHANNEL_FACTORY
                        .computeIfAbsent(topic, key -> new ConcurrentHashMap<>())
                        .merge(channel, qos, SubscribeRepository::maxQoS))));
    }

    /**
     * add subscribe channel.
     * @param channel channel
     * @param mqttTopicSubscription mqtt subscription info
     */
    public void add(final Channel channel, final List<MqttTopicSubscription> mqttTopicSubscription) {
        CompletableFuture.runAsync(() -> mqttTopicSubscription.parallelStream()
                .filter(s -> s.qualityOfService() != MqttQoS.FAILURE)
                .forEach(s -> TOPIC_CHANNEL_FACTORY
                        .computeIfAbsent(s.topicName(), key -> new ConcurrentHashMap<>())
                        .merge(channel, s.qualityOfService(), SubscribeRepository::maxQoS)));
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
        CompletableFuture.runAsync(() -> topics.parallelStream().forEach(topic -> {
            Map<Channel, MqttQoS> subscribers = TOPIC_CHANNEL_FACTORY.get(topic);
            if (Objects.nonNull(subscribers)) {
                subscribers.remove(channel);
            }
        }));
    }

    /**
     * remove the channel from all topics it subscribed.
     * @param channel channel
     */
    public void remove(final Channel channel) {
        CompletableFuture.runAsync(() -> TOPIC_CHANNEL_FACTORY.values().parallelStream()
                .forEach(subscribers -> subscribers.remove(channel)));
    }

    @Override
    public Map<Channel, MqttQoS> get(final List<String> topics) {
        Map<Channel, MqttQoS> subscribers = new ConcurrentHashMap<>();
        topics.parallelStream().forEach(topic -> TOPIC_CHANNEL_FACTORY.getOrDefault(topic, Collections.emptyMap())
                .forEach((channel, qos) -> subscribers.merge(channel, qos, SubscribeRepository::maxQoS)));
        return subscribers;
    }

    /**
     * get subscriber channels with their granted qos.
     * @param topic topic
     * @return map of channel to granted qos
     */
    public Map<Channel, MqttQoS> get(final String topic) {
        return TOPIC_CHANNEL_FACTORY.getOrDefault(topic, Collections.emptyMap());
    }

    private static MqttQoS maxQoS(final MqttQoS qos1, final MqttQoS qos2) {
        return qos1.value() >= qos2.value() ? qos1 : qos2;
    }

}
