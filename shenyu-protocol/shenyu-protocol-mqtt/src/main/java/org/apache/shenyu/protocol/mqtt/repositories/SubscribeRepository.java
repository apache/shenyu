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
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.protocol.mqtt.TopicMatcher;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * Topic and channel association.
 *
 * <p>Subscription updates are applied synchronously on the calling (event loop) thread and every
 * topic holds a copy-on-write list of channels, so a subscription is visible to publish as soon as
 * {@code add} returns and concurrent subscribers of the same topic never overwrite each other.
 */
public class SubscribeRepository implements BaseRepository<List<String>, List<Channel>> {

    private static final Map<String, List<Channel>> TOPIC_CHANNEL_FACTORY = new ConcurrentHashMap<>();

    @Override
    public void add(final List<String> topics, final List<Channel> channels) {
        topics.forEach(topic -> TOPIC_CHANNEL_FACTORY
                .computeIfAbsent(topic, key -> new CopyOnWriteArrayList<>())
                .addAll(channels));
    }

    /**
     * add subscribe channel.
     * @param channel channel
     * @param mqttTopicSubscription mqtt subscription info
     */
    public void add(final Channel channel, final List<MqttTopicSubscription> mqttTopicSubscription) {
        mqttTopicSubscription.forEach(subscription -> TOPIC_CHANNEL_FACTORY
                .computeIfAbsent(subscription.topicName(), key -> new CopyOnWriteArrayList<>())
                .add(channel));
    }

    @Override
    public void remove(final List<String> topics) {
        topics.forEach(TOPIC_CHANNEL_FACTORY::remove);
    }

    /**
     * remove subscribe channel.
     * @param topics topics
     * @param channel channel
     */
    public void remove(final List<String> topics, final Channel channel) {
        topics.forEach(topic -> {
            List<Channel> channels = TOPIC_CHANNEL_FACTORY.get(topic);
            if (CollectionUtils.isNotEmpty(channels)) {
                channels.remove(channel);
            }
        });
    }

    @Override
    public List<Channel> get(final List<String> topics) {
        Set<Channel> channels = new LinkedHashSet<>();
        topics.forEach(topic -> channels.addAll(TOPIC_CHANNEL_FACTORY.getOrDefault(topic, Collections.emptyList())));
        return new ArrayList<>(channels);
    }

    /**
     * get Channels.
     * @param topic topic
     * @return Channels
     */
    public List<Channel> get(final String topic) {
        return TOPIC_CHANNEL_FACTORY.getOrDefault(topic, new CopyOnWriteArrayList<>());
    }

    /**
     * Get channels whose subscription filter matches the published topic.
     * Supports MQTT wildcards: + (single-level) and # (multi-level).
     *
     * @param topic the published topic name
     * @return channels subscribed to matching topic filters
     */
    public List<Channel> getChannelsByTopic(final String topic) {
        // MQTT requires at most one delivery per publish per client, so dedupe
        // channels when overlapping filters (e.g. sport/# and #) both match.
        Set<Channel> result = new LinkedHashSet<>();

        // fast path: exact subscription, no wildcard scan needed
        List<Channel> exactMatch = TOPIC_CHANNEL_FACTORY.get(topic);
        if (Objects.nonNull(exactMatch)) {
            result.addAll(exactMatch);
        }

        for (Map.Entry<String, List<Channel>> entry : TOPIC_CHANNEL_FACTORY.entrySet()) {
            String filter = entry.getKey();
            if (filter.equals(topic) || filter.indexOf('+') < 0 && filter.indexOf('#') < 0) {
                continue;
            }
            if (TopicMatcher.matches(filter, topic)) {
                result.addAll(entry.getValue());
            }
        }
        return new ArrayList<>(result);
    }

}
