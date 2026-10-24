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
import org.apache.shenyu.protocol.mqtt.TopicMatcher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

/**
 * Topic and channel association.
 */
public class SubscribeRepository implements BaseRepository<List<String>, Map<Channel, MqttQoS>> {

    private static final Logger LOG = LoggerFactory.getLogger(SubscribeRepository.class);

    private static final Map<String, Map<Channel, MqttQoS>> TOPIC_CHANNEL_FACTORY = new ConcurrentHashMap<>();

    private static final Map<String, WildcardFilter> WILDCARD_FILTERS = new ConcurrentHashMap<>();

    private static final ReadWriteLock FILTER_LOCK = new ReentrantReadWriteLock();

    @Override
    public void add(final List<String> topics, final Map<Channel, MqttQoS> channelQos) {
        FILTER_LOCK.writeLock().lock();
        try {
            topics.forEach(topic ->
                    channelQos.forEach((channel, qos) -> addSubscription(topic, channel, qos)));
        } finally {
            FILTER_LOCK.writeLock().unlock();
        }
    }

    /**
     * add subscribe channel.
     * @param channel channel
     * @param mqttTopicSubscription mqtt subscription info
     */
    public void add(final Channel channel, final List<MqttTopicSubscription> mqttTopicSubscription) {
        FILTER_LOCK.writeLock().lock();
        try {
            // MQTT-3.8.4-3: a new subscription for the same filter replaces the previous one,
            // including its granted qos. Complete registration before returning so a later
            // disconnect cannot overtake it.
            mqttTopicSubscription.stream()
                    .filter(s -> s.qualityOfService() != MqttQoS.FAILURE)
                    .forEach(s -> addSubscription(s.topicName(), channel, s.qualityOfService()));
        } finally {
            FILTER_LOCK.writeLock().unlock();
        }
    }

    @Override
    public void remove(final List<String> topics) {
        FILTER_LOCK.writeLock().lock();
        try {
            topics.forEach(topic -> {
                TOPIC_CHANNEL_FACTORY.remove(topic);
                WILDCARD_FILTERS.remove(topic);
            });
        } finally {
            FILTER_LOCK.writeLock().unlock();
        }
    }

    /**
     * remove subscribe channel.
     * @param topics topics
     * @param channel channel
     */
    public void remove(final List<String> topics, final Channel channel) {
        FILTER_LOCK.writeLock().lock();
        try {
            topics.forEach(topic -> removeChannel(topic, channel));
        } finally {
            FILTER_LOCK.writeLock().unlock();
        }
    }

    /**
     * remove the channel from all topics it subscribed.
     * @param channel channel
     */
    public void remove(final Channel channel) {
        FILTER_LOCK.writeLock().lock();
        try {
            TOPIC_CHANNEL_FACTORY.forEach((filter, subscribers) -> {
                subscribers.remove(channel);
                if (subscribers.isEmpty()) {
                    TOPIC_CHANNEL_FACTORY.remove(filter, subscribers);
                    WILDCARD_FILTERS.remove(filter);
                }
            });
        } finally {
            FILTER_LOCK.writeLock().unlock();
        }
    }

    @Override
    public Map<Channel, MqttQoS> get(final List<String> topics) {
        FILTER_LOCK.readLock().lock();
        try {
            Map<Channel, MqttQoS> subscribers = new ConcurrentHashMap<>();
            topics.forEach(topic -> TOPIC_CHANNEL_FACTORY.getOrDefault(topic, Collections.emptyMap())
                    .forEach((channel, qos) -> subscribers.merge(channel, qos, SubscribeRepository::maxQoS)));
            return subscribers;
        } finally {
            FILTER_LOCK.readLock().unlock();
        }
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

    /**
     * Get the channels whose subscription filter matches the published topic,
     * mapped to the maximum qos granted across all their matching filters.
     * Supports MQTT wildcards: + (single-level) and # (multi-level).
     *
     * @param topic the published topic name
     * @return matching channels with their maximum granted qos
     */
    public Map<Channel, MqttQoS> getChannelsByTopic(final String topic) {
        FILTER_LOCK.readLock().lock();
        try {
            // MQTT requires at most one delivery per publish per client, so merge the
            // granted qos when overlapping filters (e.g. sport/# and #) both match.
            Map<Channel, MqttQoS> result = new ConcurrentHashMap<>();

            // fast path: exact subscription, no wildcard scan needed
            Map<Channel, MqttQoS> exactMatch = TOPIC_CHANNEL_FACTORY.get(topic);
            if (Objects.nonNull(exactMatch)) {
                result.putAll(exactMatch);
            }

            String[] topicLevels = topic.split("/", -1);
            WILDCARD_FILTERS.forEach((filter, wildcardFilter) -> {
                if (wildcardFilter.matches(topicLevels)) {
                    Map<Channel, MqttQoS> subscribers = TOPIC_CHANNEL_FACTORY.get(filter);
                    if (Objects.nonNull(subscribers)) {
                        subscribers.forEach((channel, qos) -> result.merge(channel, qos, SubscribeRepository::maxQoS));
                    }
                }
            });
            return result;
        } finally {
            FILTER_LOCK.readLock().unlock();
        }
    }

    private static void addSubscription(final String filter, final Channel channel, final MqttQoS qos) {
        TOPIC_CHANNEL_FACTORY.computeIfAbsent(filter, key -> new ConcurrentHashMap<>()).put(channel, qos);
        if (!WILDCARD_FILTERS.containsKey(filter)
                && (filter.indexOf('+') >= 0 || filter.indexOf('#') >= 0)
                && TopicMatcher.isValidFilter(filter)) {
            WILDCARD_FILTERS.put(filter, new WildcardFilter(filter));
        }
    }

    private static void removeChannel(final String topic, final Channel channel) {
        Map<Channel, MqttQoS> subscribers = TOPIC_CHANNEL_FACTORY.get(topic);
        if (Objects.nonNull(subscribers)) {
            subscribers.remove(channel);
            if (subscribers.isEmpty()) {
                TOPIC_CHANNEL_FACTORY.remove(topic, subscribers);
                WILDCARD_FILTERS.remove(topic);
            }
        }
    }

    private static final class WildcardFilter {

        private final String[] levels;

        private WildcardFilter(final String filter) {
            this.levels = filter.split("/", -1);
        }

        private boolean matches(final String[] topicLevels) {
            return TopicMatcher.matches(levels, topicLevels);
        }
    }

}
