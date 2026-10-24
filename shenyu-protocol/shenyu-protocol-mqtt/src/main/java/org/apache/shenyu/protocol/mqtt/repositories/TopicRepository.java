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

import io.netty.handler.codec.mqtt.MqttQoS;
import org.apache.shenyu.protocol.mqtt.TopicMatcher;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Topic repository.
 * Save the posted message.
 * {@link org.apache.shenyu.protocol.mqtt.agent.MessageAgent}
 */
public class TopicRepository implements BaseRepository<String, byte[]> {

    private static final Map<String, RetainedMessage> TOPIC_FACTORY = new ConcurrentHashMap<>();

    @Override
    public void add(final String topic, final byte[] message) {
        add(topic, message, MqttQoS.AT_MOST_ONCE);
    }

    /**
     * Store a retained message and the qos of the publish that produced it.
     *
     * @param topic the topic of the retained message
     * @param message the retained payload
     * @param qos the qos of the original publish
     */
    public void add(final String topic, final byte[] message, final MqttQoS qos) {
        TOPIC_FACTORY.put(topic, new RetainedMessage(message, qos));
    }

    @Override
    public void remove(final String topic) {
        TOPIC_FACTORY.remove(topic);
    }

    @Override
    public byte[] get(final String topic) {
        RetainedMessage message = TOPIC_FACTORY.get(topic);
        return Objects.isNull(message) ? null : message.getPayload();
    }

    /**
     * Get the retained messages whose concrete topic names match the given topic filter,
     * honoring MQTT wildcards (+ single-level and # multi-level).
     *
     * @param topicFilter the subscription topic filter
     * @return the matching retained messages keyed by their concrete topic name
     */
    public Map<String, RetainedMessage> getMatchingRetainedMessages(final String topicFilter) {
        Map<String, RetainedMessage> matchingTopics = new ConcurrentHashMap<>();
        TOPIC_FACTORY.forEach((topic, message) -> {
            if (TopicMatcher.matches(topicFilter, topic)) {
                matchingTopics.put(topic, message);
            }
        });
        return matchingTopics;
    }

}
