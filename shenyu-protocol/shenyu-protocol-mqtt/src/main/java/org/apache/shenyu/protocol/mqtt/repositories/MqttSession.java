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
import io.netty.handler.codec.mqtt.MqttTopicSubscription;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * MQTT session state stored for a client.
 */
public class MqttSession {

    private final String clientId;

    private final boolean cleanSession;

    private final Map<String, MqttQoS> topics = new ConcurrentHashMap<>();

    /**
     * MqttSession constructor.
     * @param clientId clientId
     * @param cleanSession cleanSession
     */
    public MqttSession(final String clientId, final boolean cleanSession) {
        this.clientId = clientId;
        this.cleanSession = cleanSession;
    }

    /**
     * get clientId.
     * @return clientId
     */
    public String getClientId() {
        return clientId;
    }

    /**
     * get cleanSession.
     * @return cleanSession
     */
    public boolean isCleanSession() {
        return cleanSession;
    }

    /**
     * add topic with its QoS, replacing any existing subscription for the same topic.
     * @param topic topic
     * @param qos qos
     * @return true if the topic was newly added
     */
    public boolean addTopic(final String topic, final MqttQoS qos) {
        return Objects.isNull(topics.put(topic, qos));
    }

    /**
     * remove topic.
     * @param topic topic
     * @return true if the topic was removed
     */
    public boolean removeTopic(final String topic) {
        return Objects.nonNull(topics.remove(topic));
    }

    /**
     * get topics.
     * @return topics
     */
    public Set<String> getTopics() {
        return topics.keySet();
    }

    /**
     * get topic subscriptions with their QoS for session resume.
     * @return topic subscriptions
     */
    public List<MqttTopicSubscription> getTopicSubscriptions() {
        return topics.entrySet().stream()
                .map(entry -> new MqttTopicSubscription(entry.getKey(), entry.getValue()))
                .collect(Collectors.toList());
    }
}
