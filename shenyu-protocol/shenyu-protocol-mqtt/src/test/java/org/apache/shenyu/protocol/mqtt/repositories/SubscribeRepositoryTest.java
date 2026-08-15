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
import io.netty.channel.embedded.EmbeddedChannel;
import io.netty.handler.codec.mqtt.MqttQoS;
import io.netty.handler.codec.mqtt.MqttTopicSubscription;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.BooleanSupplier;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

class SubscribeRepositoryTest {

    private final SubscribeRepository repository = new SubscribeRepository();

    private final List<Channel> channels = new ArrayList<>();

    private final List<String> topicFilters = new ArrayList<>();

    @AfterEach
    void cleanup() throws InterruptedException {
        if (!topicFilters.isEmpty()) {
            for (Channel subscribed : channels) {
                repository.remove(topicFilters, subscribed);
            }
            awaitUntil(() -> topicFilters.stream().allMatch(topic -> repository.get(topic).isEmpty()));
        }
        channels.forEach(channel -> ((EmbeddedChannel) channel).finishAndReleaseAll());
    }

    @Test
    void testGetChannelsByTopicExactMatch() throws InterruptedException {
        Channel subscriber = newSubscriber("sport/tennis");

        assertTrue(repository.getChannelsByTopic("sport/tennis").contains(subscriber));
        assertTrue(repository.getChannelsByTopic("sport/tennis/player1").isEmpty());
    }

    @Test
    void testGetChannelsByTopicWildcardMatch() throws InterruptedException {
        Channel subscriber = newSubscriber("sport/+/player1");

        assertTrue(repository.getChannelsByTopic("sport/tennis/player1").contains(subscriber));
        assertTrue(repository.getChannelsByTopic("sport/tennis").isEmpty());
    }

    @Test
    void testGetChannelsByTopicDeduplicatesOverlappingSubscriptions() throws InterruptedException {
        Channel subscriber = newSubscriber("#", "sport/#");

        // MQTT requires at most one delivery per publish per client
        assertEquals(1, repository.getChannelsByTopic("sport/tennis").size());
        assertTrue(repository.getChannelsByTopic("sport/tennis").contains(subscriber));
    }

    @Test
    void testGetChannelsByTopicMultipleSubscribers() throws InterruptedException {
        final Channel first = newSubscriber("sport/tennis");
        final Channel second = new EmbeddedChannel();
        channels.add(second);
        topicFilters.add("sport/tennis");
        repository.add(second, subscriptions("sport/tennis"));
        awaitUntil(() -> repository.get("sport/tennis").containsAll(Arrays.asList(first, second)));

        assertEquals(2, repository.getChannelsByTopic("sport/tennis").size());
    }

    private Channel newSubscriber(final String... topics) throws InterruptedException {
        Channel subscriber = new EmbeddedChannel();
        channels.add(subscriber);
        topicFilters.addAll(Arrays.asList(topics));
        repository.add(subscriber, subscriptions(topics));
        awaitUntil(() -> Arrays.stream(topics).allMatch(topic -> repository.get(topic).contains(subscriber)));
        return subscriber;
    }

    private List<MqttTopicSubscription> subscriptions(final String... topics) {
        return Arrays.stream(topics)
                .map(topic -> new MqttTopicSubscription(topic, MqttQoS.AT_MOST_ONCE))
                .collect(Collectors.toList());
    }

    private void awaitUntil(final BooleanSupplier condition) throws InterruptedException {
        long deadline = System.currentTimeMillis() + 5000;
        while (!condition.getAsBoolean()) {
            if (System.currentTimeMillis() >= deadline) {
                fail("condition not met within timeout");
            }
            Thread.sleep(10);
        }
    }
}
