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
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link SubscribeRepository}.
 */
public final class SubscribeRepositoryTest {

    private static final String EXISTING_TOPIC = "test/existing-topic";

    private static final String ABSENT_TOPIC = "test/absent-topic";

    private static final String KEPT_TOPIC = "test/kept-topic";

    private static final long JOIN_TIMEOUT_MILLIS = 5000L;

    private final SubscribeRepository repository = new SubscribeRepository();

    private final List<Channel> channels = new ArrayList<>();

    private final Set<String> subscribedTopics = new LinkedHashSet<>();

    @AfterEach
    public void tearDown() {
        // subscriptions live in a static map shared with the other test classes,
        // so every topic has to be empty before the next test runs.
        channels.forEach(channel -> subscribedTopics.forEach(
                topic -> repository.remove(Collections.singletonList(topic), channel)));
        subscribedTopics.forEach(topic -> assertTrue(repository.get(topic).isEmpty()));
        channels.forEach(channel -> ((EmbeddedChannel) channel).finishAndReleaseAll());
    }

    @Test
    public void removeRemovesChannelFromExistingTopic() {
        Channel channel = newSubscriber(EXISTING_TOPIC);

        repository.remove(Collections.singletonList(EXISTING_TOPIC), channel);

        assertTrue(repository.get(EXISTING_TOPIC).isEmpty());
    }

    @Test
    public void removeAbsentTopicDoesNotThrow() {
        Channel channel = newSubscriber(KEPT_TOPIC);

        assertDoesNotThrow(() -> repository.remove(Collections.singletonList(ABSENT_TOPIC), channel));

        assertTrue(repository.get(ABSENT_TOPIC).isEmpty());
        assertTrue(repository.get(KEPT_TOPIC).contains(channel));
    }

    @Test
    public void testGetChannelsByTopicExactMatch() {
        Channel subscriber = newSubscriber("sport/tennis");

        assertTrue(repository.getChannelsByTopic("sport/tennis").contains(subscriber));
        assertTrue(repository.getChannelsByTopic("sport/tennis/player1").isEmpty());
    }

    @Test
    public void testGetChannelsByTopicWildcardMatch() {
        Channel subscriber = newSubscriber("sport/+/player1");

        assertTrue(repository.getChannelsByTopic("sport/tennis/player1").contains(subscriber));
        assertTrue(repository.getChannelsByTopic("sport/tennis").isEmpty());
    }

    @Test
    public void testGetChannelsByTopicDeduplicatesOverlappingSubscriptions() {
        Channel subscriber = newSubscriber("#", "sport/#");

        // MQTT requires at most one delivery per publish per client
        assertEquals(1, repository.getChannelsByTopic("sport/tennis").size());
        assertTrue(repository.getChannelsByTopic("sport/tennis").contains(subscriber));
    }

    @Test
    public void testGetChannelsByTopicMultipleSubscribers() {
        Channel first = newSubscriber("sport/tennis");
        Channel second = newSubscriber("sport/tennis");

        assertEquals(2, repository.getChannelsByTopic("sport/tennis").size());
        assertTrue(repository.getChannelsByTopic("sport/tennis").containsAll(Arrays.asList(first, second)));
    }

    /**
     * Regression guard: subscribing to a brand new topic must keep every client
     * when several clients send SUBSCRIBE at the same time.
     */
    @Test
    public void concurrentSubscribersOfTheSameNewTopicAreAllRegistered() throws InterruptedException {
        int subscriberCount = 8;
        String topic = "sport/concurrent";
        subscribedTopics.add(topic);
        CountDownLatch startGate = new CountDownLatch(1);
        List<Thread> subscribingThreads = new ArrayList<>();
        for (int i = 0; i < subscriberCount; i++) {
            EmbeddedChannel subscriber = new EmbeddedChannel();
            channels.add(subscriber);
            Thread thread = new Thread(() -> subscribeAfter(startGate, subscriber, topic));
            thread.start();
            subscribingThreads.add(thread);
        }

        startGate.countDown();
        for (Thread thread : subscribingThreads) {
            thread.join(JOIN_TIMEOUT_MILLIS);
            assertFalse(thread.isAlive(), "subscribing thread did not finish in time");
        }

        assertEquals(subscriberCount, repository.get(topic).size());
        assertEquals(subscriberCount, repository.getChannelsByTopic(topic).size());
    }

    private Channel newSubscriber(final String... topics) {
        Channel subscriber = new EmbeddedChannel();
        channels.add(subscriber);
        subscribedTopics.addAll(Arrays.asList(topics));
        repository.add(subscriber, subscriptions(topics));
        return subscriber;
    }

    private List<MqttTopicSubscription> subscriptions(final String... topics) {
        return Arrays.stream(topics)
                .map(topic -> new MqttTopicSubscription(topic, MqttQoS.AT_MOST_ONCE))
                .collect(Collectors.toList());
    }

    private void subscribeAfter(final CountDownLatch startGate, final Channel subscriber, final String topic) {
        try {
            startGate.await();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return;
        }
        repository.add(subscriber, subscriptions(topic));
    }
}
