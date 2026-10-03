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
import org.apache.shenyu.common.utils.Singleton;
import org.awaitility.core.ThrowingRunnable;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.TimeUnit;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

/**
 * Test cases for {@link SubscribeRepository}.
 */
public final class SubscribeRepositoryTest {

    private static final String ABSENT_TOPIC = "test/absent-topic";

    private static final String TOPIC = "test/topic";

    private static final String OTHER_TOPIC = "test/other-topic";

    private static final String EXACT_TOPIC = "sport/tennis";

    private static final String CHILD_TOPIC = "sport/tennis/player1";

    private static final String SINGLE_LEVEL_FILTER = "sport/+/player1";

    private static final String MULTI_LEVEL_FILTER = "sport/#";

    private static final String MATCH_ALL_FILTER = "#";

    private static final String CONCURRENT_TOPIC = "sport/concurrent";

    private static final List<String> ALL_TOPICS = Arrays.asList(
            ABSENT_TOPIC, TOPIC, OTHER_TOPIC,
            EXACT_TOPIC, SINGLE_LEVEL_FILTER, MULTI_LEVEL_FILTER, MATCH_ALL_FILTER, CONCURRENT_TOPIC);

    private static final Duration TIMEOUT = Duration.ofSeconds(5);

    private static final Duration POLL_INTERVAL = Duration.ofMillis(10);

    private static final long JOIN_TIMEOUT_MILLIS = 5000L;

    private SubscribeRepository repository;

    private Channel channel;

    private Channel otherChannel;

    @BeforeEach
    public void setUp() {
        repository = new SubscribeRepository();
        channel = mock(Channel.class);
        otherChannel = mock(Channel.class);
        Singleton.INST.single(SubscribeRepository.class, repository);
        clearAllTopics();
    }

    @AfterEach
    public void tearDown() {
        clearAllTopics();
    }

    @Test
    public void testAddStoresGrantedQosPerTopic() {
        repository.add(channel, Collections.singletonList(new MqttTopicSubscription(TOPIC, MqttQoS.AT_LEAST_ONCE)));
        awaitAssert(() -> assertEquals(MqttQoS.AT_LEAST_ONCE, repository.get(TOPIC).get(channel)));
    }

    @Test
    public void testAddRegistersEverySubscribedTopic() {
        repository.add(channel, Arrays.asList(
                new MqttTopicSubscription(TOPIC, MqttQoS.AT_MOST_ONCE),
                new MqttTopicSubscription(OTHER_TOPIC, MqttQoS.EXACTLY_ONCE)));
        awaitAssert(() -> {
            assertEquals(MqttQoS.AT_MOST_ONCE, repository.get(TOPIC).get(channel));
            assertEquals(MqttQoS.EXACTLY_ONCE, repository.get(OTHER_TOPIC).get(channel));
        });
    }

    @Test
    public void testAddKeepsMaxQosForOverlappingSubscription() {
        repository.add(channel, Arrays.asList(
                new MqttTopicSubscription(TOPIC, MqttQoS.AT_LEAST_ONCE),
                new MqttTopicSubscription(TOPIC, MqttQoS.EXACTLY_ONCE)));
        awaitAssert(() -> assertEquals(MqttQoS.EXACTLY_ONCE, repository.get(TOPIC).get(channel)));

        repository.add(channel, Collections.singletonList(new MqttTopicSubscription(TOPIC, MqttQoS.AT_MOST_ONCE)));
        awaitRepositoryIdle();
        assertEquals(MqttQoS.EXACTLY_ONCE, repository.get(TOPIC).get(channel));
    }

    @Test
    public void testAddIgnoresFailureSubscription() {
        repository.add(channel, Collections.singletonList(new MqttTopicSubscription(TOPIC, MqttQoS.FAILURE)));
        awaitRepositoryIdle();
        assertTrue(repository.get(TOPIC).isEmpty());
        assertTrue(repository.get(ALL_TOPICS).isEmpty());
    }

    @Test
    public void testAddTopicsWithChannelQosMap() {
        Map<Channel, MqttQoS> channelQos = new ConcurrentHashMap<>();
        channelQos.put(channel, MqttQoS.AT_MOST_ONCE);
        channelQos.put(otherChannel, MqttQoS.EXACTLY_ONCE);
        repository.add(Arrays.asList(TOPIC, OTHER_TOPIC), channelQos);
        awaitAssert(() -> {
            assertEquals(MqttQoS.AT_MOST_ONCE, repository.get(TOPIC).get(channel));
            assertEquals(MqttQoS.EXACTLY_ONCE, repository.get(TOPIC).get(otherChannel));
            assertEquals(MqttQoS.AT_MOST_ONCE, repository.get(OTHER_TOPIC).get(channel));
            assertEquals(MqttQoS.EXACTLY_ONCE, repository.get(OTHER_TOPIC).get(otherChannel));
        });
    }

    @Test
    public void testGetMergesSubscribersOfEveryTopic() {
        repository.add(channel, Collections.singletonList(new MqttTopicSubscription(TOPIC, MqttQoS.AT_MOST_ONCE)));
        repository.add(channel, Collections.singletonList(new MqttTopicSubscription(OTHER_TOPIC, MqttQoS.EXACTLY_ONCE)));
        repository.add(otherChannel, Collections.singletonList(new MqttTopicSubscription(OTHER_TOPIC, MqttQoS.AT_LEAST_ONCE)));
        awaitAssert(() -> {
            Map<Channel, MqttQoS> subscribers = repository.get(Arrays.asList(TOPIC, OTHER_TOPIC));
            assertEquals(2, subscribers.size());
            assertEquals(MqttQoS.EXACTLY_ONCE, subscribers.get(channel));
            assertEquals(MqttQoS.AT_LEAST_ONCE, subscribers.get(otherChannel));
        });
    }

    @Test
    public void testGetAbsentTopicReturnsNoSubscribers() {
        assertTrue(repository.get(Collections.singletonList(ABSENT_TOPIC)).isEmpty());
    }

    @Test
    public void testRemoveChannelFromTopic() {
        repository.add(channel, Arrays.asList(
                new MqttTopicSubscription(TOPIC, MqttQoS.AT_MOST_ONCE),
                new MqttTopicSubscription(OTHER_TOPIC, MqttQoS.AT_MOST_ONCE)));
        repository.add(otherChannel, Collections.singletonList(new MqttTopicSubscription(TOPIC, MqttQoS.AT_LEAST_ONCE)));
        awaitAssert(() -> {
            assertEquals(MqttQoS.AT_MOST_ONCE, repository.get(TOPIC).get(channel));
            assertEquals(MqttQoS.AT_LEAST_ONCE, repository.get(TOPIC).get(otherChannel));
        });

        repository.remove(Collections.singletonList(TOPIC), channel);

        awaitAssert(() -> {
            assertFalse(repository.get(TOPIC).containsKey(channel));
            assertEquals(MqttQoS.AT_LEAST_ONCE, repository.get(TOPIC).get(otherChannel));
            assertEquals(MqttQoS.AT_MOST_ONCE, repository.get(OTHER_TOPIC).get(channel));
        });
    }

    @Test
    public void testRemoveChannelFromEveryTopic() {
        repository.add(channel, Arrays.asList(
                new MqttTopicSubscription(TOPIC, MqttQoS.AT_MOST_ONCE),
                new MqttTopicSubscription(OTHER_TOPIC, MqttQoS.AT_MOST_ONCE)));
        repository.add(otherChannel, Collections.singletonList(new MqttTopicSubscription(TOPIC, MqttQoS.AT_LEAST_ONCE)));
        awaitAssert(() -> assertEquals(MqttQoS.AT_LEAST_ONCE, repository.get(TOPIC).get(otherChannel)));

        repository.remove(channel);

        awaitAssert(() -> {
            assertFalse(repository.get(TOPIC).containsKey(channel));
            assertTrue(repository.get(OTHER_TOPIC).isEmpty());
            assertEquals(MqttQoS.AT_LEAST_ONCE, repository.get(TOPIC).get(otherChannel));
        });
    }

    @Test
    public void testRemoveAbsentTopicDoesNotThrow() {
        repository.add(channel, Collections.singletonList(new MqttTopicSubscription(TOPIC, MqttQoS.AT_MOST_ONCE)));
        awaitAssert(() -> assertEquals(MqttQoS.AT_MOST_ONCE, repository.get(TOPIC).get(channel)));

        assertDoesNotThrow(() -> repository.remove(Collections.singletonList(ABSENT_TOPIC), channel));
        awaitRepositoryIdle();

        assertTrue(repository.get(ABSENT_TOPIC).isEmpty());
        assertEquals(MqttQoS.AT_MOST_ONCE, repository.get(TOPIC).get(channel));
    }

    @Test
    public void testGetChannelsByTopicExactMatch() {
        repository.add(channel, Collections.singletonList(new MqttTopicSubscription(EXACT_TOPIC, MqttQoS.AT_MOST_ONCE)));
        awaitAssert(() -> assertTrue(repository.getChannelsByTopic(EXACT_TOPIC).containsKey(channel)));
        assertTrue(repository.getChannelsByTopic(CHILD_TOPIC).isEmpty());
    }

    @Test
    public void testGetChannelsByTopicWildcardMatch() {
        repository.add(channel, Collections.singletonList(new MqttTopicSubscription(SINGLE_LEVEL_FILTER, MqttQoS.AT_MOST_ONCE)));
        awaitAssert(() -> assertTrue(repository.getChannelsByTopic(CHILD_TOPIC).containsKey(channel)));
        assertTrue(repository.getChannelsByTopic(EXACT_TOPIC).isEmpty());
    }

    @Test
    public void testGetChannelsByTopicDeduplicatesOverlappingSubscriptions() {
        repository.add(channel, Arrays.asList(
                new MqttTopicSubscription(MATCH_ALL_FILTER, MqttQoS.AT_MOST_ONCE),
                new MqttTopicSubscription(MULTI_LEVEL_FILTER, MqttQoS.EXACTLY_ONCE)));
        awaitAssert(() -> {
            assertTrue(repository.get(MATCH_ALL_FILTER).containsKey(channel));
            assertTrue(repository.get(MULTI_LEVEL_FILTER).containsKey(channel));
        });

        // MQTT requires at most one delivery per publish per client, at the maximum qos of the matching filters
        Map<Channel, MqttQoS> matched = repository.getChannelsByTopic(EXACT_TOPIC);
        assertEquals(1, matched.size());
        assertEquals(MqttQoS.EXACTLY_ONCE, matched.get(channel));
    }

    @Test
    public void testGetChannelsByTopicMultipleSubscribers() {
        repository.add(channel, Collections.singletonList(new MqttTopicSubscription(EXACT_TOPIC, MqttQoS.AT_MOST_ONCE)));
        repository.add(otherChannel, Collections.singletonList(new MqttTopicSubscription(EXACT_TOPIC, MqttQoS.AT_MOST_ONCE)));
        awaitAssert(() -> assertEquals(2, repository.getChannelsByTopic(EXACT_TOPIC).size()));
        assertTrue(repository.getChannelsByTopic(EXACT_TOPIC).keySet().containsAll(Arrays.asList(channel, otherChannel)));
    }

    /**
     * Regression guard: subscribing to a brand new topic must keep every client
     * when several clients send SUBSCRIBE at the same time.
     *
     * @throws InterruptedException if a subscribing thread is interrupted
     */
    @Test
    public void concurrentSubscribersOfTheSameNewTopicAreAllRegistered() throws InterruptedException {
        int subscriberCount = 8;
        CountDownLatch startGate = new CountDownLatch(1);
        List<Channel> subscribers = new ArrayList<>();
        List<Thread> subscribingThreads = new ArrayList<>();
        for (int i = 0; i < subscriberCount; i++) {
            Channel subscriber = mock(Channel.class);
            subscribers.add(subscriber);
            Thread thread = new Thread(() -> subscribeAfter(startGate, subscriber, CONCURRENT_TOPIC));
            thread.start();
            subscribingThreads.add(thread);
        }

        startGate.countDown();
        for (Thread thread : subscribingThreads) {
            thread.join(JOIN_TIMEOUT_MILLIS);
            assertFalse(thread.isAlive(), "subscribing thread did not finish in time");
        }

        awaitAssert(() -> assertEquals(subscriberCount, repository.get(CONCURRENT_TOPIC).size()));
        Map<Channel, MqttQoS> matched = repository.getChannelsByTopic(CONCURRENT_TOPIC);
        assertEquals(subscriberCount, matched.size());
        assertTrue(matched.keySet().containsAll(subscribers));
    }

    private void subscribeAfter(final CountDownLatch startGate, final Channel subscriber, final String topic) {
        try {
            startGate.await();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return;
        }
        repository.add(subscriber, Collections.singletonList(new MqttTopicSubscription(topic, MqttQoS.AT_MOST_ONCE)));
    }

    /**
     * The repository mutates its state asynchronously on the common pool,
     * so assertions have to be retried until the mutation becomes visible.
     *
     * @param assertion assertion to retry
     */
    private void awaitAssert(final ThrowingRunnable assertion) {
        await().atMost(TIMEOUT).pollInterval(POLL_INTERVAL).untilAsserted(assertion);
    }

    /**
     * Waits until the repository finished all pending asynchronous mutations.
     * Required to assert that a mutation did <em>not</em> change the shared state.
     */
    private void awaitRepositoryIdle() {
        assertTrue(ForkJoinPool.commonPool().awaitQuiescence(TIMEOUT.toMillis(), TimeUnit.MILLISECONDS));
    }

    /**
     * The repository keeps its state in a static map which is shared by every instance and
     * by the other test classes of this module, so the topics used here are released around every test.
     */
    private void clearAllTopics() {
        repository.remove(ALL_TOPICS);
        awaitAssert(() -> ALL_TOPICS.forEach(topic -> assertTrue(repository.get(topic).isEmpty())));
    }
}
