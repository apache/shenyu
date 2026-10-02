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
import org.apache.shenyu.common.utils.Singleton;
import org.awaitility.core.ThrowingRunnable;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.time.Duration;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

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

    private static final Duration TIMEOUT = Duration.ofSeconds(5);

    private static final Duration POLL_INTERVAL = Duration.ofMillis(10);

    private static final String EXISTING_TOPIC = "test/existing-topic";

    private static final String KEPT_TOPIC = "test/kept-topic";

    private static final String SPORT_TOPIC = "test/sport/tennis";

    private static final String PLAYER_TOPIC = "test/sport/tennis/player1";

    private static final String SINGLE_LEVEL_FILTER = "test/sport/+/player1";

    private static final String MULTI_LEVEL_FILTER = "test/sport/#";

    private static final String MATCH_ALL_FILTER = "test/#";

    /**
     * The topics and topic filters used by these tests. The repository keeps its subscriptions in a static
     * map shared with the other test classes of this module, so they are released around every test to keep
     * the tests independent of each other.
     */
    private static final List<String> ALL_TOPICS = Arrays.asList(EXISTING_TOPIC, ABSENT_TOPIC, KEPT_TOPIC,
            SPORT_TOPIC, PLAYER_TOPIC, SINGLE_LEVEL_FILTER, MULTI_LEVEL_FILTER, MATCH_ALL_FILTER, TOPIC, OTHER_TOPIC);

    private final SubscribeRepository repository = new SubscribeRepository();

    private final List<EmbeddedChannel> subscribers = new ArrayList<>();

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
     * The repository keeps its state in a static map which is shared by every instance and
     * by the other test classes of this module, so the topics used here are released around every test.
     */
    private void clearAllTopics() {
        repository.remove(ALL_TOPICS);
        awaitAssert(() -> ALL_TOPICS.forEach(topic -> assertTrue(repository.get(topic).isEmpty())));
    }

    @BeforeEach
    void releaseTopicsBeforeTest() {
        clearTopics();
    }

    @AfterEach
    void releaseTopicsAndSubscribersAfterTest() {
        clearTopics();
        subscribers.forEach(EmbeddedChannel::finishAndReleaseAll);
        subscribers.clear();
    }

    @Test
    void removeRemovesChannelFromExistingTopic() {
        EmbeddedChannel subscriber = subscribe(EXISTING_TOPIC);

        repository.remove(Collections.singletonList(EXISTING_TOPIC), subscriber);

        awaitAssert(() -> assertTrue(repository.get(EXISTING_TOPIC).isEmpty()));
    }

    @Test
    void removeAbsentTopicDoesNotThrow() {
        EmbeddedChannel subscriber = subscribe(KEPT_TOPIC);

        assertDoesNotThrow(() -> repository.remove(Collections.singletonList(ABSENT_TOPIC), subscriber));
        awaitRepositoryIdle();

        assertTrue(repository.get(ABSENT_TOPIC).isEmpty());
        assertTrue(repository.get(KEPT_TOPIC).contains(subscriber));
    }

    @Test
    void testGetChannelsByTopicExactMatch() {
        EmbeddedChannel subscriber = subscribe(SPORT_TOPIC);

        assertEquals(Collections.singletonList(subscriber), repository.getChannelsByTopic(SPORT_TOPIC));
        assertTrue(repository.getChannelsByTopic(PLAYER_TOPIC).isEmpty());
    }

    @Test
    void testGetChannelsByTopicWildcardMatch() {
        EmbeddedChannel subscriber = subscribe(SINGLE_LEVEL_FILTER);

        assertEquals(Collections.singletonList(subscriber), repository.getChannelsByTopic(PLAYER_TOPIC));
        assertTrue(repository.getChannelsByTopic(SPORT_TOPIC).isEmpty());
    }

    @Test
    void testGetChannelsByTopicMultiLevelWildcardMatch() {
        EmbeddedChannel subscriber = subscribe(MULTI_LEVEL_FILTER);

        assertTrue(repository.getChannelsByTopic(SPORT_TOPIC).contains(subscriber));
        assertTrue(repository.getChannelsByTopic(PLAYER_TOPIC).contains(subscriber));
    }

    @Test
    void testGetChannelsByTopicDeduplicatesOverlappingSubscriptions() {
        EmbeddedChannel subscriber = subscribe(MATCH_ALL_FILTER, MULTI_LEVEL_FILTER);

        // MQTT requires at most one delivery per publish per client
        assertEquals(Collections.singletonList(subscriber), repository.getChannelsByTopic(SPORT_TOPIC));
    }

    @Test
    void testGetChannelsByTopicMultipleSubscribers() {
        EmbeddedChannel first = subscribe(SPORT_TOPIC);
        EmbeddedChannel second = subscribe(SPORT_TOPIC);

        List<Channel> matched = repository.getChannelsByTopic(SPORT_TOPIC);
        assertEquals(2, matched.size());
        assertTrue(matched.containsAll(Arrays.asList(first, second)));
    }

    @Test
    void testGetChannelsByTopicIgnoresUnsubscribedChannel() {
        EmbeddedChannel subscriber = subscribe(MULTI_LEVEL_FILTER);

        repository.remove(Collections.singletonList(MULTI_LEVEL_FILTER), subscriber);
        awaitAssert(() -> assertTrue(repository.get(MULTI_LEVEL_FILTER).isEmpty()));

        assertTrue(repository.getChannelsByTopic(SPORT_TOPIC).isEmpty());
    }

    /**
     * Registers a fresh subscriber for the given topic filters and waits until the repository knows it.
     *
     * @param filters the topic filters the subscriber subscribes to
     * @return the subscribed channel
     */
    private EmbeddedChannel subscribe(final String... filters) {
        EmbeddedChannel subscriber = new EmbeddedChannel();
        subscribers.add(subscriber);
        repository.add(subscriber, toSubscriptions(filters));
        awaitAssert(() -> Arrays.stream(filters).forEach(filter -> assertTrue(repository.get(filter).contains(subscriber))));
        return subscriber;
    }

    private List<MqttTopicSubscription> toSubscriptions(final String... topics) {
        return Arrays.stream(topics)
                .map(topic -> new MqttTopicSubscription(topic, MqttQoS.AT_MOST_ONCE))
                .collect(Collectors.toList());
    }

    /**
     * Releases every topic so that the shared static map of the repository starts empty for the next test.
     */
    private void clearTopics() {
        repository.remove(ALL_TOPICS);
        awaitAssert(() -> ALL_TOPICS.forEach(topic -> assertTrue(repository.get(topic).isEmpty())));
    }

    /**
     * Waits until the repository applied every pending asynchronous mutation. Needed before asserting that a
     * mutation did <em>not</em> change the shared state.
     */
    private void awaitRepositoryIdle() {
        assertTrue(ForkJoinPool.commonPool().awaitQuiescence(TIMEOUT.toMillis(), TimeUnit.MILLISECONDS));
    }
}
