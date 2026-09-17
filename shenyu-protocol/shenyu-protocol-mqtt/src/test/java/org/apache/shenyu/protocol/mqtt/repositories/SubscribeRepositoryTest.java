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
import org.awaitility.core.ThrowingRunnable;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link SubscribeRepository}.
 */
public final class SubscribeRepositoryTest {

    private static final Duration TIMEOUT = Duration.ofSeconds(5);

    private static final Duration POLL_INTERVAL = Duration.ofMillis(10);

    private static final String EXISTING_TOPIC = "test/existing-topic";

    private static final String ABSENT_TOPIC = "test/absent-topic";

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
            SPORT_TOPIC, PLAYER_TOPIC, SINGLE_LEVEL_FILTER, MULTI_LEVEL_FILTER, MATCH_ALL_FILTER);

    private final SubscribeRepository repository = new SubscribeRepository();

    private final List<EmbeddedChannel> subscribers = new ArrayList<>();

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
     * The repository mutates its state asynchronously on the common pool, so assertions have to be retried
     * until the mutation becomes visible.
     *
     * @param assertion the assertion to retry
     */
    private void awaitAssert(final ThrowingRunnable assertion) {
        await().atMost(TIMEOUT).pollInterval(POLL_INTERVAL).untilAsserted(assertion);
    }

    /**
     * Waits until the repository applied every pending asynchronous mutation. Needed before asserting that a
     * mutation did <em>not</em> change the shared state.
     */
    private void awaitRepositoryIdle() {
        assertTrue(ForkJoinPool.commonPool().awaitQuiescence(TIMEOUT.toMillis(), TimeUnit.MILLISECONDS));
    }
}
