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
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.time.Duration;
import java.util.Arrays;
import java.util.Collections;
import java.util.concurrent.ForkJoinPool;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

/**
 * Test cases for {@link SubscribeRepository}.
 */
public final class SubscribeRepositoryTest {

    private static final String EXISTING_TOPIC = "test/existing-topic";

    private static final String ABSENT_TOPIC = "test/absent-topic";

    private static final String KEPT_TOPIC = "test/kept-topic";

    private static final String TOPIC = "test/topic";

    private static final String OTHER_TOPIC = "test/other-topic";

    private final SubscribeRepository repository = new SubscribeRepository();

    private final Channel channel = mock(Channel.class);

    @Test
    public void removeRemovesChannelFromExistingTopic() {
        SubscribeRepository repository = new SubscribeRepository();
        Channel channel = mock(Channel.class);
        repository.add(channel, Collections.singletonList(new MqttTopicSubscription(EXISTING_TOPIC, MqttQoS.AT_MOST_ONCE)));
        await().atMost(Duration.ofSeconds(5)).until(() -> repository.get(EXISTING_TOPIC).contains(channel));

        repository.remove(Collections.singletonList(EXISTING_TOPIC), channel);

        await().atMost(Duration.ofSeconds(5)).until(() -> repository.get(EXISTING_TOPIC).isEmpty());
    }

    @Test
    public void removeAbsentTopicDoesNotThrow() {
        SubscribeRepository repository = new SubscribeRepository();
        Channel channel = mock(Channel.class);
        repository.add(channel, Collections.singletonList(new MqttTopicSubscription(KEPT_TOPIC, MqttQoS.AT_MOST_ONCE)));
        await().atMost(Duration.ofSeconds(5)).until(() -> repository.get(KEPT_TOPIC).contains(channel));

        assertDoesNotThrow(() -> repository.remove(Collections.singletonList(ABSENT_TOPIC), channel));
        await().atMost(Duration.ofSeconds(5))
                .until(() -> ForkJoinPool.commonPool().awaitQuiescence(1, TimeUnit.SECONDS));

        assertTrue(repository.get(ABSENT_TOPIC).isEmpty());
        assertTrue(repository.get(KEPT_TOPIC).contains(channel));
    }

    @BeforeEach
    public void setUp() {
        Singleton.INST.single(SubscribeRepository.class, repository);
    }

    @AfterEach
    public void tearDown() {
        repository.remove(Arrays.asList(TOPIC, OTHER_TOPIC));
        await().atMost(5, TimeUnit.SECONDS).untilAsserted(() -> assertTrue(repository.get(TOPIC).isEmpty()));
    }

    @Test
    public void testAddStoresGrantedQosPerTopic() {
        repository.add(channel, Collections.singletonList(new MqttTopicSubscription(TOPIC, MqttQoS.AT_LEAST_ONCE)));
        await().atMost(5, TimeUnit.SECONDS).untilAsserted(() ->
                assertEquals(MqttQoS.AT_LEAST_ONCE, repository.get(TOPIC).get(channel)));
    }

    @Test
    public void testAddKeepsMaxQosForOverlappingSubscription() {
        repository.add(channel, Arrays.asList(
                new MqttTopicSubscription(TOPIC, MqttQoS.AT_LEAST_ONCE),
                new MqttTopicSubscription(TOPIC, MqttQoS.EXACTLY_ONCE)));
        await().atMost(5, TimeUnit.SECONDS).untilAsserted(() ->
                assertEquals(MqttQoS.EXACTLY_ONCE, repository.get(TOPIC).get(channel)));
    }

    @Test
    public void testAddIgnoresFailureSubscription() {
        repository.add(channel, Collections.singletonList(new MqttTopicSubscription(TOPIC, MqttQoS.FAILURE)));
        await().atMost(5, TimeUnit.SECONDS).untilAsserted(() -> assertTrue(repository.get(TOPIC).isEmpty()));
    }

    @Test
    public void testRemoveChannelFromTopic() {
        repository.add(channel, Collections.singletonList(new MqttTopicSubscription(TOPIC, MqttQoS.AT_MOST_ONCE)));
        await().atMost(5, TimeUnit.SECONDS).untilAsserted(() ->
                assertEquals(MqttQoS.AT_MOST_ONCE, repository.get(TOPIC).get(channel)));
        repository.remove(Collections.singletonList(TOPIC), channel);
        await().atMost(5, TimeUnit.SECONDS).untilAsserted(() -> assertTrue(repository.get(TOPIC).isEmpty()));
    }

    @Test
    public void testGetTopicsMergesSubscribers() {
        repository.add(channel, Arrays.asList(
                new MqttTopicSubscription(TOPIC, MqttQoS.AT_MOST_ONCE),
                new MqttTopicSubscription(OTHER_TOPIC, MqttQoS.AT_LEAST_ONCE)));
        await().atMost(5, TimeUnit.SECONDS).untilAsserted(() -> {
            Map<Channel, MqttQoS> subscribers = repository.get(Arrays.asList(TOPIC, OTHER_TOPIC));
            assertEquals(MqttQoS.AT_LEAST_ONCE, subscribers.get(channel));
        });
    }
}
