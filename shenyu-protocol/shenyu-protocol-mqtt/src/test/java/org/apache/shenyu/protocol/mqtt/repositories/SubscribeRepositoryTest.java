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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.time.Duration;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.awaitility.Awaitility.await;
import static org.mockito.Mockito.mock;

/**
 * Test cases for {@link SubscribeRepository}.
 */
public final class SubscribeRepositoryTest {

    private static final String TOPIC = "test/subscribe";

    private static final String MULTI_TOPIC_A = "test/subscribe-a";

    private static final String MULTI_TOPIC_B = "test/subscribe-b";

    private static final String CONCURRENT_TOPIC = "test/concurrent-subscribe";

    private static final String CONCURRENT_TOPICS_TOPIC = "test/concurrent-subscribe-topics";

    private static final int SUBSCRIBER_COUNT = 50;

    private SubscribeRepository repository;

    @BeforeEach
    void setUp() {
        repository = new SubscribeRepository();
    }

    @Test
    public void addChannelSubscribesChannelToTopic() {
        Channel channel = mock(Channel.class);
        repository.add(channel, Collections.singletonList(new MqttTopicSubscription(TOPIC, MqttQoS.AT_MOST_ONCE)));
        await().atMost(Duration.ofSeconds(5)).until(() -> repository.get(TOPIC).contains(channel));
    }

    @Test
    public void addTopicsRegistersChannelsForEachTopic() {
        Channel channel = mock(Channel.class);
        repository.add(List.of(MULTI_TOPIC_A, MULTI_TOPIC_B), Collections.singletonList(channel));
        await().atMost(Duration.ofSeconds(5)).until(() ->
                repository.get(MULTI_TOPIC_A).contains(channel) && repository.get(MULTI_TOPIC_B).contains(channel));
    }

    @Test
    public void concurrentAddsToSameTopicDoNotLoseSubscribers() {
        List<Channel> channels = IntStream.range(0, SUBSCRIBER_COUNT)
                .mapToObj(i -> mock(Channel.class))
                .collect(Collectors.toList());
        channels.parallelStream().forEach(channel -> repository.add(channel,
                Collections.singletonList(new MqttTopicSubscription(CONCURRENT_TOPIC, MqttQoS.AT_MOST_ONCE))));
        await().atMost(Duration.ofSeconds(10)).until(() -> repository.get(CONCURRENT_TOPIC).size() == SUBSCRIBER_COUNT);
    }

    @Test
    public void concurrentAddsForTopicsDoNotLoseChannels() {
        List<Channel> channels = IntStream.range(0, SUBSCRIBER_COUNT)
                .mapToObj(i -> mock(Channel.class))
                .collect(Collectors.toList());
        channels.parallelStream().forEach(channel -> repository.add(
                Collections.singletonList(CONCURRENT_TOPICS_TOPIC), Collections.singletonList(channel)));
        await().atMost(Duration.ofSeconds(10)).until(() -> repository.get(CONCURRENT_TOPICS_TOPIC).size() == SUBSCRIBER_COUNT);
    }
}
