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

package org.apache.shenyu.protocol.mqtt;

import io.netty.channel.Channel;
import io.netty.handler.codec.mqtt.MqttPublishMessage;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.SubscribeRepository;
import org.apache.shenyu.protocol.mqtt.repositories.WillRepository;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class PublishWillTest {

    @Mock
    private SubscribeRepository subscribeRepository;

    @Mock
    private Channel subscriberChannel;

    @BeforeEach
    public void setUp() {
        Singleton.INST.single(SubscribeRepository.class, subscribeRepository);
    }

    @AfterEach
    public void tearDown() {
        Singleton.INST.single(SubscribeRepository.class, new SubscribeRepository());
    }

    @Test
    public void testPublishWillToActiveSubscriber() {
        when(subscriberChannel.isActive()).thenReturn(true);
        when(subscribeRepository.getChannelsByTopic("status/offline"))
                .thenReturn(Collections.singletonList(subscriberChannel));

        byte[] message = "client lost".getBytes();
        WillRepository.WillEntry will = new WillRepository.WillEntry("status/offline", message, 1, true);
        Publish.publishWill(will);

        ArgumentCaptor<Object> captor = ArgumentCaptor.forClass(Object.class);
        verify(subscriberChannel).writeAndFlush(captor.capture());
        MqttPublishMessage published = (MqttPublishMessage) captor.getValue();
        assertEquals("status/offline", published.variableHeader().topicName());
        assertEquals(1, published.fixedHeader().qosLevel().value());
        assertTrue(published.fixedHeader().isRetain());
    }

    @Test
    public void testPublishWillSkipsInactiveChannel() {
        when(subscriberChannel.isActive()).thenReturn(false);
        when(subscribeRepository.getChannelsByTopic("status/inactive"))
                .thenReturn(Collections.singletonList(subscriberChannel));

        WillRepository.WillEntry will = new WillRepository.WillEntry("status/inactive", "msg".getBytes(), 0, false);
        Publish.publishWill(will);

        verify(subscriberChannel, never()).writeAndFlush(any());
    }

    @Test
    public void testPublishWillToEmptySubscribers() {
        when(subscribeRepository.getChannelsByTopic("topic/none")).thenReturn(Collections.emptyList());

        WillRepository.WillEntry will = new WillRepository.WillEntry("topic/none", "msg".getBytes(), 2, false);
        Publish.publishWill(will);
    }

    @Test
    public void testPublishWillQosAndRetain() {
        when(subscriberChannel.isActive()).thenReturn(true);
        when(subscribeRepository.getChannelsByTopic("qos/retain"))
                .thenReturn(Collections.singletonList(subscriberChannel));

        WillRepository.WillEntry will = new WillRepository.WillEntry("qos/retain", "data".getBytes(), 0, false);
        Publish.publishWill(will);

        ArgumentCaptor<Object> captor = ArgumentCaptor.forClass(Object.class);
        verify(subscriberChannel).writeAndFlush(captor.capture());
        MqttPublishMessage published = (MqttPublishMessage) captor.getValue();
        assertEquals(0, published.fixedHeader().qosLevel().value());
        assertFalse(published.fixedHeader().isRetain());
    }

    @Test
    public void testPublishWillToWildcardSubscriber() {
        when(subscriberChannel.isActive()).thenReturn(true);
        when(subscribeRepository.getChannelsByTopic("status/client-001"))
                .thenReturn(Collections.singletonList(subscriberChannel));

        WillRepository.WillEntry will = new WillRepository.WillEntry("status/client-001", "gone".getBytes(), 0, false);
        Publish.publishWill(will);

        ArgumentCaptor<Object> captor = ArgumentCaptor.forClass(Object.class);
        verify(subscriberChannel).writeAndFlush(captor.capture());
        MqttPublishMessage published = (MqttPublishMessage) captor.getValue();
        assertEquals("status/client-001", published.variableHeader().topicName());
    }
}
