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
import io.netty.channel.ChannelHandlerContext;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.SubscribeRepository;
import org.apache.shenyu.protocol.mqtt.repositories.WillRepository;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class MqttTransportHandlerTest {

    @Mock
    private ChannelHandlerContext ctx;

    @Mock
    private Channel channel;

    @Mock
    private SubscribeRepository subscribeRepository;

    private MqttTransportHandler handler;

    private WillRepository willRepository;

    @BeforeEach
    public void setUp() {
        handler = new MqttTransportHandler();
        willRepository = new WillRepository();
        Singleton.INST.single(WillRepository.class, willRepository);
        Singleton.INST.single(SubscribeRepository.class, subscribeRepository);
        when(ctx.channel()).thenReturn(channel);
    }

    @AfterEach
    public void tearDown() {
        Singleton.INST.single(WillRepository.class, new WillRepository());
        Singleton.INST.single(SubscribeRepository.class, new SubscribeRepository());
    }

    @Test
    public void testChannelInactiveFiresWillAndRemovesIt() throws Exception {
        byte[] willMessage = "sudden disconnect".getBytes();
        WillRepository.WillEntry will = new WillRepository.WillEntry("status/offline", willMessage, 1, true);
        willRepository.add(channel, will);

        // publishWill uses subscribeRepository to get target channels
        when(subscribeRepository.getChannelsByTopic("status/offline")).thenReturn(java.util.Collections.emptyList());

        handler.channelInactive(ctx);

        // will should be removed after firing
        assertThat(willRepository.get(channel), nullValue());
    }

    @Test
    public void testChannelInactiveDoesNothingWhenNoWill() throws Exception {
        handler.channelInactive(ctx);

        assertThat(willRepository.get(channel), nullValue());
    }

    @Test
    public void testChannelInactiveAfterDisconnectClearsWill() throws Exception {
        byte[] willMessage = "graceful close".getBytes();
        WillRepository.WillEntry will = new WillRepository.WillEntry("status/clean", willMessage, 0, false);
        willRepository.add(channel, will);

        // simulate graceful disconnect: remove will first, then channelInactive
        willRepository.remove(channel);
        handler.channelInactive(ctx);

        assertThat(willRepository.get(channel), nullValue());
    }
}
