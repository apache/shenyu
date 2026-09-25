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
import org.apache.shenyu.protocol.mqtt.repositories.ChannelRepository;
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
public class DisconnectTest {

    @Mock
    private ChannelHandlerContext ctx;

    @Mock
    private Channel channel;

    private Disconnect disconnect;

    private WillRepository willRepository;

    private ChannelRepository channelRepository;

    @BeforeEach
    public void setUp() {
        disconnect = new Disconnect();
        willRepository = new WillRepository();
        channelRepository = new ChannelRepository();
        Singleton.INST.single(WillRepository.class, willRepository);
        Singleton.INST.single(ChannelRepository.class, channelRepository);
        when(ctx.channel()).thenReturn(channel);
    }

    @AfterEach
    public void tearDown() {
        Singleton.INST.single(WillRepository.class, new WillRepository());
        Singleton.INST.single(ChannelRepository.class, new ChannelRepository());
    }

    @Test
    public void testDisconnectClearsWill() {
        WillRepository.WillEntry will = new WillRepository.WillEntry("topic/will", "goodbye".getBytes(), 0, false);
        willRepository.add(channel, will);

        disconnect.disconnect(ctx);

        assertThat(willRepository.get(channel), nullValue());
    }

    @Test
    public void testDisconnectWithoutWill() {
        disconnect.disconnect(ctx);
        assertThat(willRepository.get(channel), nullValue());
    }

    @Test
    public void testDisconnectRemovesChannel() {
        channelRepository.add(channel, "client-1");
        disconnect.disconnect(ctx);
        assertThat(channelRepository.get(channel), nullValue());
    }
}
