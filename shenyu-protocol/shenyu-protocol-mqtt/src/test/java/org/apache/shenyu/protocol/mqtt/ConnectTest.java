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
import io.netty.handler.codec.mqtt.MqttConnectMessage;
import io.netty.handler.codec.mqtt.MqttConnectPayload;
import io.netty.handler.codec.mqtt.MqttConnectVariableHeader;
import io.netty.handler.codec.mqtt.MqttVersion;
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
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class ConnectTest {

    private static final String VALID_USER = "admin";

    private static final String VALID_PASS = "pass123";

    @Mock
    private ChannelHandlerContext ctx;

    @Mock
    private Channel channel;

    @Mock
    private MqttConnectMessage msg;

    @Mock
    private MqttConnectVariableHeader variableHeader;

    @Mock
    private MqttConnectPayload payload;

    private Connect connect;

    private WillRepository willRepository;

    private ChannelRepository channelRepository;

    @BeforeEach
    public void setUp() {
        connect = new Connect();
        willRepository = new WillRepository();
        channelRepository = new ChannelRepository();
        Singleton.INST.single(WillRepository.class, willRepository);
        Singleton.INST.single(ChannelRepository.class, channelRepository);

        when(ctx.channel()).thenReturn(channel);
        when(msg.variableHeader()).thenReturn(variableHeader);
        when(msg.payload()).thenReturn(payload);
        when(variableHeader.version()).thenReturn((int) MqttVersion.MQTT_3_1.protocolLevel());
        when(payload.clientIdentifier()).thenReturn("test-client-001");
        when(payload.userName()).thenReturn(VALID_USER);
        when(payload.passwordInBytes()).thenReturn(VALID_PASS.getBytes());

        MqttContext mqttContext = new MqttContext();
        mqttContext.setUserName(VALID_USER);
        mqttContext.setPassword(VALID_PASS);
    }

    @AfterEach
    public void tearDown() {
        Singleton.INST.single(WillRepository.class, new WillRepository());
        Singleton.INST.single(ChannelRepository.class, new ChannelRepository());
    }

    @Test
    public void testStoresWillOnConnect() {
        byte[] willMessage = "client disconnected unexpectedly".getBytes();
        when(variableHeader.isWillFlag()).thenReturn(true);
        when(variableHeader.willQos()).thenReturn(1);
        when(variableHeader.isWillRetain()).thenReturn(true);
        when(payload.willTopic()).thenReturn("status/client-001");
        when(payload.willMessageInBytes()).thenReturn(willMessage);

        connect.connect(ctx, msg);

        WillRepository.WillEntry will = willRepository.get(channel);
        assertThat(will, notNullValue());
        assertEquals("status/client-001", will.getTopic());
        assertArrayEquals(willMessage, will.getMessage());
        assertEquals(1, will.getQos());
        assertTrue(will.isRetain());
    }

    @Test
    public void testDoesNotStoreWillWhenWillFlagIsFalse() {
        when(variableHeader.isWillFlag()).thenReturn(false);

        connect.connect(ctx, msg);

        WillRepository.WillEntry will = willRepository.get(channel);
        assertThat(will, nullValue());
    }

    @Test
    public void testWillQosZero() {
        byte[] willMessage = "qos0 will".getBytes();
        when(variableHeader.isWillFlag()).thenReturn(true);
        when(variableHeader.willQos()).thenReturn(0);
        when(variableHeader.isWillRetain()).thenReturn(false);
        when(payload.willTopic()).thenReturn("topic/qos0");
        when(payload.willMessageInBytes()).thenReturn(willMessage);

        connect.connect(ctx, msg);

        WillRepository.WillEntry will = willRepository.get(channel);
        assertThat(will, notNullValue());
        assertEquals(0, will.getQos());
        assertFalse(will.isRetain());
    }

    @Test
    public void testWillRetainTrue() {
        byte[] willMessage = "retained will".getBytes();
        when(variableHeader.isWillFlag()).thenReturn(true);
        when(variableHeader.willQos()).thenReturn(2);
        when(variableHeader.isWillRetain()).thenReturn(true);
        when(payload.willTopic()).thenReturn("topic/retained");
        when(payload.willMessageInBytes()).thenReturn(willMessage);

        connect.connect(ctx, msg);

        WillRepository.WillEntry will = willRepository.get(channel);
        assertThat(will, notNullValue());
        assertTrue(will.isRetain());
        assertEquals(2, will.getQos());
    }
}
