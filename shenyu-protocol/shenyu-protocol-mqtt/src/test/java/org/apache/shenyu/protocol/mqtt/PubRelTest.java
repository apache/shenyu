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

import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.mqtt.MqttFixedHeader;
import io.netty.handler.codec.mqtt.MqttMessage;
import io.netty.handler.codec.mqtt.MqttMessageIdVariableHeader;
import io.netty.handler.codec.mqtt.MqttMessageType;
import io.netty.handler.codec.mqtt.MqttQoS;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

/**
 * Test cases for PubRel.
 */
public class PubRelTest {

    private static final int PACKET_ID = 12345;

    private final ChannelHandlerContext ctx = mock(ChannelHandlerContext.class);

    @Test
    public void testPubRelRespondsWithPubComp() {
        MqttFixedHeader pubRelFixedHeader = new MqttFixedHeader(MqttMessageType.PUBREL, false, MqttQoS.AT_LEAST_ONCE, false, 0);
        MqttMessage pubRel = new MqttMessage(pubRelFixedHeader, MqttMessageIdVariableHeader.from(PACKET_ID));
        new PubRel().pubRel(ctx, pubRel);

        ArgumentCaptor<MqttMessage> captor = ArgumentCaptor.forClass(MqttMessage.class);
        verify(ctx).writeAndFlush(captor.capture());
        MqttMessage pubComp = captor.getValue();
        assertEquals(MqttMessageType.PUBCOMP, pubComp.fixedHeader().messageType());
        assertEquals(MqttQoS.AT_MOST_ONCE, pubComp.fixedHeader().qosLevel());
        assertEquals(PACKET_ID, ((MqttMessageIdVariableHeader) pubComp.variableHeader()).messageId());
    }
}
