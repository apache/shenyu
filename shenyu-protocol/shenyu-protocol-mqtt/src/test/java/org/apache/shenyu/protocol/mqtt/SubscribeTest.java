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
import io.netty.channel.ChannelInboundHandlerAdapter;
import io.netty.channel.embedded.EmbeddedChannel;
import io.netty.handler.codec.mqtt.MqttMessageBuilders;
import io.netty.handler.codec.mqtt.MqttQoS;
import io.netty.handler.codec.mqtt.MqttSubAckMessage;
import io.netty.handler.codec.mqtt.MqttSubscribeMessage;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.SubscribeRepository;
import org.apache.shenyu.protocol.mqtt.repositories.TopicRepository;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.List;
import java.util.function.BooleanSupplier;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

class SubscribeTest {

    @Test
    void testSubscribeRejectsInvalidTopicFilter() throws InterruptedException {
        EmbeddedChannel channel = new EmbeddedChannel();
        channel.pipeline().addLast(new ChannelInboundHandlerAdapter());
        ChannelHandlerContext ctx = channel.pipeline().firstContext();
        MqttSubscribeMessage msg = MqttMessageBuilders.subscribe()
                .messageId(1)
                .addSubscription(MqttQoS.AT_MOST_ONCE, "sport/#")
                .addSubscription(MqttQoS.AT_MOST_ONCE, "bad#filter")
                .build();

        SubscribeRepository repository = new SubscribeRepository();
        Singleton.INST.single(SubscribeRepository.class, repository);
        Singleton.INST.single(TopicRepository.class, new TopicRepository());

        new Subscribe().subscribe(ctx, msg);
        awaitUntil(() -> repository.get("sport/#").contains(channel));
        assertTrue(repository.get("bad#filter").isEmpty());

        MqttSubAckMessage subAck = channel.readOutbound();
        assertNotNull(subAck);
        List<Integer> granted = subAck.payload().grantedQoSLevels();
        assertEquals(2, granted.size());
        assertEquals(0, granted.get(0).intValue());
        assertEquals(0x80, granted.get(1).intValue());

        repository.remove(Collections.singletonList("sport/#"), channel);
        awaitUntil(() -> repository.get("sport/#").isEmpty());
        channel.finishAndReleaseAll();
    }

    private void awaitUntil(final BooleanSupplier condition) throws InterruptedException {
        long deadline = System.currentTimeMillis() + 5000;
        while (!condition.getAsBoolean()) {
            if (System.currentTimeMillis() >= deadline) {
                fail("condition not met within timeout");
            }
            Thread.sleep(10);
        }
    }
}
