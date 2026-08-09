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

import io.netty.channel.embedded.EmbeddedChannel;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class WillRepositoryTest {

    private WillRepository willRepository;

    private EmbeddedChannel channel;

    @BeforeEach
    public void setUp() {
        willRepository = new WillRepository();
        channel = new EmbeddedChannel();
    }

    @AfterEach
    public void tearDown() {
        channel.close();
    }

    @Test
    public void testAddAndGetWillEntry() {
        byte[] message = "offline".getBytes();
        WillRepository.WillEntry entry = new WillRepository.WillEntry("topic/test", message, 1, true);
        willRepository.add(channel, entry);

        WillRepository.WillEntry result = willRepository.get(channel);
        assertThat(result, notNullValue());
        assertEquals("topic/test", result.getTopic());
        assertArrayEquals(message, result.getMessage());
        assertEquals(1, result.getQos());
        assertTrue(result.isRetain());
    }

    @Test
    public void testRemoveWillEntry() {
        WillRepository.WillEntry entry = new WillRepository.WillEntry("topic/test", "bye".getBytes(), 0, false);
        willRepository.add(channel, entry);
        assertThat(willRepository.get(channel), notNullValue());

        willRepository.remove(channel);
        assertThat(willRepository.get(channel), nullValue());
    }

    @Test
    public void testGetReturnsNullForUnknownChannel() {
        assertThat(willRepository.get(channel), nullValue());
    }

    @Test
    public void testWillEntryFields() {
        byte[] message = "disconnected".getBytes();
        WillRepository.WillEntry entry = new WillRepository.WillEntry("alerts/disconnect", message, 2, false);

        assertEquals("alerts/disconnect", entry.getTopic());
        assertArrayEquals("disconnected".getBytes(), entry.getMessage());
        assertEquals(2, entry.getQos());
        assertFalse(entry.isRetain());
    }

    @Test
    public void testReplaceWillEntryOnReconnect() {
        WillRepository.WillEntry first = new WillRepository.WillEntry("topic/a", "msg1".getBytes(), 0, false);
        WillRepository.WillEntry second = new WillRepository.WillEntry("topic/b", "msg2".getBytes(), 1, true);
        willRepository.add(channel, first);
        willRepository.add(channel, second);

        WillRepository.WillEntry result = willRepository.get(channel);
        assertEquals("topic/b", result.getTopic());
        assertArrayEquals("msg2".getBytes(), result.getMessage());
    }

    @Test
    public void testRemoveNonexistentChannel() {
        EmbeddedChannel otherChannel = new EmbeddedChannel();
        try {
            willRepository.remove(otherChannel);
            assertThat(willRepository.get(channel), nullValue());
        } finally {
            otherChannel.close();
        }
    }
}
