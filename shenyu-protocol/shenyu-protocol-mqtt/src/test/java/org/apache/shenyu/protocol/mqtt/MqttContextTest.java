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

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link MqttContext}.
 */
public final class MqttContextTest {

    @BeforeEach
    public void setUp() {
        MqttContext context = new MqttContext();
        context.setPort(1883);
        context.setBossGroupThreadCount(1);
        context.setWorkerGroupThreadCount(2);
        context.setMaxPayloadSize(1024);
        context.setUserName("test-user");
        context.setPassword("test-password");
        context.setLeakDetectorLevel("disabled");
    }

    @AfterEach
    public void tearDown() {
        MqttContext context = new MqttContext();
        context.setPort(0);
        context.setBossGroupThreadCount(0);
        context.setWorkerGroupThreadCount(0);
        context.setMaxPayloadSize(0);
        context.setUserName(null);
        context.setPassword(null);
        context.setLeakDetectorLevel(null);
    }

    @Test
    public void settersShouldUpdateStaticState() {
        MqttContext context = new MqttContext();

        assertEquals(1883, context.getPort());
        assertEquals(1, context.getBossGroupThreadCount());
        assertEquals(2, context.getWorkerGroupThreadCount());
        assertEquals(1024, context.getMaxPayloadSize());
        assertEquals("test-user", context.getUserName());
        assertEquals("test-password", context.getPassword());
        assertEquals("disabled", context.getLeakDetectorLevel());
    }

    @Test
    public void emptyUserNameOrPasswordShouldBeRejected() {
        byte[] password = "test-password".getBytes(StandardCharsets.UTF_8);

        assertFalse(MqttContext.isValid("", password));
        assertFalse(MqttContext.isValid("test-user", new byte[0]));
    }

    @Test
    public void mismatchedCredentialsShouldBeRejected() {
        byte[] password = "test-password".getBytes(StandardCharsets.UTF_8);

        assertFalse(MqttContext.isValid("another-user", password));
        assertFalse(MqttContext.isValid("test-user", "another-password".getBytes(StandardCharsets.UTF_8)));
    }

    @Test
    public void validCredentialsShouldBeAccepted() {
        assertTrue(MqttContext.isValid("test-user", "test-password".getBytes(StandardCharsets.UTF_8)));
    }

    @Test
    public void nullUserNameArgumentShouldBeRejectedWithoutNpe() {
        assertFalse(MqttContext.isValid(null, "test-password".getBytes(StandardCharsets.UTF_8)));
    }
}
