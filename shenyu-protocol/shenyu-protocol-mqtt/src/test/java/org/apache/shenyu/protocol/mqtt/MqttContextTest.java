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

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test Case For {@link MqttContext}.
 */
public class MqttContextTest {

    private static final String USER_NAME = "testUser";

    private static final String PASSWORD = "testPass";

    private final MqttContext mqttContext = new MqttContext();

    @BeforeEach
    public void setUp() {
        mqttContext.setUserName(USER_NAME);
        mqttContext.setPassword(PASSWORD);
    }

    @Test
    public void testIsValidWithCorrectCredentials() {
        assertTrue(MqttContext.isValid(USER_NAME, PASSWORD.getBytes()));
    }

    @Test
    public void testIsValidWithNullPasswordInBytes() {
        assertFalse(MqttContext.isValid(USER_NAME, null));
    }

    @Test
    public void testIsValidWithEmptyPassword() {
        assertFalse(MqttContext.isValid(USER_NAME, new byte[0]));
    }

    @Test
    public void testIsValidWithWrongPassword() {
        assertFalse(MqttContext.isValid(USER_NAME, "wrongPass".getBytes()));
    }

    @Test
    public void testIsValidWithNullUserName() {
        assertFalse(MqttContext.isValid(null, PASSWORD.getBytes()));
    }

    @Test
    public void testIsValidWithEmptyUserName() {
        assertFalse(MqttContext.isValid("", PASSWORD.getBytes()));
    }
}
