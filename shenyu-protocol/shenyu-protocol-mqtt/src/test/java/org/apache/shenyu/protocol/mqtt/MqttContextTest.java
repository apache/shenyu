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
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.nio.charset.StandardCharsets;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.params.provider.Arguments.arguments;

/**
 * Test cases for {@link MqttContext}.
 */
public final class MqttContextTest {

    private static final int PORT = 1883;

    private static final int BOSS_GROUP_THREAD_COUNT = 1;

    private static final int WORKER_GROUP_THREAD_COUNT = 2;

    private static final int MAX_PAYLOAD_SIZE = 1024;

    private static final String USER_NAME = "test-user";

    private static final String PASSWORD = "test-password";

    private static final String LEAK_DETECTOR_LEVEL = "disabled";

    private static final byte[] PASSWORD_IN_BYTES = PASSWORD.getBytes(StandardCharsets.UTF_8);

    private final MqttContext mqttContext = new MqttContext();

    @BeforeEach
    public void setUp() {
        mqttContext.setPort(PORT);
        mqttContext.setBossGroupThreadCount(BOSS_GROUP_THREAD_COUNT);
        mqttContext.setWorkerGroupThreadCount(WORKER_GROUP_THREAD_COUNT);
        mqttContext.setMaxPayloadSize(MAX_PAYLOAD_SIZE);
        mqttContext.setUserName(USER_NAME);
        mqttContext.setPassword(PASSWORD);
        mqttContext.setLeakDetectorLevel(LEAK_DETECTOR_LEVEL);
    }

    @AfterEach
    public void tearDown() {
        mqttContext.setPort(0);
        mqttContext.setBossGroupThreadCount(0);
        mqttContext.setWorkerGroupThreadCount(0);
        mqttContext.setMaxPayloadSize(0);
        mqttContext.setUserName(null);
        mqttContext.setPassword(null);
        mqttContext.setLeakDetectorLevel(null);
    }

    @Test
    public void settersShouldBeReflectedByGetters() {
        assertEquals(PORT, mqttContext.getPort());
        assertEquals(BOSS_GROUP_THREAD_COUNT, mqttContext.getBossGroupThreadCount());
        assertEquals(WORKER_GROUP_THREAD_COUNT, mqttContext.getWorkerGroupThreadCount());
        assertEquals(MAX_PAYLOAD_SIZE, mqttContext.getMaxPayloadSize());
        assertEquals(USER_NAME, mqttContext.getUserName());
        assertEquals(PASSWORD, mqttContext.getPassword());
        assertEquals(LEAK_DETECTOR_LEVEL, mqttContext.getLeakDetectorLevel());
    }

    @Test
    public void settingsShouldBeSharedBetweenInstances() {
        MqttContext anotherContext = new MqttContext();

        assertEquals(PORT, anotherContext.getPort());
        assertEquals(USER_NAME, anotherContext.getUserName());
        assertEquals(PASSWORD, anotherContext.getPassword());
    }

    @ParameterizedTest(name = "userName=[{0}], password=[{1}] should be valid: {2}")
    @MethodSource("credentials")
    public void isValidShouldCheckConfiguredCredentials(final String userName, final byte[] passwordInBytes, final boolean expected) {
        assertEquals(expected, MqttContext.isValid(userName, passwordInBytes));
    }

    private static Stream<Arguments> credentials() {
        return Stream.of(
                arguments(USER_NAME, PASSWORD_IN_BYTES, true),
                arguments(USER_NAME, null, false),
                arguments(USER_NAME, new byte[0], false),
                arguments(null, PASSWORD_IN_BYTES, false),
                arguments("", PASSWORD_IN_BYTES, false),
                arguments(USER_NAME, "wrong-password".getBytes(StandardCharsets.UTF_8), false),
                arguments("wrong-user", PASSWORD_IN_BYTES, false),
                arguments(USER_NAME.toUpperCase(), PASSWORD_IN_BYTES, false),
                arguments(USER_NAME, PASSWORD.toUpperCase().getBytes(StandardCharsets.UTF_8), false));
    }

    @Test
    public void isValidShouldOnlyAcceptLatestConfiguredCredentials() {
        String updatedUserName = "updated-user";
        String updatedPassword = "updated-password";
        mqttContext.setUserName(updatedUserName);
        mqttContext.setPassword(updatedPassword);

        assertTrue(MqttContext.isValid(updatedUserName, updatedPassword.getBytes(StandardCharsets.UTF_8)));
        assertFalse(MqttContext.isValid(USER_NAME, PASSWORD_IN_BYTES));
    }
}
