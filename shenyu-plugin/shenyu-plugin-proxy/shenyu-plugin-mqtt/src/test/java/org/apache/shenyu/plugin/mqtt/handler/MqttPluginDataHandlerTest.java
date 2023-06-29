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

package org.apache.shenyu.plugin.mqtt.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.awaitility.Awaitility;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.net.InetAddress;
import java.net.Socket;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test case for {@link MqttPluginDataHandler}.
 */
public class MqttPluginDataHandlerTest {

    private MqttPluginDataHandler mqttPluginDataHandlerUnderTest;

    @BeforeEach
    public void setUp() {
        mqttPluginDataHandlerUnderTest = new MqttPluginDataHandler();
    }

    @Test
    public void testEnableConfiguration() throws InterruptedException {
        final PluginData enablePluginData = new PluginData("pluginId", "pluginName", "{\n"
                + "  \"port\": 9500,"
                + "  \"bossGroupThreadCount\": 1,"
                + "  \"maxPayloadSize\": 65536,"
                + "  \"workerGroupThreadCount\": 12,"
                + "  \"userName\": \"shenyu\","
                + "  \"password\": \"shenyu\","
                + "  \"isEncryptPassword\": false,"
                + "  \"encryptMode\": \"\","
                + "  \"leakDetectorLevel\": \"DISABLED\""
                + "}", "0", true, null);
        mqttPluginDataHandlerUnderTest.handlerPlugin(enablePluginData);
        assertTrue(isPortUsing());
        final PluginData disablePluginData = new PluginData("pluginId", "pluginName", "{\n"
                + "  \"port\": 9500,"
                + "  \"bossGroupThreadCount\": 1,"
                + "  \"maxPayloadSize\": 65536,"
                + "  \"workerGroupThreadCount\": 12,"
                + "  \"userName\": \"shenyu\","
                + "  \"password\": \"shenyu\","
                + "  \"isEncryptPassword\": false,"
                + "  \"encryptMode\": \"\","
                + "  \"leakDetectorLevel\": \"DISABLED\""
                + "}", "0", false, null);
        mqttPluginDataHandlerUnderTest.handlerPlugin(disablePluginData);

        Awaitility.await()
                .atMost(5, TimeUnit.SECONDS)
                .untilAsserted(() -> assertFalse(isPortUsing()));
    }

    private boolean isPortUsing() {
        boolean flag = false;
        try {
            InetAddress address = InetAddress.getByName("127.0.0.1");
            Socket socket = new Socket(address, 9500);
            flag = true;
        } catch (Exception ignored) {

        }
        return flag;

    }
}
