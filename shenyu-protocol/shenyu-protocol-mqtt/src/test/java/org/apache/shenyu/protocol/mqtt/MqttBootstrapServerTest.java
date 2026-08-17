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

import io.netty.channel.EventLoopGroup;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.lang.reflect.Field;
import java.net.ServerSocket;
import java.net.Socket;
import java.time.Duration;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

/**
 * Test cases for {@link MqttBootstrapServer}.
 */
public final class MqttBootstrapServerTest {

    @Test
    public void shutdownWithoutStartDoesNotThrow() {
        assertDoesNotThrow(new MqttBootstrapServer()::shutdown);
    }

    @Test
    public void startWithPortInUseThenShutdownDoesNotThrow() throws Exception {
        MqttContext env = new MqttContext();
        env.setBossGroupThreadCount(1);
        env.setWorkerGroupThreadCount(1);
        env.setMaxPayloadSize(1024);
        env.setLeakDetectorLevel("DISABLED");
        try (ServerSocket socket = new ServerSocket(0)) {
            env.setPort(socket.getLocalPort());
            MqttBootstrapServer server = new MqttBootstrapServer();
            server.start();
            await().atMost(Duration.ofSeconds(10))
                    .until(() -> getBossGroup(server).isShutdown());
            assertDoesNotThrow(server::shutdown);
        }
    }

    @Test
    public void startOnFreePortThenShutdownDoesNotThrow() throws Exception {
        MqttContext env = new MqttContext();
        env.setBossGroupThreadCount(1);
        env.setWorkerGroupThreadCount(1);
        env.setMaxPayloadSize(1024);
        env.setLeakDetectorLevel("DISABLED");
        try (ServerSocket socket = new ServerSocket(0)) {
            int freePort = socket.getLocalPort();
            env.setPort(freePort);
            MqttBootstrapServer server = new MqttBootstrapServer();
            server.start();
            await().atMost(Duration.ofSeconds(10))
                    .until(() -> canConnect(freePort));
            assertDoesNotThrow(server::shutdown);
        }
    }

    private static EventLoopGroup getBossGroup(final MqttBootstrapServer server) throws Exception {
        Field field = MqttBootstrapServer.class.getDeclaredField("bossGroup");
        field.setAccessible(true);
        return (EventLoopGroup) field.get(server);
    }

    private static boolean canConnect(final int port) {
        try (Socket ignored = new Socket("127.0.0.1", port)) {
            return true;
        } catch (IOException e) {
            return false;
        }
    }
}
