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

package org.dromara.soul.common.utils;

import org.junit.Test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

import lombok.SneakyThrows;

/**
 * Test cases for UpstreamCheckUtils.
 *
 * @author dengliming
 */
public final class UpstreamCheckUtilsTest {

    @Test
    public void testBlank() {
        assertFalse(UpstreamCheckUtils.checkUrl(""));
    }

    @Test
    public void testNotIp() {
        assertFalse(UpstreamCheckUtils.checkUrl("test"));
    }

    @Test
    public void testPingHostname() {
        assertTrue(UpstreamCheckUtils.checkUrl("localhost"));
    }

    @Test
    @SneakyThrows
    public void testSocketConnect() {
        Runnable runnable = () -> {
            ServerSocket serverSocket;
            try {
                serverSocket = new ServerSocket(13098);
                Socket socket = serverSocket.accept();
                socket.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        };
        new Thread(runnable).start();
        Thread.sleep(100);
        assertTrue(UpstreamCheckUtils.checkUrl("127.0.0.1:13098"));
        assertFalse(UpstreamCheckUtils.checkUrl("http://127.0.0.1:13099"));
        assertTrue(UpstreamCheckUtils.checkUrl("http://127.0.0.1:13098"));
    }
}
