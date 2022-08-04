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

package org.apache.shenyu.common.utils;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for UpstreamCheckUtils.
 */
public final class UpstreamCheckUtilsTest {
    
    private static final Logger LOG = LoggerFactory.getLogger(UpstreamCheckUtilsTest.class);

    private volatile int port = -1;

    @Test
    public void testBlank() {
        assertFalse(UpstreamCheckUtils.checkUrl(""));
    }

    @Test
    @Disabled
    public void testSocketConnect() {
        Runnable runnable = () -> {
            ServerSocket serverSocket;
            try {
                serverSocket = new ServerSocket(0);
                port = serverSocket.getLocalPort();
                Socket socket = serverSocket.accept();
                socket.close();
            } catch (IOException e) {
                LOG.error(e.getMessage());
            }
        };
        new Thread(runnable).start();

        while (port == -1) {
            Thread.yield();
        }

        assertTrue(UpstreamCheckUtils.checkUrl("127.0.0.1:" + port));
        assertFalse(UpstreamCheckUtils.checkUrl("http://127.0.0.1:" + (port == 0 ? port + 1 : port - 1)));
        assertTrue(UpstreamCheckUtils.checkUrl("http://127.0.0.1:" + port));
        assertTrue(UpstreamCheckUtils.checkUrl("https://shenyu.apache.org"));
    }
}
