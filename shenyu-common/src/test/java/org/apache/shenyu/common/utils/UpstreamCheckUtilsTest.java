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

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.ResourceLock;

import java.io.IOException;
import java.net.InetAddress;
import java.net.ProxySelector;
import java.net.ServerSocket;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * Test cases for UpstreamCheckUtils.
 */
public final class UpstreamCheckUtilsTest {

    @Test
    public void testBlank() {
        assertFalse(UpstreamCheckUtils.checkUrl(""));
    }

    @Test
    @ResourceLock("java.net.ProxySelector")
    public void testSocketConnectIgnoresDefaultProxy() throws IOException {
        ProxySelector original = ProxySelector.getDefault();
        ProxySelector proxySelector = mock(ProxySelector.class);
        try {
            ProxySelector.setDefault(proxySelector);
            String upstream;
            try (ServerSocket server = new ServerSocket(0, 0, InetAddress.getByName("127.0.0.1"))) {
                upstream = "127.0.0.1:" + server.getLocalPort();
                assertTrue(UpstreamCheckUtils.checkUrl(upstream));
                assertTrue(UpstreamCheckUtils.checkUrl("http://" + upstream));
                assertTrue(UpstreamCheckUtils.checkUrl("https://" + upstream));
            }
            assertFalse(UpstreamCheckUtils.checkUrl(upstream));
            verifyNoInteractions(proxySelector);
        } finally {
            ProxySelector.setDefault(original);
        }
    }
}
