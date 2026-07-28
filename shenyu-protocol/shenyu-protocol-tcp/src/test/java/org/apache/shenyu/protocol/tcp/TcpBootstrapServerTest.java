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

package org.apache.shenyu.protocol.tcp;

import com.google.common.eventbus.EventBus;
import org.junit.jupiter.api.Test;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.InetSocketAddress;
import java.net.SocketAddress;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test Case For {@link TcpBootstrapServer}.
 */
public class TcpBootstrapServerTest {

    private final TcpBootstrapServer server = new TcpBootstrapServer(new EventBus());

    @Test
    public void testGetIpWithIpv4() throws Exception {
        SocketAddress address = new InetSocketAddress("192.168.1.1", 8080);
        String ip = invokeGetIp(address);
        assertEquals("192.168.1.1", ip);
    }

    @Test
    public void testGetIpWithIpv6() throws Exception {
        SocketAddress address = new InetSocketAddress("2001:db8::1", 12345);
        String ip = invokeGetIp(address);
        assertTrue(ip.contains("2001:db8"));
    }

    @Test
    public void testGetIpWithNull() {
        InvocationTargetException ex = assertThrows(InvocationTargetException.class, () -> invokeGetIp(null));
        assertTrue(ex.getCause() instanceof NullPointerException);
    }

    @Test
    public void testGetIpWithUnsupportedAddressType() {
        SocketAddress customAddress = new SocketAddress() {
            private static final long serialVersionUID = 1L;
        };
        InvocationTargetException ex = assertThrows(InvocationTargetException.class, () -> invokeGetIp(customAddress));
        assertTrue(ex.getCause() instanceof IllegalArgumentException);
    }

    private String invokeGetIp(final SocketAddress socketAddress) throws Exception {
        Method method = TcpBootstrapServer.class.getDeclaredMethod("getIp", SocketAddress.class);
        method.setAccessible(true);
        return (String) method.invoke(server, socketAddress);
    }
}
