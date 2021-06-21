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

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.MockedStatic;

import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.util.Vector;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link IpUtils}.
 */
public final class IpUtilsTest {

    private MockedStatic<NetworkInterface> networkInterfaceMockedStatic;

    @Before
    public void setUp() {
        networkInterfaceMockedStatic = mockStatic(NetworkInterface.class);
    }

    @After
    public void close() {
        networkInterfaceMockedStatic.close();
    }

    @Test
    public void testGetHost() throws Exception {
        Vector<InetAddress> addresses = new Vector<>();
        addresses.add(InetAddress.getByAddress("local", new byte[]{(byte) 192, (byte) 168, (byte) 50, (byte) 66}));
        addresses.add(InetAddress.getByAddress("eth0", new byte[]{(byte) 172, (byte) 168, (byte) 166, (byte) 12}));
        addresses.add(InetAddress.getByAddress("eth1", new byte[]{(byte) 10, (byte) 150, (byte) 111, (byte) 66}));
        NetworkInterface nic = mock(NetworkInterface.class);
        when(nic.getInetAddresses()).thenReturn(addresses.elements());
        Vector<NetworkInterface> nics = new Vector<>();
        nics.add(nic);
        networkInterfaceMockedStatic.when((MockedStatic.Verification) NetworkInterface.getNetworkInterfaces()).thenReturn(nics.elements());
        String prefix1 = "172.168";
        assertEquals("172.168.166.12", IpUtils.getHost(prefix1));
    }

    @Test
    public void testIsCompleteHost() {
        assertTrue(IpUtils.isCompleteHost("192.168.1.166"));
        assertFalse(IpUtils.isCompleteHost("192.168."));
        assertFalse(IpUtils.isCompleteHost("192.."));
    }

    @Test
    public void testGetHostWithException() throws Exception {
        networkInterfaceMockedStatic.when((MockedStatic.Verification) NetworkInterface.getNetworkInterfaces())
                .thenThrow(SocketException.class);
        assertEquals("127.0.0.1", IpUtils.getHost());
    }
}
