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

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.util.Vector;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link IpUtils}.
 */
public final class IpUtilsTest {

    private MockedStatic<NetworkInterface> networkInterfaceMockedStatic;

    @BeforeEach
    public void setUp() {
        networkInterfaceMockedStatic = mockStatic(NetworkInterface.class);
    }

    @AfterEach
    public void close() {
        networkInterfaceMockedStatic.close();
    }

    @Test
    public void testGetHost() throws Exception {
        // first net card
        Vector<InetAddress> addresses1 = new Vector<>();
        addresses1.add(InetAddress.getByAddress("local-host", new byte[]{(byte) 192, (byte) 168, (byte) 50, (byte) 66}));
        NetworkInterface nic1 = mock(NetworkInterface.class);
        when(nic1.getInetAddresses()).thenReturn(addresses1.elements());
        when(nic1.getName()).thenReturn("local");
        // second net card
        Vector<InetAddress> addresses2 = new Vector<>();
        addresses2.add(InetAddress.getByAddress("eth0-host", new byte[]{(byte) 172, (byte) 168, (byte) 166, (byte) 12}));
        NetworkInterface nic2 = mock(NetworkInterface.class);
        when(nic2.getInetAddresses()).thenReturn(addresses2.elements());
        when(nic2.getName()).thenReturn("eth0");
        // third net card
        Vector<InetAddress> addresses3 = new Vector<>();
        addresses3.add(InetAddress.getByAddress("eth1-host", new byte[]{(byte) 10, (byte) 150, (byte) 111, (byte) 66}));
        NetworkInterface nic3 = mock(NetworkInterface.class);
        when(nic3.getInetAddresses()).thenReturn(addresses3.elements());
        when(nic3.getName()).thenReturn("eth1");
        // add all
        Vector<NetworkInterface> nics = new Vector<>();
        nics.add(nic1);
        nics.add(nic2);
        nics.add(nic3);
        networkInterfaceMockedStatic.when((MockedStatic.Verification) NetworkInterface.getNetworkInterfaces()).thenReturn(nics.elements());
        String prefix1 = "172.168";
        assertEquals("172.168.166.12", IpUtils.getHost(prefix1));
    }

    @Test
    public void testGetHostHasNotMatchPrefix() throws Exception {
        // first net card
        Vector<InetAddress> addresses1 = new Vector<>();
        addresses1.add(InetAddress.getByAddress("local-host", new byte[]{(byte) 192, (byte) 168, (byte) 50, (byte) 66}));
        NetworkInterface nic1 = mock(NetworkInterface.class);
        when(nic1.getInetAddresses()).thenReturn(addresses1.elements());
        when(nic1.getName()).thenReturn("local");
        // second net card
        Vector<InetAddress> addresses2 = new Vector<>();
        addresses2.add(InetAddress.getByAddress("eth0-host", new byte[]{(byte) 172, (byte) 168, (byte) 166, (byte) 12}));
        NetworkInterface nic2 = mock(NetworkInterface.class);
        when(nic2.getInetAddresses()).thenReturn(addresses2.elements());
        when(nic2.getName()).thenReturn("eth0");
        // third net card
        Vector<InetAddress> addresses3 = new Vector<>();
        addresses3.add(InetAddress.getByAddress("eth1-host", new byte[]{(byte) 10, (byte) 150, (byte) 111, (byte) 66}));
        NetworkInterface nic3 = mock(NetworkInterface.class);
        when(nic3.getInetAddresses()).thenReturn(addresses3.elements());
        when(nic3.getName()).thenReturn("eth1");
        // add all
        Vector<NetworkInterface> nics = new Vector<>();
        nics.add(nic1);
        nics.add(nic2);
        nics.add(nic3);
        networkInterfaceMockedStatic.when((MockedStatic.Verification) NetworkInterface.getNetworkInterfaces()).thenReturn(nics.elements());
        assertEquals("172.168.166.12", IpUtils.getHost());
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
