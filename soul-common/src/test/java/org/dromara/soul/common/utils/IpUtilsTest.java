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

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.MockedStatic;

import java.net.InetAddress;
import java.net.UnknownHostException;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link IpUtils}.
 *
 * @author HoldDie
 */
public final class IpUtilsTest {

    private MockedStatic<InetAddress> inetAddressMockedStatic;

    @Before
    public void setUp() {
        inetAddressMockedStatic = mockStatic(InetAddress.class);
    }

    @After
    public void close() {
        inetAddressMockedStatic.close();
    }

    @Test
    public void testGetHost() throws UnknownHostException {
        InetAddress inetAddress = mock(InetAddress.class);
        inetAddressMockedStatic.when((MockedStatic.Verification) InetAddress.getLocalHost()).thenReturn(inetAddress);
        when(inetAddress.getHostAddress()).thenReturn("127.0.0.1");
        assertEquals("127.0.0.1", IpUtils.getHost());
    }

    @Test
    public void testGetHostWithException() throws UnknownHostException {
        inetAddressMockedStatic.when((MockedStatic.Verification) InetAddress.getLocalHost())
                .thenThrow(UnknownHostException.class);
        assertEquals("127.0.0.1", IpUtils.getHost());
    }
}
