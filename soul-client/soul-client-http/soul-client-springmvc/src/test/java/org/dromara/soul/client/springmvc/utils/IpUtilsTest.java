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

package org.dromara.soul.client.springmvc.utils;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.internal.stubbing.answers.ThrowsExceptionForClassType;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

import java.net.InetAddress;
import java.net.UnknownHostException;

import static org.junit.Assert.assertEquals;
import static org.powermock.api.mockito.PowerMockito.when;

/**
 * Test case for {@link IpUtils}.
 *
 * @author HoldDie
 */
@RunWith(PowerMockRunner.class)
@PrepareForTest(IpUtils.class)
public final class IpUtilsTest {

    @Mock
    InetAddress inetAddress;

    @Test
    public void testGetHost() throws UnknownHostException {
        PowerMockito.mockStatic(InetAddress.class);
        when(InetAddress.getLocalHost()).thenReturn(inetAddress);
        when(inetAddress.getHostAddress()).thenReturn("127.0.0.1");
        assertEquals("127.0.0.1", IpUtils.getHost());
    }

    @Test
    public void testGetHostWithException() throws UnknownHostException {
        PowerMockito.mockStatic(InetAddress.class);
        when(InetAddress.getLocalHost()).then(new ThrowsExceptionForClassType(UnknownHostException.class));
        assertEquals("127.0.0.1", IpUtils.getHost());
    }
}
