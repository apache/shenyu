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

package org.apache.shenyu.admin.listener;

import org.apache.shenyu.admin.mode.ShenyuRunningModeService;
import org.apache.shenyu.common.utils.IpUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.web.server.WebServer;
import org.springframework.boot.web.context.WebServerInitializedEvent;
import org.springframework.test.util.ReflectionTestUtils;

import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link ApplicationStartListener}.
 */
@ExtendWith(MockitoExtension.class)
public final class ApplicationStartListenerTest {

    @InjectMocks
    private ApplicationStartListener applicationStartListener;

    @Mock
    private ShenyuRunningModeService shenyuRunningModeService;

    @Mock
    private WebServerInitializedEvent event;

    @Mock
    private WebServer webServer;

    @BeforeEach
    void setUp() {
        ReflectionTestUtils.setField(applicationStartListener, "contextPath", "/shenyu");
        when(event.getWebServer()).thenReturn(webServer);
        when(webServer.getPort()).thenReturn(9095);
    }

    @Test
    void testOnApplicationEvent() {
        try (MockedStatic<IpUtils> ipUtilsMocked = mockStatic(IpUtils.class)) {
            ipUtilsMocked.when(IpUtils::getHost).thenReturn("192.168.1.1");
            applicationStartListener.onApplicationEvent(event);
            verify(shenyuRunningModeService).start("192.168.1.1", 9095, "/shenyu");
        }
    }

    @Test
    void testOnApplicationEventWithEmptyContextPath() {
        ReflectionTestUtils.setField(applicationStartListener, "contextPath", "");
        try (MockedStatic<IpUtils> ipUtilsMocked = mockStatic(IpUtils.class)) {
            ipUtilsMocked.when(IpUtils::getHost).thenReturn("10.0.0.1");
            applicationStartListener.onApplicationEvent(event);
            verify(shenyuRunningModeService).start("10.0.0.1", 9095, "");
        }
    }

    @Test
    void testOnApplicationEventWithDifferentPort() {
        when(webServer.getPort()).thenReturn(8080);
        try (MockedStatic<IpUtils> ipUtilsMocked = mockStatic(IpUtils.class)) {
            ipUtilsMocked.when(IpUtils::getHost).thenReturn("127.0.0.1");
            applicationStartListener.onApplicationEvent(event);
            verify(shenyuRunningModeService).start("127.0.0.1", 8080, "/shenyu");
        }
    }
}
