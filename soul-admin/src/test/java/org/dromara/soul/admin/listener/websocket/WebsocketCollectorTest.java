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

package org.dromara.soul.admin.listener.websocket;

import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.admin.service.SyncDataService;
import org.dromara.soul.admin.spring.SpringBeanUtils;
import org.dromara.soul.common.enums.DataEventTypeEnum;
import org.junit.Before;
import org.junit.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.test.util.ReflectionTestUtils;

import javax.websocket.RemoteEndpoint;
import javax.websocket.Session;
import java.util.Set;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test case for WebsocketCollector.
 *
 * @author wuudongdong
 */
@Slf4j
public final class WebsocketCollectorTest {

    private WebsocketCollector websocketCollector;

    private Session session;

    @Before
    public void setUp() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setCfgContext(context);
        SyncDataService syncDataService = mock(SyncDataService.class);
        when(SpringBeanUtils.getInstance().getBean(SyncDataService.class)).thenReturn(syncDataService);
        when(syncDataService.syncAll(DataEventTypeEnum.MYSELF)).thenReturn(true);
        session = mock(Session.class);
        RemoteEndpoint.Basic basic = mock(RemoteEndpoint.Basic.class);
        when(session.getBasicRemote()).thenReturn(basic);
        websocketCollector = new WebsocketCollector();
    }

    @Test
    public void testOnOpen() {
        websocketCollector.onOpen(session);
        assertEquals(1L, getSessionSetSize());
        websocketCollector.onClose(session);
    }

    @Test
    public void testOnMessage() {
        websocketCollector.onOpen(session);
        websocketCollector.onMessage(DataEventTypeEnum.MYSELF.name(), session);
        assertNotNull(getSession());
        websocketCollector.onClose(session);
    }

    @Test
    public void testOnClose() {
        websocketCollector.onOpen(session);
        assertEquals(1L, getSessionSetSize());
        websocketCollector.onClose(session);
        assertEquals(0L, getSessionSetSize());
        assertNull(getSession());
    }

    @Test
    public void testOnError() {
        websocketCollector.onOpen(session);
        assertEquals(1L, getSessionSetSize());
        Throwable throwable = mock(Throwable.class);
        websocketCollector.onError(session, throwable);
        assertEquals(0L, getSessionSetSize());
        assertNull(getSession());
    }

    @Test
    public void testSend() {
        websocketCollector.onOpen(session);
        assertEquals(1L, getSessionSetSize());
        websocketCollector.onMessage(DataEventTypeEnum.MYSELF.name(), session);
        WebsocketCollector.send(null, DataEventTypeEnum.MYSELF);
        WebsocketCollector.send("test", DataEventTypeEnum.MYSELF);
        WebsocketCollector.send("test", DataEventTypeEnum.CREATE);
        websocketCollector.onClose(session);
    }

    private long getSessionSetSize() {
        Set sessionSet = (Set) ReflectionTestUtils.getField(WebsocketCollector.class, "SESSION_SET");
        return sessionSet == null ? -1 : sessionSet.size();
    }

    private Session getSession() {
        return (Session) ReflectionTestUtils.getField(WebsocketCollector.class, "session");
    }
}
