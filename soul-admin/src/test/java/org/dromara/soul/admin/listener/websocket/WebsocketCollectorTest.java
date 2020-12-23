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

import org.dromara.soul.admin.service.SyncDataService;
import org.dromara.soul.admin.spring.SpringBeanUtils;
import org.dromara.soul.common.enums.DataEventTypeEnum;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.test.util.ReflectionTestUtils;

import javax.websocket.RemoteEndpoint;
import javax.websocket.Session;
import java.io.IOException;
import java.util.Set;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test case for WebsocketCollector.
 *
 * @author wuudongdong
 */
@RunWith(MockitoJUnitRunner.class)
public final class WebsocketCollectorTest {

    private WebsocketCollector websocketCollector;

    @Mock
    private Session session;

    @Mock
    private SyncDataService syncDataService;

    @Before
    public void setUp() {
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
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setCfgContext(context);
        when(SpringBeanUtils.getInstance().getBean(SyncDataService.class)).thenReturn(syncDataService);
        when(syncDataService.syncAll(DataEventTypeEnum.MYSELF)).thenReturn(true);
        websocketCollector.onOpen(session);
        websocketCollector.onMessage(DataEventTypeEnum.MYSELF.name(), session);
        assertEquals(session, getSession());
        Mockito.verify(syncDataService, Mockito.times(1)).syncAll(DataEventTypeEnum.MYSELF);
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
    public void testSend() throws IOException {
        RemoteEndpoint.Basic basic = mock(RemoteEndpoint.Basic.class);
        when(session.getBasicRemote()).thenReturn(basic);
        websocketCollector.onOpen(session);
        assertEquals(1L, getSessionSetSize());
        WebsocketCollector.send(null, DataEventTypeEnum.MYSELF);
        Mockito.verify(basic, Mockito.times(0)).sendText(null);
        ReflectionTestUtils.setField(WebsocketCollector.class, "session", session);
        WebsocketCollector.send("test_message_1", DataEventTypeEnum.MYSELF);
        Mockito.verify(basic, Mockito.times(1)).sendText("test_message_1");
        WebsocketCollector.send("test_message_2", DataEventTypeEnum.CREATE);
        Mockito.verify(basic, Mockito.times(1)).sendText("test_message_2");
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
