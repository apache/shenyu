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
import org.dromara.soul.admin.SoulAdminBootstrap;
import org.dromara.soul.common.enums.DataEventTypeEnum;
import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import javax.websocket.Session;
import java.lang.reflect.Field;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.Set;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

/**
 * Test case for WebsocketCollector.
 *
 * @author wuudongdong
 */
@Slf4j
@ActiveProfiles("test")
@RunWith(SpringJUnit4ClassRunner.class)
@SpringBootTest(classes = SoulAdminBootstrap.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT,
        properties = "spring.autoconfigure.exclude=org.springframework.boot.actuate.autoconfigure.jdbc.DataSourceHealthContributorAutoConfiguration")
public final class WebsocketCollectorTest {

    private WebSocketClient client;

    @Before
    public void setUp() throws URISyntaxException {
        client = new WebSocketClient(new URI("ws://localhost:9095/websocket")) {
            @Override
            public void onOpen(final ServerHandshake serverHandshake) {
                log.info("Open connection");
            }

            @Override
            public void onMessage(final String s) {
                log.info("message : {}", s);
            }

            @Override
            public void onClose(final int i, final String s, final boolean b) {
                log.info("connection closed");
            }

            @Override
            public void onError(final Exception e) {
            }
        };
    }

    @Test
    public void testOnOpen() throws Exception {
        client.connectBlocking();
        Thread.sleep(1000);
        assertEquals(1L, getSessionSetSize());
        client.closeBlocking();
    }

    @Test
    public void testOnMessage() throws Exception {
        client.connectBlocking();
        Thread.sleep(1000);
        client.send(DataEventTypeEnum.MYSELF.name());
        Thread.sleep(1000);
        assertNotNull(getSession());
        client.closeBlocking();
    }

    @Test
    public void testOnClose() throws Exception {
        client.connectBlocking();
        Thread.sleep(1000);
        assertEquals(1L, getSessionSetSize());
        client.closeBlocking();
        Thread.sleep(1000);
        assertEquals(0L, getSessionSetSize());
        assertNull(getSession());
    }

    @Test
    public void testSend() throws Exception {
        client.connectBlocking();
        Thread.sleep(1000);
        client.send(DataEventTypeEnum.MYSELF.name());
        Thread.sleep(1000);
        assertNotNull(getSession());
        WebsocketCollector.send(null, DataEventTypeEnum.MYSELF);
        WebsocketCollector.send("test", DataEventTypeEnum.MYSELF);
        WebsocketCollector.send("test", DataEventTypeEnum.CREATE);
        Thread.sleep(1000);
        client.closeBlocking();
        assertEquals(0L, getSessionSetSize());
        assertNull(getSession());
    }

    private long getSessionSetSize() throws ClassNotFoundException {
        return ((Set) getField("SESSION_SET")).size();
    }

    private Session getSession() throws ClassNotFoundException {
        return (Session) getField("session");
    }

    private Object getField(final String fieldName) throws ClassNotFoundException {
        Class clazz = Class.forName("org.dromara.soul.admin.listener.websocket.WebsocketCollector");
        Field[] declaredFields = clazz.getDeclaredFields();
        return Arrays.stream(declaredFields)
                .filter(each -> {
                    each.setAccessible(true);
                    return fieldName.equals(each.getName());
                })
                .findFirst()
                .map(each -> {
                    try {
                        return each.get(clazz);
                    } catch (IllegalAccessException e) {
                        log.error("get field error", e);
                        return null;
                    }
                }).orElse(null);
    }
}
