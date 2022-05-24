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

package org.apache.shenyu.plugin.sync.data.websocket.client;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.WebsocketData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.java_websocket.handshake.ServerHandshake;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ScheduledThreadPoolExecutor;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

/**
 * add test case for {@link ShenyuWebsocketClient}.
 */
@ExtendWith(MockitoExtension.class)
public class ShenyuWebsocketClientTest {
    
    @InjectMocks
    private ShenyuWebsocketClient shenyuWebsocketClient;
    
    @Mock
    private URI serverUri;
    
    @Mock
    private PluginDataSubscriber pluginDataSubscriber;
    
    @Mock
    private List<MetaDataSubscriber> metaDataSubscribers;
    
    @Mock
    private List<AuthDataSubscriber> authDataSubscribers;
    
    @Mock
    private ScheduledThreadPoolExecutor executor;
    
    private WebsocketData<PluginData> websocketData;
    
    @BeforeEach
    public void setUp() {
        websocketData = new WebsocketData<>();
        websocketData.setEventType(DataEventTypeEnum.MYSELF.name());
        websocketData.setGroupType(ConfigGroupEnum.PLUGIN.name());
        List<PluginData> list = new ArrayList<>(1);
        PluginData pluginData = PluginData.builder().enabled(true).name("shenyu-plugin-grpc")
                .id("shenyu-plugin-grpc").role("admin").build();
        list.add(pluginData);
        websocketData.setData(list);
    }
    
    @Test
    public void testOnOpen() {
        shenyuWebsocketClient = spy(shenyuWebsocketClient);
        ServerHandshake serverHandshake = mock(ServerHandshake.class);
        doNothing().when(shenyuWebsocketClient).send(DataEventTypeEnum.MYSELF.name());
        shenyuWebsocketClient.onOpen(serverHandshake);
        verify(shenyuWebsocketClient).send(DataEventTypeEnum.MYSELF.name());
    }
    
    @Test
    public void testOnMessage() {
        doNothing().when(pluginDataSubscriber).onSubscribe(any());
        String json = GsonUtils.getInstance().toJson(websocketData);
        shenyuWebsocketClient.onMessage(json);
        verify(pluginDataSubscriber).onSubscribe(any());
    }
    
    @Test
    public void testOnClose() {
        shenyuWebsocketClient = spy(shenyuWebsocketClient);
        doNothing().when(shenyuWebsocketClient).close();
        shenyuWebsocketClient.onClose(1, "shenyu-plugin-grpc", true);
        verify(shenyuWebsocketClient).close();
    }
    
    @Test
    public void testOnError() {
        shenyuWebsocketClient = spy(shenyuWebsocketClient);
        Assertions.assertDoesNotThrow(() -> shenyuWebsocketClient.onError(new ShenyuException("test")));
    }
}
