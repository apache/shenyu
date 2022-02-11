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

package org.apache.shenyu.springboot.starter.sync.data.websocket;

import org.apache.shenyu.plugin.sync.data.websocket.WebsocketSyncDataService;
import org.apache.shenyu.plugin.sync.data.websocket.config.WebsocketConfig;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.hamcrest.MatcherAssert.assertThat;

/**
 * Test case for {@link WebsocketSyncDataConfiguration}.
 */
@ExtendWith(SpringExtension.class)
@SpringBootTest(
        classes = {
                WebsocketSyncDataConfiguration.class
        },
        webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT,
        properties = {
                "shenyu.sync.websocket.urls=ws://localhost:9095/websocket"
        })
@EnableAutoConfiguration
@MockBean(PluginDataSubscriber.class)
public final class WebsocketSyncDataConfigurationTest {

    @Autowired
    private WebsocketConfig websocketConfig;

    @Autowired
    private WebsocketSyncDataService websocketSyncDataService;

    @Test
    public void testWebsocketSyncDataService() {
        assertNotNull(websocketSyncDataService);
    }

    @Test
    public void testWebsocketConfig() {
        assertThat(websocketConfig.getUrls(), is("ws://localhost:9095/websocket"));
    }
}
