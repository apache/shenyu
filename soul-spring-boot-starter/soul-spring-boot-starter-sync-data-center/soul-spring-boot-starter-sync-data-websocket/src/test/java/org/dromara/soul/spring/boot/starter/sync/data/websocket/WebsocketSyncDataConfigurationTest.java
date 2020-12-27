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

package org.dromara.soul.spring.boot.starter.sync.data.websocket;

import org.dromara.soul.plugin.sync.data.weboscket.WebsocketSyncDataService;
import org.dromara.soul.plugin.sync.data.weboscket.config.WebsocketConfig;
import org.dromara.soul.sync.data.api.PluginDataSubscriber;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Test case for {@link WebsocketSyncDataConfiguration}.
 *
 * @author midnight2104
 */
@RunWith(SpringRunner.class)
@SpringBootTest(
        classes = {
                WebsocketSyncDataConfiguration.class
        },
        webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT,
        properties = {
                "soul.sync.websocket.urls=ws://localhost:9095/websocket"
        })
@EnableAutoConfiguration
@MockBean(PluginDataSubscriber.class)
public class WebsocketSyncDataConfigurationTest {

    @Autowired
    private WebsocketConfig websocketConfig;

    @Autowired
    private WebsocketSyncDataService websocketSyncDataService;

    @Test
    public void testWebsocketSyncDataService() {
        Assert.assertNotNull(websocketSyncDataService);
    }

    @Test
    public void testWebsocketConfig() {
        Assert.assertEquals("ws://localhost:9095/websocket", websocketConfig.getUrls());
    }
}
