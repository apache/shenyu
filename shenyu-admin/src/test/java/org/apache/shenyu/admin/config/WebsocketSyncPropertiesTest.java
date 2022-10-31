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

package org.apache.shenyu.admin.config;

import org.apache.shenyu.admin.AbstractConfigurationTest;
import org.apache.shenyu.admin.config.properties.WebsocketSyncProperties;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.comparesEqualTo;
import static org.hamcrest.Matchers.is;

/**
 * Test cases for WebsocketSyncProperties.
 */
public final class WebsocketSyncPropertiesTest extends AbstractConfigurationTest {

    @Test
    public void testWebsocketSyncPropertiesDefaultValue() {
        assertThat(new WebsocketSyncProperties().isEnabled(), is(true));
    }

    @Test
    public void testWebsocketSyncPropertiesSetValue() {
        load(WebsocketSyncPropertiesConfiguration.class, "shenyu.sync.websocket.enabled=false");
        WebsocketSyncProperties websocketSyncProperties = getContext().getBean(WebsocketSyncProperties.class);
        websocketSyncProperties.setMessageMaxSize(0);
        websocketSyncProperties.setAllowOrigins("allowOrigins");
        assertThat(websocketSyncProperties.isEnabled(), comparesEqualTo(false));
        Assertions.assertEquals(websocketSyncProperties.getMessageMaxSize(), 0);
        Assertions.assertEquals(websocketSyncProperties.getAllowOrigins(), "allowOrigins");
    }

    @Configuration
    @EnableConfigurationProperties(WebsocketSyncProperties.class)
    static class WebsocketSyncPropertiesConfiguration {
    }
}
