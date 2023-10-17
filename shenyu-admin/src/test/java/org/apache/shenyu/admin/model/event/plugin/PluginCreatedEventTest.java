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

package org.apache.shenyu.admin.model.event.plugin;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * test cast for {@link PluginCreatedEvent}.
 */
public class PluginCreatedEventTest {

    private PluginDO pluginDO;

    private PluginCreatedEvent pluginCreateEvent;

    @BeforeEach
    public void setUp() {
        pluginDO = PluginDO.builder()
                .id("1")
                .name("test-plugin")
                .config("{\"config\":\"test\"}")
                .enabled(false)
                .role("Test")
                .sort(1)
                .build();
        pluginCreateEvent = new PluginCreatedEvent(pluginDO, "test-operator");
    }

    @Test
    public void createPluginBuildContextTest() {
        String context =
                String.format("the plugin [%s] is %s", pluginDO.getName(), StringUtils.lowerCase(EventTypeEnum.PLUGIN_CREATE.getType().toString()));
        assertEquals(context, pluginCreateEvent.buildContext());
    }

    @Test
    public void getSourceTest() {
        assertEquals(pluginDO, pluginCreateEvent.getSource());
    }
}
