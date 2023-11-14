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

import java.sql.Timestamp;
import java.util.Arrays;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * test cast for {@link BatchPluginChangedEvent}.
 */
public class BatchPluginChangedEventTest {

    private PluginDO one;

    private PluginDO two;

    private BatchPluginChangedEvent changedEvent;

    @BeforeEach
    public void setUp() {
        Timestamp now = new Timestamp(System.currentTimeMillis());
        one = PluginDO.builder()
                .id("1")
                .name("test-plugin")
                .config("{\"config\":\"test\"}")
                .enabled(false)
                .role("Test")
                .sort(1)
                .dateUpdated(now)
                .dateCreated(now)
                .build();
        two = PluginDO.builder()
                .id("2")
                .name("test-plugin-two")
                .config("{\"config\":\"test\"}")
                .enabled(false)
                .role("Test")
                .sort(2)
                .dateUpdated(now)
                .dateCreated(now)
                .build();

        changedEvent = new BatchPluginChangedEvent(Arrays.asList(one, two), null, EventTypeEnum.PLUGIN_UPDATE, "test-operator");
    }

    @Test
    public void batchChangePluginBuildContextTest() {
        String context = String.format("the plugins[%s] is %s", "test-plugin,test-plugin-two", EventTypeEnum.PLUGIN_UPDATE.getType().toString().toLowerCase());
        assertEquals(context, changedEvent.buildContext());
    }
}
