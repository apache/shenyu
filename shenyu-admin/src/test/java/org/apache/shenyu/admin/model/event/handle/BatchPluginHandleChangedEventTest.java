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

package org.apache.shenyu.admin.model.event.handle;

import java.util.Arrays;
import org.apache.shenyu.admin.model.entity.PluginHandleDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * test case for {@link BatchPluginHandleChangedEvent}.
 */
public class BatchPluginHandleChangedEventTest {

    private PluginHandleDO one;

    private PluginHandleDO two;

    @BeforeEach
    public void setUp() {
        one = PluginHandleDO.builder()
                .id("1")
                .pluginId("1")
                .field("testFieldOne")
                .label("testLabelOne")
                .dataType(1)
                .type(1)
                .sort(1)
                .build();
        two = PluginHandleDO.builder()
                .id("2")
                .pluginId("2")
                .field("testFieldTwo")
                .label("testLabelTwo")
                .dataType(2)
                .type(2)
                .sort(2)
                .build();
    }

    @Test
    public void batchChangePluginHandleContextTest() {
        BatchPluginHandleChangedEvent batchPluginHandleChangedEvent =
                new BatchPluginHandleChangedEvent(Arrays.asList(one, two), null, EventTypeEnum.PLUGIN_HANDLE_DELETE, "test-operator");
        String context =
                String.format("the plugin handle[%s] is %s", "testFieldOne,testFieldTwo", EventTypeEnum.PLUGIN_HANDLE_DELETE.getType().toString().toLowerCase());
        assertEquals(context, batchPluginHandleChangedEvent.buildContext());
    }
}
