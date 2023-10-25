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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.entity.PluginHandleDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * test case for {@link PluginHandleChangedEvent}.
 */
public class PluginHandleChangedEventTest {

    private PluginHandleDO pluginHandleDO;

    private PluginHandleDO withoutChangePluginHandleDO;

    private PluginHandleDO changePluginHandleDO;

    @BeforeEach
    public void setUp() {
        pluginHandleDO = PluginHandleDO.builder()
                .id("1")
                .pluginId("1")
                .field("testField")
                .label("testLabel")
                .dataType(1)
                .type(1)
                .sort(1)
                .build();
        withoutChangePluginHandleDO = PluginHandleDO.builder()
                .id("1")
                .pluginId("1")
                .field("testField")
                .label("testLabel")
                .dataType(1)
                .type(1)
                .sort(1)
                .build();
        changePluginHandleDO = PluginHandleDO.builder()
                .id("1")
                .pluginId("1")
                .field("testField-change")
                .label("testLabel-change")
                .dataType(2)
                .type(2)
                .sort(2)
                .build();
    }

    @Test
    public void deletePluginHandleBuildContextTest() {
        PluginHandleChangedEvent pluginChangedEvent = new PluginHandleChangedEvent(pluginHandleDO, null,
                EventTypeEnum.PLUGIN_HANDLE_DELETE, "test-operator");
        String context = String.format("the plugin-handle [%s] is %s", pluginHandleDO.getField(),
                StringUtils.lowerCase(EventTypeEnum.PLUGIN_HANDLE_DELETE.getType().toString()));
        assertEquals(context, pluginChangedEvent.buildContext());
    }

    @Test
    public void createPluginHandleBuildContextTest() {
        PluginHandleChangedEvent pluginChangedEvent = new PluginHandleChangedEvent(pluginHandleDO, null,
                EventTypeEnum.PLUGIN_HANDLE_CREATE, "test-operator");
        String context = String.format("the plugin-handle [%s] is %s", pluginHandleDO.getField(),
                StringUtils.lowerCase(EventTypeEnum.PLUGIN_HANDLE_CREATE.getType().toString()));
        assertEquals(context, pluginChangedEvent.buildContext());
    }

    @Test
    public void updatePluginHandleBuildContextTest() {
        String eventTypeStr = StringUtils.lowerCase(EventTypeEnum.PLUGIN_HANDLE_UPDATE.getType().toString());
        PluginHandleChangedEvent pluginHandleUpdateEventWithoutChange =
                new PluginHandleChangedEvent(pluginHandleDO, pluginHandleDO, EventTypeEnum.PLUGIN_HANDLE_UPDATE, "test-operator");
        String withoutChangeContrast = "it no change";
        String context =
                String.format("the plugin-handle [%s] is %s : %s", pluginHandleDO.getField(), eventTypeStr, withoutChangeContrast);
        assertEquals(context, pluginHandleUpdateEventWithoutChange.buildContext());

        PluginHandleChangedEvent pluginHandleUpdateEventNotSameDO =
                new PluginHandleChangedEvent(withoutChangePluginHandleDO, pluginHandleDO, EventTypeEnum.PLUGIN_HANDLE_UPDATE, "test-operator");
        assertEquals(context, pluginHandleUpdateEventNotSameDO.buildContext());

        final StringBuilder contrast = new StringBuilder();
        contrast.append(String.format("field[%s => %s] ", pluginHandleDO.getField(), changePluginHandleDO.getField()));
        contrast.append(String.format("label[%s => %s] ", pluginHandleDO.getLabel(), changePluginHandleDO.getLabel()));
        contrast.append(String.format("type[%s => %s] ", pluginHandleDO.getType(), changePluginHandleDO.getType()));
        contrast.append(String.format("dataType[%s => %s] ", pluginHandleDO.getDataType(), changePluginHandleDO.getDataType()));
        contrast.append(String.format("sort[%s => %s] ", pluginHandleDO.getSort(), changePluginHandleDO.getSort()));
        String changeContext = String.format("the plugin-handle [%s] is %s : %s", changePluginHandleDO.getField(), eventTypeStr, contrast);

        PluginHandleChangedEvent pluginHandleUpdateEventChange =
                new PluginHandleChangedEvent(changePluginHandleDO, pluginHandleDO, EventTypeEnum.PLUGIN_HANDLE_UPDATE, "test-operator");
        assertEquals(changeContext, pluginHandleUpdateEventChange.buildContext());
    }

}
