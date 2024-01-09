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

package org.apache.shenyu.admin.model.event.resource;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.entity.ResourceDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * test case for {@link ResourceChangedEvent}.
 */
public class ResourceChangedEventTest {

    private ResourceDO before;

    private ResourceDO after;

    @BeforeEach
    public void setUp() {
        before = ResourceDO.builder()
                .name("plug")
                .component("PluginList")
                .icon("dashboard")
                .title("SHENYU.MENU.PLUGIN.LIST")
                .sort(1)
                .perms("system:plugin:list")
                .build();

        after = ResourceDO.builder()
                .name("system")
                .component("system")
                .icon("setting")
                .title("SHENYU.MENU.SYSTEM.MANAGMENT")
                .sort(2)
                .perms("system:manager:list")
                .build();
    }

    @Test
    public void resourceChangeBuildContextTest() {

        final StringBuilder contrast = new StringBuilder();
        contrast.append(String.format("name[%s => %s] ", before.getName(), after.getName()));
        contrast.append(String.format("component[%s => %s] ", before.getComponent(), after.getComponent()));
        contrast.append(String.format("match icon[%s => %s] ", before.getIcon(), after.getIcon()));
        contrast.append(String.format("title[%s => %s] ", before.getTitle(), after.getTitle()));
        contrast.append(String.format("sort[%s => %s] ", before.getSort(), after.getSort()));
        contrast.append(String.format("perms[%s => %s] ", before.getPerms(), after.getPerms()));

        String typeStr = StringUtils.lowerCase(EventTypeEnum.RESOURCE_UPDATE.getType().toString());
        String context = String.format("the resource [%s] is %s : %s", after.getTitle(), typeStr, contrast);

        ResourceChangedEvent resourceChangedEvent = new ResourceChangedEvent(after, before, EventTypeEnum.RESOURCE_UPDATE, "test-operator");
        assertEquals(context, resourceChangedEvent.buildContext());
    }

    @Test
    public void resourceDeleteBuildContextTest() {
        ResourceChangedEvent resourceDeleteEvent = new ResourceChangedEvent(after, after, EventTypeEnum.RESOURCE_DELETE, "test-operator");

        String typeStr = StringUtils.lowerCase(EventTypeEnum.RESOURCE_DELETE.getType().toString());
        String context = String.format("the resource [%s] is %s : %s", after.getTitle(), typeStr, "it no change");

        assertEquals(context, resourceDeleteEvent.buildContext());
    }
}
