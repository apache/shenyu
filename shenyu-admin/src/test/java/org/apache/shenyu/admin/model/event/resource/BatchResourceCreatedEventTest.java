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

import java.util.Arrays;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.entity.ResourceDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * test case for {@link BatchResourceCreatedEvent}.
 */
public class BatchResourceCreatedEventTest {

    private ResourceDO one;

    private ResourceDO two;

    @BeforeEach
    public void setUp() {
        one = ResourceDO.builder()
                .name("plug")
                .component("PluginList")
                .icon("dashboard")
                .title("SHENYU.MENU.PLUGIN.LIST")
                .sort(1)
                .perms("system:plugin:list")
                .build();

        two = ResourceDO.builder()
                .name("system")
                .component("system")
                .icon("setting")
                .title("SHENYU.MENU.SYSTEM.MANAGMENT")
                .sort(2)
                .perms("system:manager:list")
                .build();
    }

    @Test
    public void resourceCreateBuildContextTest() {
        BatchResourceCreatedEvent createdEvent = new BatchResourceCreatedEvent(Arrays.asList(one, two), "test-operator");

        String context = String.format("the resource [%s] is %s",
                one.getTitle() + "," + two.getTitle(), StringUtils.lowerCase(EventTypeEnum.RESOURCE_CREATE.getType().toString()));

        assertEquals(context, createdEvent.buildContext());
    }
}
