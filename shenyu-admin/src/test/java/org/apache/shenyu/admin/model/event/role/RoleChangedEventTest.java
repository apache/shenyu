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

package org.apache.shenyu.admin.model.event.role;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.entity.RoleDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.sql.Timestamp;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * test cast for {@link RoleChangedEvent}.
 */
public final class RoleChangedEventTest {

    private RoleChangedEvent roleChangedEventRoleNameChangeTest;

    private RoleChangedEvent roleChangedEventDescriptionChangeTest;

    private RoleChangedEvent roleChangedEventTest;

    private RoleChangedEvent roleChangedEventBeforeNullTest;

    private RoleChangedEvent roleChangedEventWithoutChangeTest;

    private RoleDO roleDO;

    private RoleDO roleDORoleNameChange;

    private RoleDO roleDODescriptionChange;

    private RoleDO roleDOChange;

    @BeforeEach
    public void setUp() {
        Timestamp now = new Timestamp(System.currentTimeMillis());
        String id = UUIDUtils.getInstance().generateShortUuid();
        roleDO = RoleDO.builder()
                .id(id)
                .roleName("test-role")
                .description("test role")
                .dateUpdated(now)
                .dateCreated(now)
                .build();
        roleDORoleNameChange = RoleDO.builder()
                .id(id)
                .roleName("test-role-name-chang")
                .description("test role")
                .dateUpdated(now)
                .dateCreated(now)
                .build();
        roleDODescriptionChange = RoleDO.builder()
                .id(id)
                .roleName("test-role")
                .description("test role description change")
                .dateUpdated(now)
                .dateCreated(now)
                .build();
        roleDOChange = RoleDO.builder()
                .id(id)
                .roleName("test-role-name-change")
                .description("test role description change")
                .dateUpdated(now)
                .dateCreated(now)
                .build();
        roleChangedEventRoleNameChangeTest =
                new RoleChangedEvent(roleDORoleNameChange, roleDO, EventTypeEnum.ROLE_UPDATE, "test-op");
        roleChangedEventDescriptionChangeTest =
                new RoleChangedEvent(roleDODescriptionChange, roleDO, EventTypeEnum.ROLE_UPDATE, "test-op");
        roleChangedEventTest =
                new RoleChangedEvent(roleDOChange, roleDO, EventTypeEnum.ROLE_UPDATE, "test-op");
        roleChangedEventBeforeNullTest =
                new RoleChangedEvent(roleDO, null, EventTypeEnum.ROLE_UPDATE, "test-op");
        roleChangedEventWithoutChangeTest =
                new RoleChangedEvent(roleDO, roleDO, EventTypeEnum.ROLE_UPDATE, "test-op");
    }

    @Test
    public void testBuildContext() {
        String roleUpdateStr = StringUtils.lowerCase(EventTypeEnum.ROLE_UPDATE.getType().toString());

        String roleNameChangeStr =
                String.format("name[%s => %s] ", roleDO.getRoleName(), roleDORoleNameChange.getRoleName());
        String roleNameChangeExpectedStr = String.format("the role [%s] is %s : %s",
                roleDORoleNameChange.getRoleName(), roleUpdateStr, roleNameChangeStr);
        assertEquals(roleNameChangeExpectedStr, roleChangedEventRoleNameChangeTest.buildContext());

        String descriptionChangeStr =
                String.format("disc[%s => %s] ", roleDO.getDescription(), roleDODescriptionChange.getDescription());
        String descriptionChangeExpectedStr = String.format("the role [%s] is %s : %s",
                roleDODescriptionChange.getRoleName(), roleUpdateStr, descriptionChangeStr);
        assertEquals(descriptionChangeExpectedStr, roleChangedEventDescriptionChangeTest.buildContext());

        String changeStr = String.format("name[%s => %s] disc[%s => %s] ",
                roleDO.getRoleName(), roleDOChange.getRoleName(),
                roleDO.getDescription(), roleDOChange.getDescription());
        String changeExpectedStr = String.format("the role [%s] is %s : %s",
                roleDOChange.getRoleName(), roleUpdateStr, changeStr);
        assertEquals(changeExpectedStr, roleChangedEventTest.buildContext());

        String beforeNullExpectedStr = String.format("the role [%s] is %s", roleDO.getRoleName(), roleUpdateStr);
        assertEquals(beforeNullExpectedStr, roleChangedEventBeforeNullTest.buildContext());

        String withoutChangeExpectedStr = String.format("the role [%s] is %s : it no change",
                roleDO.getRoleName(), roleUpdateStr);
        assertEquals(withoutChangeExpectedStr, roleChangedEventWithoutChangeTest.buildContext());
    }

    @Test
    public void testEventName() {
        assertEquals("role", roleChangedEventTest.eventName());
    }
}
