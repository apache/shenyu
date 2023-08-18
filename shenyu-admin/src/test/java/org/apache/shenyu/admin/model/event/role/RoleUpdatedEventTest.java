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

import org.apache.shenyu.admin.model.entity.RoleDO;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import javax.validation.constraints.NotBlank;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * test cast for {@link RoleUpdatedEvent}.
 */
public final class RoleUpdatedEventTest {

    private RoleUpdatedEvent roleUpdatedEvent;

    private RoleDO roleDO;

    private List<@NotBlank String> permissionList;

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
        RoleDO roleBefore;
        roleBefore = RoleDO.builder()
                .id(id)
                .roleName("test-role-before")
                .description("test role")
                .dateUpdated(now)
                .dateCreated(now)
                .build();
        permissionList = new ArrayList<>();
        permissionList.add("test-permission1");
        permissionList.add("test-permission2");
        roleUpdatedEvent = new RoleUpdatedEvent(roleDO, roleBefore, "test-op", permissionList);
    }

    @Test
    public void testGetRole() {
        assertEquals(roleDO, roleUpdatedEvent.getRole());
    }

    @Test
    public void testGetNewPermission() {
        assertEquals(permissionList, roleUpdatedEvent.getNewPermission());
    }
}
