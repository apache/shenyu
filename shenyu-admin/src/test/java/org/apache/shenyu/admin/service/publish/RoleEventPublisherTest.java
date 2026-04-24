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

package org.apache.shenyu.admin.service.publish;

import org.apache.shenyu.admin.model.entity.RoleDO;
import org.apache.shenyu.admin.model.event.role.BatchRoleDeletedEvent;
import org.apache.shenyu.admin.model.event.role.RoleCreatedEvent;
import org.apache.shenyu.admin.model.event.role.RoleUpdatedEvent;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ApplicationEventPublisher;

import java.sql.Timestamp;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * Test case for {@link RoleEventPublisher}.
 */
@ExtendWith(MockitoExtension.class)
class RoleEventPublisherTest {

    private static final String TEST_OPERATOR = "test-operator";

    @Mock
    private ApplicationEventPublisher applicationEventPublisher;

    private RoleEventPublisher roleEventPublisher;

    private MockedStatic<SessionUtil> sessionUtilMockedStatic;

    @BeforeEach
    void setUp() {
        roleEventPublisher = new RoleEventPublisher(applicationEventPublisher);
        sessionUtilMockedStatic = mockStatic(SessionUtil.class);
        sessionUtilMockedStatic.when(SessionUtil::visitorName).thenReturn(TEST_OPERATOR);
    }

    @AfterEach
    void tearDown() {
        sessionUtilMockedStatic.close();
    }

    @Test
    void testOnCreated() {
        RoleDO role = buildRoleDO("1", "admin", "Administrator role");

        roleEventPublisher.onCreated(role);

        ArgumentCaptor<RoleCreatedEvent> captor = ArgumentCaptor.forClass(RoleCreatedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        RoleCreatedEvent event = captor.getValue();
        assertNotNull(event);
        assertEquals(role, event.getRole());
    }

    @Test
    void testOnUpdated() {
        RoleDO before = buildRoleDO("1", "admin", "Old description");
        RoleDO after = buildRoleDO("1", "admin", "New description");
        List<String> permissions = Arrays.asList("permission1", "permission2");

        roleEventPublisher.onUpdated(after, before, permissions);

        ArgumentCaptor<RoleUpdatedEvent> captor = ArgumentCaptor.forClass(RoleUpdatedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        RoleUpdatedEvent event = captor.getValue();
        assertNotNull(event);
        assertEquals(after, event.getRole());
        assertEquals(permissions, event.getNewPermission());
    }

    @Test
    void testOnUpdatedWithEmptyPermissions() {
        RoleDO before = buildRoleDO("1", "admin", "Old description");
        RoleDO after = buildRoleDO("1", "admin", "New description");
        List<String> emptyPermissions = Collections.emptyList();

        roleEventPublisher.onUpdated(after, before, emptyPermissions);

        ArgumentCaptor<RoleUpdatedEvent> captor = ArgumentCaptor.forClass(RoleUpdatedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        RoleUpdatedEvent event = captor.getValue();
        assertNotNull(event);
        assertEquals(after, event.getRole());
        assertEquals(emptyPermissions, event.getNewPermission());
    }

    @Test
    void testOnDeleted() {
        RoleDO role1 = buildRoleDO("1", "admin", "Admin role");
        RoleDO role2 = buildRoleDO("2", "user", "User role");
        List<RoleDO> roles = Arrays.asList(role1, role2);

        roleEventPublisher.onDeleted(roles);

        ArgumentCaptor<BatchRoleDeletedEvent> captor = ArgumentCaptor.forClass(BatchRoleDeletedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        BatchRoleDeletedEvent event = captor.getValue();
        assertNotNull(event);
    }

    @Test
    void testOnDeletedWithEmptyCollection() {
        List<RoleDO> emptyList = Collections.emptyList();

        roleEventPublisher.onDeleted(emptyList);

        verify(applicationEventPublisher, times(1)).publishEvent(ArgumentCaptor.forClass(BatchRoleDeletedEvent.class).capture());
    }

    @Test
    void testOnDeletedWithSingleRole() {
        RoleDO role = buildRoleDO("1", "admin", "Admin role");
        List<RoleDO> roles = Collections.singletonList(role);

        roleEventPublisher.onDeleted(roles);

        ArgumentCaptor<BatchRoleDeletedEvent> captor = ArgumentCaptor.forClass(BatchRoleDeletedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        BatchRoleDeletedEvent event = captor.getValue();
        assertNotNull(event);
    }

    @Test
    void testPublish() {
        RoleDO role = buildRoleDO("1", "admin", "Admin role");
        RoleCreatedEvent event = new RoleCreatedEvent(role, TEST_OPERATOR);

        roleEventPublisher.publish(event);

        verify(applicationEventPublisher, times(1)).publishEvent(event);
    }

    private static RoleDO buildRoleDO(final String id, final String roleName, final String description) {
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        return RoleDO.builder()
                .id(id)
                .roleName(roleName)
                .description(description)
                .dateCreated(currentTime)
                .dateUpdated(currentTime)
                .build();
    }
}
