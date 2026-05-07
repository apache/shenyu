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

import org.apache.shenyu.admin.model.entity.DashboardUserDO;
import org.apache.shenyu.admin.model.event.user.BatchUserDeletedEvent;
import org.apache.shenyu.admin.model.event.user.UserCreatedEvent;
import org.apache.shenyu.admin.model.event.user.UserUpdatedEvent;
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
 * Test case for {@link UserEventPublisher}.
 */
@ExtendWith(MockitoExtension.class)
class UserEventPublisherTest {

    private static final String TEST_OPERATOR = "test-operator";

    @Mock
    private ApplicationEventPublisher applicationEventPublisher;

    private UserEventPublisher userEventPublisher;

    private MockedStatic<SessionUtil> sessionUtilMockedStatic;

    @BeforeEach
    void setUp() {
        userEventPublisher = new UserEventPublisher(applicationEventPublisher);
        sessionUtilMockedStatic = mockStatic(SessionUtil.class);
        sessionUtilMockedStatic.when(SessionUtil::visitorName).thenReturn(TEST_OPERATOR);
    }

    @AfterEach
    void tearDown() {
        sessionUtilMockedStatic.close();
    }

    @Test
    void testOnCreated() {
        DashboardUserDO user = buildDashboardUserDO("1", "testuser", "password");

        userEventPublisher.onCreated(user);

        ArgumentCaptor<UserCreatedEvent> captor = ArgumentCaptor.forClass(UserCreatedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        UserCreatedEvent event = captor.getValue();
        assertNotNull(event);
        assertEquals(user, event.getChangedUser());
    }

    @Test
    void testOnUpdated() {
        DashboardUserDO before = buildDashboardUserDO("1", "olduser", "oldpassword");
        DashboardUserDO after = buildDashboardUserDO("1", "newuser", "newpassword");

        userEventPublisher.onUpdated(after, before);

        ArgumentCaptor<UserUpdatedEvent> captor = ArgumentCaptor.forClass(UserUpdatedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        UserUpdatedEvent event = captor.getValue();
        assertNotNull(event);
        assertEquals(after, event.getAfter());
        assertEquals(before, event.getBefore());
    }

    @Test
    void testOnDeleted() {
        DashboardUserDO user1 = buildDashboardUserDO("1", "user1", "password1");
        DashboardUserDO user2 = buildDashboardUserDO("2", "user2", "password2");
        List<DashboardUserDO> users = Arrays.asList(user1, user2);

        userEventPublisher.onDeleted(users);

        ArgumentCaptor<BatchUserDeletedEvent> captor = ArgumentCaptor.forClass(BatchUserDeletedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        BatchUserDeletedEvent event = captor.getValue();
        assertNotNull(event);
    }

    @Test
    void testOnDeletedEmptyCollection() {
        List<DashboardUserDO> emptyUsers = Collections.emptyList();

        userEventPublisher.onDeleted(emptyUsers);

        ArgumentCaptor<BatchUserDeletedEvent> captor = ArgumentCaptor.forClass(BatchUserDeletedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        BatchUserDeletedEvent event = captor.getValue();
        assertNotNull(event);
    }

    @Test
    void testPublish() {
        DashboardUserDO user = buildDashboardUserDO("1", "testuser", "password");
        UserCreatedEvent event = new UserCreatedEvent(user, TEST_OPERATOR);

        userEventPublisher.publish(event);

        verify(applicationEventPublisher, times(1)).publishEvent(event);
    }

    private static DashboardUserDO buildDashboardUserDO(final String id, final String userName, final String password) {
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        DashboardUserDO userDO = new DashboardUserDO();
        userDO.setId(id);
        userDO.setUserName(userName);
        userDO.setPassword(password);
        userDO.setRole(1);
        userDO.setEnabled(true);
        userDO.setDateCreated(currentTime);
        userDO.setDateUpdated(currentTime);
        return userDO;
    }
}
