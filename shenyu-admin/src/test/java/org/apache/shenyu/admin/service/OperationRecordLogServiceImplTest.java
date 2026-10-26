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

package org.apache.shenyu.admin.service;

import org.apache.shenyu.admin.config.properties.DashboardProperties;
import org.apache.shenyu.admin.exception.ValidFailException;
import org.apache.shenyu.admin.mapper.OperationRecordLogMapper;
import org.apache.shenyu.admin.model.custom.UserInfo;
import org.apache.shenyu.admin.model.query.RecordLogQueryCondition;
import org.apache.shenyu.admin.service.impl.OperationRecordLogServiceImpl;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.Mock;

import java.util.Collections;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/** Tests for {@link OperationRecordLogServiceImpl}. */
@ExtendWith(MockitoExtension.class)
public final class OperationRecordLogServiceImplTest {
    @Mock
    private OperationRecordLogMapper mapper;

    @Mock
    private DashboardProperties properties;

    @InjectMocks
    private OperationRecordLogServiceImpl service;

    @AfterEach
    public void cleanSession() {
        SessionUtil.clean();
    }

    @Test
    public void testConditionSearchAndList() {
        SessionUtil.setLocalVisitor(UserInfo.builder().userName("operator").build());
        RecordLogQueryCondition condition = new RecordLogQueryCondition();
        condition.setStartTime(new Date(1_000));
        condition.setEndTime(new Date(2_000));
        service.doConditionPreProcessing(condition);
        assertEquals("operator", condition.getUsername());
        when(mapper.selectByCondition(condition)).thenReturn(Collections.emptyList());
        assertTrue(service.searchByCondition(condition).isEmpty());
        when(properties.getRecordLogLimit()).thenReturn(10);
        when(mapper.selectLimit("operator", 10)).thenReturn(Collections.emptyList());
        assertTrue(service.list().isEmpty());
        verify(mapper).selectByCondition(condition);
        verify(mapper).selectLimit("operator", 10);
    }

    @Test
    public void testAdminConditionPreservesUsernameAndListsAllUsers() {
        SessionUtil.setLocalVisitor(UserInfo.builder().userName(org.apache.shenyu.common.constant.AdminConstants.ADMIN_NAME).build());
        RecordLogQueryCondition adminCondition = new RecordLogQueryCondition();
        adminCondition.setStartTime(new Date(1_000));
        adminCondition.setEndTime(new Date(2_000));
        adminCondition.setUsername("requested-user");
        service.doConditionPreProcessing(adminCondition);
        assertEquals("requested-user", adminCondition.getUsername());
        when(properties.getRecordLogLimit()).thenReturn(10);
        service.list();
        verify(mapper).selectLimit(null, 10);
    }

    @Test
    public void testInvalidConditionAndCleanHistory() {
        RecordLogQueryCondition condition = new RecordLogQueryCondition();
        condition.setStartTime(new Date(2_000));
        condition.setEndTime(new Date(1_000));
        assertThrows(ValidFailException.class, () -> service.doConditionPreProcessing(condition));
        when(properties.getOnlyCleanDays()).thenReturn(1);
        assertThrows(ValidFailException.class, () -> service.cleanHistory(new Date()));
        when(mapper.deleteByBefore(any(Date.class))).thenReturn(1);
        assertTrue(service.cleanHistory(new Date(System.currentTimeMillis() - 3L * 24 * 60 * 60 * 1000)));
    }

    @Test
    public void testCleanHistoryReturnsFalseWhenNothingIsDeleted() {
        when(properties.getOnlyCleanDays()).thenReturn(1);
        when(mapper.deleteByBefore(any(Date.class))).thenReturn(0);
        assertFalse(service.cleanHistory(new Date(System.currentTimeMillis() - 3L * 24 * 60 * 60 * 1000)));
    }
}
