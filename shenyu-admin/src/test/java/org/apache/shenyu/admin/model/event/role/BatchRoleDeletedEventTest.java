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
import org.apache.shenyu.admin.model.entity.BaseDO;
import org.apache.shenyu.admin.model.entity.RoleDO;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * test cast for {@link BatchRoleDeletedEvent}.
 */
public final class BatchRoleDeletedEventTest {

    private BatchRoleDeletedEvent batchRoleDeletedEventTest;

    private BatchRoleDeletedEvent batchRoleDeletedEventEmptySourceTest;

    private List<RoleDO> roleDOList;

    private List<RoleDO> emptyRoleDOList;

    @BeforeEach
    public void setUp() {
        roleDOList = batchInitRoleDOList();
        emptyRoleDOList = new ArrayList<>();
        batchRoleDeletedEventTest = new BatchRoleDeletedEvent(roleDOList, "test-op");
        batchRoleDeletedEventEmptySourceTest = new BatchRoleDeletedEvent(emptyRoleDOList, "test-op");
    }

    @Test
    public void testBuildContext() {
        String expectedSelector = roleDOList
                .stream()
                .map(RoleDO::getRoleName)
                .collect(Collectors.joining(","));
        String expectedStr = String.format("the role[%s] is %s", expectedSelector,
                StringUtils.lowerCase(batchRoleDeletedEventTest.getType().getType().toString()));
        assertEquals(expectedStr, batchRoleDeletedEventTest.buildContext());

        String expectedEmptySelector = emptyRoleDOList
                .stream()
                .map(RoleDO::getRoleName)
                .collect(Collectors.joining(","));
        String expectedEmptyStr = String.format("the role[%s] is %s", expectedEmptySelector,
                StringUtils.lowerCase(batchRoleDeletedEventEmptySourceTest.getType().getType().toString()));
        assertEquals(expectedEmptyStr, batchRoleDeletedEventEmptySourceTest.buildContext());
    }

    @Test
    public void testGetRoles() {
        List<RoleDO> roles = batchRoleDeletedEventTest.getRoles();
        assertEquals(roles, roleDOList);

        List<RoleDO> emptyRoles = batchRoleDeletedEventEmptySourceTest.getRoles();
        assertEquals(emptyRoles, emptyRoleDOList);
    }

    @Test
    public void testGetDeletedIds() {
        List<String> ids = roleDOList
                .stream()
                .map(BaseDO::getId)
                .collect(Collectors.toList());
        assertEquals(ids, batchRoleDeletedEventTest.getDeletedIds());

        List<String> emptyIds = emptyRoleDOList
                .stream()
                .map(BaseDO::getId)
                .collect(Collectors.toList());
        assertEquals(emptyIds, batchRoleDeletedEventEmptySourceTest.getDeletedIds());
    }

    private List<RoleDO> batchInitRoleDOList() {
        int defaultInsertNum = 10;
        List<RoleDO> roleDOList = new ArrayList<>();
        for (int i = 0; i < defaultInsertNum; i++) {
            Timestamp now = new Timestamp(System.currentTimeMillis());
            String id = UUIDUtils.getInstance().generateShortUuid();
            RoleDO roleDO = RoleDO.builder()
                    .id(id)
                    .roleName("test-" + i)
                    .description("role-test-description")
                    .dateCreated(now)
                    .dateUpdated(now)
                    .build();
            roleDOList.add(roleDO);
        }
        return roleDOList;
    }
}
