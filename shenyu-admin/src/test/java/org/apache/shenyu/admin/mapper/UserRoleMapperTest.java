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

package org.apache.shenyu.admin.mapper;

import org.apache.shenyu.admin.AbstractSpringIntegrationTest;
import org.apache.shenyu.admin.model.entity.UserRoleDO;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import javax.annotation.Resource;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test cases for UserRoleMapper.
 */
public class UserRoleMapperTest extends AbstractSpringIntegrationTest {

    private static final String ADMIN_ROLE_ID = "1346358560427216896";

    private static final String DEFAULT_ROLE_ID = "1385482862971723776";

    private static final String ADMIN_USER_ID = "1";

    @Resource
    private UserRoleMapper userRoleMapper;

    private final UserRoleDO userRoleDO = buildUserRoleDO(ADMIN_USER_ID, ADMIN_ROLE_ID);

    @BeforeEach
    public void before() {
        userRoleMapper.insert(userRoleDO);
    }

    @Test
    public void testSelectById() {
        assertNotNull(userRoleMapper.selectById(userRoleDO.getId()));
    }

    @Test
    public void testFindByUserId() {
        assertNotNull(userRoleMapper.findByUserId(userRoleDO.getUserId()));
    }

    @Test
    public void testInsertAndDelete() {
        UserRoleDO newUserRoleDo = buildUserRoleDO("2", DEFAULT_ROLE_ID);
        assertEquals(userRoleMapper.insert(newUserRoleDo), 1);
        assertEquals(userRoleMapper.delete(newUserRoleDo.getId()), 1);
    }

    @Test
    public void testInsertBatch() {
        List<UserRoleDO> userRoleList = new ArrayList<>();
        userRoleList.add(buildUserRoleDO("2", DEFAULT_ROLE_ID));
        userRoleList.add(buildUserRoleDO("3", DEFAULT_ROLE_ID));
        assertEquals(userRoleMapper.insertBatch(userRoleList), 2);
        userRoleList.forEach(u -> userRoleMapper.delete(u.getId()));
    }

    @Test
    public void testInsertSelective() {
        UserRoleDO newUserRoleDo = buildUserRoleDO("2", DEFAULT_ROLE_ID);
        assertEquals(userRoleMapper.insertSelective(newUserRoleDo), 1);
        userRoleMapper.delete(newUserRoleDo.getId());
    }

    @Test
    public void testDeleteByUserId() {
        UserRoleDO newUserRoleDo = buildUserRoleDO("2", DEFAULT_ROLE_ID);
        userRoleMapper.insert(newUserRoleDo);
        assertEquals(userRoleMapper.deleteByUserId(newUserRoleDo.getUserId()), 1);
    }

    @Test
    public void testDeleteByUserIdList() {
        List<String> userIdList = Stream.of("2", "3").collect(Collectors.toList());
        userIdList.forEach(i -> userRoleMapper.insert(buildUserRoleDO(i, DEFAULT_ROLE_ID)));
        assertEquals(userRoleMapper.deleteByUserIdList(userIdList), 2);
    }

    @AfterEach
    public void after() {
        userRoleMapper.delete(userRoleDO.getId());
    }

    private UserRoleDO buildUserRoleDO(final String userId, final String roleId) {
        Timestamp now = new Timestamp(System.currentTimeMillis());
        return UserRoleDO.builder()
                .id(UUIDUtils.getInstance().generateShortUuid())
                .userId(userId)
                .roleId(roleId)
                .dateCreated(now)
                .dateUpdated(now)
                .build();
    }
}
