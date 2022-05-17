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
import org.apache.shenyu.admin.model.entity.BaseDO;
import org.apache.shenyu.admin.model.entity.RoleDO;
import org.apache.shenyu.admin.model.query.RoleQuery;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import javax.annotation.Resource;
import java.sql.Timestamp;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * Test cases for RoleMapper.
 */
public final class RoleMapperTest extends AbstractSpringIntegrationTest {

    @Resource
    private RoleMapper mapper;

    @Test
    public void testSelectById() {
        RoleDO roleDO = buildRoleDO();
        int insert = mapper.insert(roleDO);
        assertThat(insert, equalTo(1));

        RoleDO result = mapper.selectById(roleDO.getId());
        assertThat(roleDO, equalTo(result));
    }

    @Test
    public void testSelectByQuery() {
        RoleDO roleDO = buildRoleDO();
        int insert = mapper.insert(roleDO);
        assertThat(insert, equalTo(1));

        RoleQuery query = new RoleQuery();
        query.setRoleName(roleDO.getRoleName());
        List<RoleDO> roleDOS = mapper.selectByQuery(query);
        assertThat(roleDOS.size(), equalTo(1));
        assertThat(roleDOS.get(0), equalTo(roleDO));

    }

    @Test
    public void testFindByRoleName() {
        RoleDO roleDO = buildRoleDO();
        int insert = mapper.insert(roleDO);
        assertThat(insert, equalTo(1));

        RoleDO byRoleName = mapper.findByRoleName(roleDO.getRoleName());
        assertThat(byRoleName, equalTo(roleDO));
    }

    @Test
    public void testCountByQuery() {
        RoleDO roleDO = buildRoleDO();
        int insert = mapper.insert(roleDO);
        assertThat(insert, equalTo(1));

        RoleQuery query = new RoleQuery();
        query.setRoleName(roleDO.getRoleName());
        Integer count = mapper.countByQuery(query);
        assertThat(count, equalTo(1));
    }

    @Test
    public void testInsert() {
        RoleDO roleDO = buildRoleDO();
        int insert = mapper.insert(roleDO);
        assertThat(insert, equalTo(1));
    }

    @Test
    public void testInsertSelective() {
        RoleDO roleDO = buildRoleDO();
        int insert = mapper.insertSelective(roleDO);
        assertThat(insert, equalTo(1));
    }

    @Test
    public void testUpdate() {
        RoleDO roleDO = buildRoleDO();
        int insert = mapper.insert(roleDO);
        assertThat(insert, equalTo(1));

        roleDO.setRoleName("updated-role");
        int update = mapper.update(roleDO);
        assertThat(update, equalTo(1));
        RoleDO result = mapper.selectById(roleDO.getId());
        assertThat(result, equalTo(roleDO));
    }

    @Test
    public void testUpdateSelective() {
        RoleDO roleDO = buildRoleDO();
        int insert = mapper.insert(roleDO);
        assertThat(insert, equalTo(1));

        roleDO.setRoleName("updated-role");
        String description = roleDO.getDescription();
        roleDO.setDescription(null);
        int update = mapper.updateSelective(roleDO);
        assertThat(update, equalTo(1));
        RoleDO result = mapper.selectById(roleDO.getId());
        roleDO.setDescription(description);
        assertThat(result, equalTo(roleDO));
    }

    @Test
    public void testDelete() {
        RoleDO roleDO = buildRoleDO();
        int insert = mapper.insertSelective(roleDO);
        assertThat(insert, equalTo(1));

        int delete = mapper.delete(Collections.singletonList(roleDO.getId()));
        assertThat(delete, equalTo(1));

        RoleDO result = mapper.selectById(roleDO.getId());
        assertNull(result);
    }

    @Test
    public void testSelectAll() {
        RoleDO roleDO = buildRoleDO();
        int insert = mapper.insert(roleDO);
        assertThat(insert, equalTo(1));

        List<RoleDO> roleDOS = mapper.selectAll();
        assertThat(roleDOS.size(), equalTo(1));
    }

    @AfterEach
    public void resetDB() {
        List<String> ids = mapper.selectAll().stream().map(BaseDO::getId).collect(Collectors.toList());
        if (!ids.isEmpty()) {
            mapper.delete(ids);
        }
    }

    private RoleDO buildRoleDO() {
        Timestamp now = new Timestamp(System.currentTimeMillis());
        return RoleDO.builder()
                .id(UUIDUtils.getInstance().generateShortUuid())
                .roleName("test-role")
                .description("test role")
                .dateUpdated(now)
                .dateCreated(now)
                .build();
    }
}
