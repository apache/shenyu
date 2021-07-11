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
import org.apache.shenyu.admin.model.entity.DashboardUserDO;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.DashboardUserQuery;
import org.apache.shenyu.admin.utils.AesUtils;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.Test;

import javax.annotation.Resource;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.List;

import static org.hamcrest.Matchers.comparesEqualTo;
import static org.hamcrest.Matchers.greaterThan;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;

/**
 * test case for DashboardUserMapper.
 */
public final class DashboardUserMapperTest extends AbstractSpringIntegrationTest {

    @Resource
    private DashboardUserMapper dashboardUserMapper;

    @Test
    public void testInsert() {
        DashboardUserDO record = buildDashboardUserDO();
        int count = dashboardUserMapper.insert(record);
        assertThat(count, comparesEqualTo(1));

        int delete = dashboardUserMapper.delete(record.getId());
        assertEquals(1, delete);
    }

    @Test
    public void testInsertSelective() {
        DashboardUserDO record = buildDashboardUserDO();
        int count = dashboardUserMapper.insertSelective(record);
        assertThat(count, comparesEqualTo(1));

        int delete = dashboardUserMapper.delete(record.getId());
        assertEquals(1, delete);
    }

    @Test
    public void testSelectById() {
        DashboardUserDO record = buildDashboardUserDO();
        int count = dashboardUserMapper.insert(record);
        assertThat(count, comparesEqualTo(1));
        DashboardUserDO result = dashboardUserMapper.selectById(record.getId());
        assertNotNull(result);

        int delete = dashboardUserMapper.delete(record.getId());
        assertEquals(1, delete);
    }

    @Test
    public void testFindByQuery() {
        DashboardUserDO record = buildDashboardUserDO();
        record.setUserName("adminshenyu");
        int count = dashboardUserMapper.insert(record);
        assertThat(count, comparesEqualTo(1));

        DashboardUserDO result = dashboardUserMapper.findByQuery(record.getUserName(), record.getPassword());
        assertNotNull(result);

        int delete = dashboardUserMapper.delete(record.getId());
        assertEquals(1, delete);
    }

    @Test
    public void testSelectByQuery() {
        DashboardUserDO record = buildDashboardUserDO();
        int count = dashboardUserMapper.insert(record);
        assertThat(count, comparesEqualTo(1));

        DashboardUserQuery query = new DashboardUserQuery();
        PageParameter pageParameter = new PageParameter();
        query.setUserName("adminTest");
        query.setPageParameter(pageParameter);
        List<DashboardUserDO> result = dashboardUserMapper.selectByQuery(query);
        assertThat(result.size(), greaterThan(0));

        int delete = dashboardUserMapper.delete(record.getId());
        assertEquals(1, delete);
    }

    @Test
    public void testCountByQuery() {
        DashboardUserDO record = buildDashboardUserDO();
        int count = dashboardUserMapper.insert(record);
        assertThat(count, comparesEqualTo(1));

        DashboardUserQuery query = new DashboardUserQuery();
        query.setUserName("adminTest");
        int result = dashboardUserMapper.countByQuery(query);
        assertThat(result, greaterThan(0));

        int delete = dashboardUserMapper.delete(record.getId());
        assertEquals(1, delete);
    }

    @Test
    public void testUpdate() {
        DashboardUserDO record = buildDashboardUserDO();
        int count = dashboardUserMapper.insert(record);
        assertThat(count, comparesEqualTo(1));

        record.setUserName("adminUpdate");
        int result = dashboardUserMapper.update(record);
        assertThat(result, comparesEqualTo(1));

        int delete = dashboardUserMapper.delete(record.getId());
        assertEquals(1, delete);
    }

    @Test
    public void testUpdateSelective() {
        DashboardUserDO record = buildDashboardUserDO();
        int count = dashboardUserMapper.insert(record);
        assertThat(count, comparesEqualTo(1));

        record.setUserName("adminUpdate");
        int result = dashboardUserMapper.updateSelective(record);
        assertThat(result, comparesEqualTo(1));

        int delete = dashboardUserMapper.delete(record.getId());
        assertEquals(1, delete);
    }

    @Test
    public void testDelete() {
        DashboardUserDO record = buildDashboardUserDO();
        int count = dashboardUserMapper.insert(record);
        assertThat(count, comparesEqualTo(1));

        int result = dashboardUserMapper.delete(record.getId());
        assertThat(result, comparesEqualTo(1));
    }

    private DashboardUserDO buildDashboardUserDO() {
        String aseKey = "2095132720951327";
        String iv = "6075877187097700";

        Timestamp now = Timestamp.valueOf(LocalDateTime.now());
        return DashboardUserDO.builder()
                .id(UUIDUtils.getInstance().generateShortUuid())
                .userName("adminTest")
                .password(AesUtils.aesEncryption("123456", aseKey, iv))
                .enabled(true)
                .role(1)
                .dateCreated(now)
                .dateUpdated(now)
                .build();
    }
}
