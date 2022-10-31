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
import org.apache.shenyu.admin.model.entity.AppAuthDO;
import org.apache.shenyu.admin.model.query.AppAuthQuery;
import org.apache.shenyu.common.utils.SignUtils;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import javax.annotation.Resource;
import java.sql.Timestamp;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test cases for AppAuthMapper.
 */
public final class AppAuthMapperTest extends AbstractSpringIntegrationTest {

    @Resource
    private AppAuthMapper appAuthMapper;

    private final AppAuthDO appAuthDO = buildAppAuthDO();

    @BeforeEach
    public void before() {
        int count = appAuthMapper.insert(appAuthDO);
        assertEquals(1, count);
    }

    @Test
    public void testInsertSelective() {
        AppAuthDO newAppAuthDO = buildAppAuthDO();
        int count = appAuthMapper.insertSelective(newAppAuthDO);
        assertEquals(1, count);
    }

    @Test
    public void testSelectById() {
        AppAuthDO selectAppAuthDO = appAuthMapper.selectById(appAuthDO.getId());
        assertNotNull(selectAppAuthDO);
    }

    @Test
    public void testByQuery() {
        AppAuthQuery appAuthQuery = new AppAuthQuery();
        List<AppAuthDO> appAuthDOsWithQuery = appAuthMapper.selectByQuery(appAuthQuery);
        assertThat(appAuthDOsWithQuery.size(), greaterThan(0));

        List<AppAuthDO> appAuthDOsWithoutQuery = appAuthMapper.selectByQuery(null);
        assertThat(appAuthDOsWithoutQuery.size(), greaterThan(0));
    }

    @Test
    public void testSelectAll() {
        List<AppAuthDO> appAuthDOs = appAuthMapper.selectAll();
        assertThat(appAuthDOs.size(), greaterThan(0));
    }

    @Test
    public void testFindByAppKey() {
        AppAuthDO findByAppKeyAppAuthDO = appAuthMapper.findByAppKey(appAuthDO.getAppKey());
        assertNotNull(findByAppKeyAppAuthDO);
    }

    @Test
    public void testCountByQuery() {
        AppAuthQuery appAuthQuery = new AppAuthQuery();
        appAuthQuery.setPhone(appAuthDO.getPhone());
        appAuthQuery.setAppKey(appAuthDO.getAppKey());
        int count = appAuthMapper.countByQuery(appAuthQuery);
        assertEquals(1, count);
    }

    @Test
    public void testUpdate() {
        int count = appAuthMapper.update(appAuthDO);
        assertEquals(1, count);
    }

    @Test
    public void testUpdateEnable() {
        boolean hasEnable = !appAuthDO.getEnabled();
        appAuthDO.setEnabled(hasEnable);
        int count = appAuthMapper.updateEnable(appAuthDO);
        assertEquals(1, count);

        AppAuthDO selectAppAuthDO = appAuthMapper.selectById(appAuthDO.getId());
        assertEquals(hasEnable, selectAppAuthDO.getEnabled());
    }

    @Test
    public void testUpdateAppSecretByAppKey() {
        String appSecret = SignUtils.getInstance().generateKey();
        appAuthDO.setAppSecret(appSecret);
        int count = appAuthMapper.updateAppSecretByAppKey(appAuthDO.getAppKey(), appSecret);
        assertEquals(1, count);

        AppAuthDO selectAppAuthDO = appAuthMapper.selectById(appAuthDO.getId());
        assertEquals(appSecret, selectAppAuthDO.getAppSecret());
    }

    @Test
    public void testUpdateSelective() {
        int count = appAuthMapper.updateSelective(appAuthDO);
        assertEquals(1, count);
    }

    @Test
    public void testDelete() {
        int countDelete = appAuthMapper.delete(appAuthDO.getId());
        assertEquals(1, countDelete);
    }

    private AppAuthDO buildAppAuthDO() {
        Timestamp now = new Timestamp(System.currentTimeMillis());
        return AppAuthDO.builder()
                .id(UUIDUtils.getInstance().generateShortUuid())
                .appKey(SignUtils.getInstance().generateKey())
                .appSecret(SignUtils.getInstance().generateKey())
                .extInfo("{\"extInfo\":\"json\"}")
                .open(true)
                .enabled(false)
                .phone("18800000000")
                .userId(UUIDUtils.getInstance().generateShortUuid())
                .dateCreated(now)
                .dateUpdated(now)
                .build();
    }
}
