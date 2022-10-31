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
import org.apache.shenyu.admin.model.entity.ApiRuleRelationDO;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import javax.annotation.Resource;
import java.sql.Timestamp;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test cases for ApiRuleRelationMapper.
 */
public final class ApiRuleRelationMapperTest extends AbstractSpringIntegrationTest {

    @Resource
    private ApiRuleRelationMapper apiRuleRelationMapper;

    private final ApiRuleRelationDO apiRuleRelationDO = buildApiRuleRelationDO();

    @BeforeEach
    public void before() {
        int count = apiRuleRelationMapper.insert(apiRuleRelationDO);
        assertEquals(1, count);
    }

    @Test
    public void testInsert() {
        ApiRuleRelationDO newApiRuleRelationDO = buildApiRuleRelationDO();
        int count = apiRuleRelationMapper.insert(newApiRuleRelationDO);
        assertEquals(1, count);
    }

    @Test
    public void testInsertSelective() {
        ApiRuleRelationDO newApiRuleRelationDO = buildApiRuleRelationDO();
        int count = apiRuleRelationMapper.insertSelective(newApiRuleRelationDO);
        assertEquals(1, count);
    }

    @Test
    public void testSelectByPrimaryKey() {
        ApiRuleRelationDO apiRuleRelationDO = apiRuleRelationMapper.selectByPrimaryKey(this.apiRuleRelationDO.getId());
        assertNotNull(apiRuleRelationDO);
    }

    @Test
    public void testUpdateByPrimaryKeySelective() {
        Timestamp now = new Timestamp(System.currentTimeMillis());
        this.apiRuleRelationDO.setDateUpdated(now);
        this.apiRuleRelationDO.setApiId(UUIDUtils.getInstance().generateShortUuid());
        this.apiRuleRelationDO.setRuleId(UUIDUtils.getInstance().generateShortUuid());
        final int count = apiRuleRelationMapper.updateByPrimaryKeySelective(this.apiRuleRelationDO);
        assertEquals(1, count);
    }

    @Test
    public void testUpdateByPrimaryKey() {
        Timestamp now = new Timestamp(System.currentTimeMillis());
        this.apiRuleRelationDO.setDateUpdated(now);
        this.apiRuleRelationDO.setApiId(UUIDUtils.getInstance().generateShortUuid());
        this.apiRuleRelationDO.setRuleId(UUIDUtils.getInstance().generateShortUuid());
        final int count = apiRuleRelationMapper.updateByPrimaryKeySelective(this.apiRuleRelationDO);
        assertEquals(1, count);
    }

    @Test
    public void testDeleteByPrimaryKey() {
        final int count = apiRuleRelationMapper.deleteByPrimaryKey(this.apiRuleRelationDO.getId());
        assertEquals(1, count);
    }

    private ApiRuleRelationDO buildApiRuleRelationDO() {
        Timestamp now = new Timestamp(System.currentTimeMillis());
        ApiRuleRelationDO apiRuleRelationDO = new ApiRuleRelationDO();
        apiRuleRelationDO.setId(UUIDUtils.getInstance().generateShortUuid());
        apiRuleRelationDO.setApiId(UUIDUtils.getInstance().generateShortUuid());
        apiRuleRelationDO.setRuleId(UUIDUtils.getInstance().generateShortUuid());
        apiRuleRelationDO.setDateCreated(now);
        apiRuleRelationDO.setDateUpdated(now);
        return apiRuleRelationDO;
    }
}
