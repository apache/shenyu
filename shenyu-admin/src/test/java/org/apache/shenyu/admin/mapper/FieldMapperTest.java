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
import org.apache.shenyu.admin.model.entity.FieldDO;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.Test;

import javax.annotation.Resource;
import java.sql.Timestamp;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

public class FieldMapperTest extends AbstractSpringIntegrationTest {

    @Resource
    private FieldMapper fieldMapper;

    private FieldDO buildFieldDO() {
        Timestamp now = new Timestamp(System.currentTimeMillis());
        String id = UUIDUtils.getInstance().generateShortUuid();
        String modelId = UUIDUtils.getInstance().generateShortUuid();
        String selfModelId = UUIDUtils.getInstance().generateShortUuid();
        return FieldDO.builder()
                .id(id)
                .name("name")
                .modelId(modelId)
                .selfModelId(selfModelId)
                .fieldDesc("fieldDesc")
                .required(true)
                .ext("ext")
                .dateCreated(now)
                .dateUpdated(now)
                .build();
    }

    @Test
    public void testInsert() {
        FieldDO record = buildFieldDO();
        int count = fieldMapper.insert(record);
        assertThat(count, greaterThan(0));

        int delete = fieldMapper.deleteByPrimaryKey(record.getId());
        assertEquals(delete, 1);
    }

    @Test
    public void testInsertSelective() {
        FieldDO record = buildFieldDO();
        int count = fieldMapper.insertSelective(record);
        assertThat(count, greaterThan(0));

        int delete = fieldMapper.deleteByPrimaryKey(record.getId());
        assertEquals(delete, 1);
    }

    @Test
    public void testSelectByPrimaryKey() {
        FieldDO record = buildFieldDO();
        int count = fieldMapper.insert(record);
        assertThat(count, greaterThan(0));

        FieldDO fieldDO = fieldMapper.selectByPrimaryKey(record.getId());
        assertNotNull(fieldDO);
    }

    @Test
    public void testUpdateByPrimaryKey() {
        FieldDO record = buildFieldDO();
        int count = fieldMapper.insert(record);
        assertThat(count, greaterThan(0));

        record.setName("update");
        int updateCount = fieldMapper.updateByPrimaryKey(record);
        assertThat(updateCount, greaterThan(0));

        FieldDO fieldDO = fieldMapper.selectByPrimaryKey(record.getId());
        assertEquals(fieldDO.getName(), "update");
    }

    @Test
    public void testDeleteByPrimaryKey() {
        FieldDO record = buildFieldDO();
        int count = fieldMapper.insertSelective(record);
        assertThat(count, greaterThan(0));
        int delete = fieldMapper.deleteByPrimaryKey(record.getId());
        assertEquals(delete, 1);
    }

}
