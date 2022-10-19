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
import org.apache.shenyu.admin.model.entity.ModelDO;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.Test;

import javax.annotation.Resource;

import java.sql.Timestamp;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.greaterThan;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

public class ModelMapperTest extends AbstractSpringIntegrationTest {

    @Resource
    private ModelMapper modelMapper;

    private ModelDO buildModelDO() {
        Timestamp now = new Timestamp(System.currentTimeMillis());
        String id = UUIDUtils.getInstance().generateShortUuid();
        return ModelDO.builder()
                .id(id)
                .name("111")
                .modelDesc("test_modelDesc")
                .dateCreated(now)
                .dateUpdated(now)
                .build();
    }

    @Test
    public void testInsert() {
        ModelDO record = buildModelDO();
        int count = modelMapper.insert(record);
        assertThat(count, greaterThan(0));

        int delete = modelMapper.deleteByPrimaryKey(record.getId());
        assertThat(delete, equalTo(1));
    }

    @Test
    public void testInsertSelective() {
        ModelDO record = buildModelDO();
        int count = modelMapper.insertSelective(record);
        assertThat(count, greaterThan(0));

        int delete = modelMapper.deleteByPrimaryKey(record.getId());
        assertEquals(delete, 1);
    }

    @Test
    public void testSelectByPrimaryKey() {
        ModelDO record = buildModelDO();
        int count = modelMapper.insert(record);
        assertThat(count, greaterThan(0));

        ModelDO modelDO = modelMapper.selectByPrimaryKey(record.getId());
        assertNotNull(modelDO);
    }

    @Test
    public void testUpdateByPrimaryKey() {
        ModelDO record = buildModelDO();
        int count = modelMapper.insert(record);
        assertThat(count, greaterThan(0));

        record.setName("update");
        int updateCount = modelMapper.updateByPrimaryKey(record);
        assertThat(updateCount, greaterThan(0));

        ModelDO modelDO = modelMapper.selectByPrimaryKey(record.getId());
        assertEquals(modelDO.getName(), "update");
    }

    @Test
    public void testDeleteByPrimaryKey() {
        ModelDO record = buildModelDO();
        int count = modelMapper.insertSelective(record);
        assertThat(count, greaterThan(0));
        int delete = modelMapper.deleteByPrimaryKey(record.getId());
        assertEquals(delete, 1);
    }

}
