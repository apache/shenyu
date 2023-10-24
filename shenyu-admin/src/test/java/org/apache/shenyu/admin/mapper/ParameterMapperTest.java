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
import org.apache.shenyu.admin.model.entity.ParameterDO;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.Test;

import javax.annotation.Resource;
import java.sql.Timestamp;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test cases for ParameterMapper.
 */
public class ParameterMapperTest extends AbstractSpringIntegrationTest {

    @Resource
    private ParameterMapper parameterMapper;

    private ParameterDO buildParameterDO() {
        Timestamp now = new Timestamp(System.currentTimeMillis());
        String id = UUIDUtils.getInstance().generateShortUuid();
        return ParameterDO.builder()
                .id(id)
                .apiId("testApi")
                .modelId("111")
                .type(1)
                .name("test")
                .paramDesc("test_desc")
                .required(true)
                .ext("test")
                .dateCreated(now)
                .dateUpdated(now)
                .build();
    }

    @Test
    public void testInsertAndDelete() {
        ParameterDO parameterDO = buildParameterDO();
        assertEquals(parameterMapper.insert(parameterDO), 1);
        assertEquals(parameterMapper.deleteByPrimaryKey(parameterDO.getId()), 1);
    }

    @Test
    public void testSelectByPrimaryKey() {
        ParameterDO parameterDO = buildParameterDO();
        parameterMapper.insert(parameterDO);
        assertNotNull(parameterMapper.selectByPrimaryKey(parameterDO.getId()));
        parameterMapper.deleteByPrimaryKey(parameterDO.getId());
    }

    @Test
    public void testInsertSelective() {
        ParameterDO parameterDO = buildParameterDO();
        assertEquals(parameterMapper.insertSelective(parameterDO), 1);
        parameterMapper.deleteByPrimaryKey(parameterDO.getId());
    }

    @Test
    public void testUpdateByPrimaryKey() {
        ParameterDO parameterDO = buildParameterDO();
        parameterMapper.insert(parameterDO);
        parameterDO.setApiId("updateApi");
        parameterDO.setExt("update");
        parameterDO.setDateUpdated(new Timestamp(System.currentTimeMillis()));
        assertEquals(parameterMapper.updateByPrimaryKey(parameterDO), 1);
        parameterMapper.deleteByPrimaryKey(parameterDO.getId());
    }

    @Test
    public void testUpdateSelective() {
        ParameterDO parameterDO = buildParameterDO();
        parameterMapper.insert(parameterDO);
        parameterDO.setApiId("updateApi");
        parameterDO.setExt("selective update");
        parameterDO.setDateUpdated(new Timestamp(System.currentTimeMillis()));
        assertEquals(parameterMapper.updateByPrimaryKeySelective(parameterDO), 1);
        parameterMapper.deleteByPrimaryKey(parameterDO.getId());
    }
}
