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

import jakarta.annotation.Resource;
import org.apache.shenyu.admin.AbstractSpringIntegrationTest;
import org.apache.shenyu.admin.model.entity.ScalePolicyDO;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.Test;

import java.util.Date;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.time.ZoneId;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test cases for ScalePolicyMapper.
 */
public final class ScalePolicyMapperTest extends AbstractSpringIntegrationTest {

    @Resource
    private ScalePolicyMapper scalePolicyMapper;

    @Test
    void deleteByPrimaryKey() {
        ScalePolicyDO scalePolicyDO = buildScalePolicyDO();
        int insert = scalePolicyMapper.insertSelective(scalePolicyDO);
        assertThat(insert, equalTo(1));

        int delete = scalePolicyMapper.deleteByPrimaryKey(scalePolicyDO.getId());
        assertThat(delete, equalTo(1));

        ScalePolicyDO result = scalePolicyMapper.selectByPrimaryKey(scalePolicyDO.getId());
        assertThat(result, equalTo(null));
    }

    @Test
    void insert() {
        ScalePolicyDO scalePolicyDO = buildScalePolicyDO();
        int insert = scalePolicyMapper.insert(scalePolicyDO);
        assertThat(insert, equalTo(1));
    }

    @Test
    void insertSelective() {
        ScalePolicyDO scalePolicyDO = buildScalePolicyDO();
        int insert = scalePolicyMapper.insertSelective(scalePolicyDO);
        assertThat(insert, equalTo(1));
    }

    @Test
    void selectByPrimaryKey() {
        ScalePolicyDO resultDO = scalePolicyMapper.selectByPrimaryKey("1");
        // assertNotNull(resultDO);
        assertEquals("1", resultDO.getId());
        assertEquals(1, resultDO.getSort());
        assertEquals(0, resultDO.getStatus());
        assertEquals(10, resultDO.getNum());
    }

    @Test
    void updateByPrimaryKeySelective() {
        ScalePolicyDO scalePolicyDO = buildScalePolicyDO();
        int insert = scalePolicyMapper.insert(scalePolicyDO);
        assertThat(insert, equalTo(1));

        scalePolicyDO.setNum(14);
        Date beginTime = scalePolicyDO.getBeginTime();
        scalePolicyDO.setBeginTime(null);
        int update = scalePolicyMapper.updateByPrimaryKeySelective(scalePolicyDO);
        assertThat(update, equalTo(1));
        ScalePolicyDO result = scalePolicyMapper.selectByPrimaryKey(scalePolicyDO.getId());
        scalePolicyDO.setBeginTime(beginTime);
        assertThat(result, equalTo(scalePolicyDO));
    }

    @Test
    void updateByPrimaryKey() {
        ScalePolicyDO scalePolicyDO = buildScalePolicyDO();
        int insert = scalePolicyMapper.insert(scalePolicyDO);
        assertThat(insert, equalTo(1));

        scalePolicyDO.setNum(16);
        int update = scalePolicyMapper.updateByPrimaryKey(scalePolicyDO);
        assertThat(update, equalTo(1));
        ScalePolicyDO result = scalePolicyMapper.selectByPrimaryKey(scalePolicyDO.getId());
        assertThat(result, equalTo(scalePolicyDO));
    }

    private ScalePolicyDO buildScalePolicyDO() {
        Timestamp now = new Timestamp(System.currentTimeMillis());
        String id = UUIDUtils.getInstance().generateShortUuid();
        LocalDateTime begin = LocalDateTime.of(2024, 7, 31, 20, 0, 0, 0);
        LocalDateTime end = LocalDateTime.of(2024, 8, 11, 20, 0, 0, 0);
        Date date1 = Date.from(begin.atZone(ZoneId.systemDefault()).toInstant());
        Date date2 = Date.from(end.atZone(ZoneId.systemDefault()).toInstant());
        return ScalePolicyDO.builder()
                .id(id)
                .sort(1)
                .status(0)
                .num(10)
                .beginTime(date1)
                .endTime(date2)
                .dateCreated(now)
                .dateUpdated(now)
                .build();
    }
}
