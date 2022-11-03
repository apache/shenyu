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
import org.apache.shenyu.admin.model.entity.DetailDO;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.Test;

import javax.annotation.Resource;
import java.sql.Timestamp;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

public class DetailMapperTest extends AbstractSpringIntegrationTest {

    @Resource
    private DetailMapper detailMapper;

    private DetailDO buildDetailDO() {
        Timestamp now = new Timestamp(System.currentTimeMillis());
        String id = UUIDUtils.getInstance().generateShortUuid();
        String fieldId = UUIDUtils.getInstance().generateShortUuid();
        return DetailDO.builder()
                .id(id)
                .fieldId(fieldId)
                .example(true)
                .fieldValue("fieldValue")
                .valueDesc("valueDesc")
                .dateCreated(now)
                .dateUpdated(now)
                .build();
    }

    @Test
    public void testInsert() {
        DetailDO detailDO = buildDetailDO();
        int count = detailMapper.insert(detailDO);
        assertThat(count, greaterThan(0));

        int delete = detailMapper.deleteByPrimaryKey(detailDO.getId());
        assertEquals(delete, 1);
    }

    @Test
    public void testInsertSelective() {
        DetailDO detailDO = buildDetailDO();
        int count = detailMapper.insertSelective(detailDO);
        assertThat(count, greaterThan(0));

        int delete = detailMapper.deleteByPrimaryKey(detailDO.getId());
        assertEquals(delete, 1);
    }

    @Test
    public void testSelectByPrimaryKey() {
        DetailDO detailDO = buildDetailDO();
        int count = detailMapper.insert(detailDO);
        assertThat(count, greaterThan(0));

        DetailDO fieldDO = detailMapper.selectByPrimaryKey(detailDO.getId());
        assertNotNull(fieldDO);
    }

    @Test
    public void testUpdateByPrimaryKey() {
        DetailDO detailDO = buildDetailDO();
        int count = detailMapper.insert(detailDO);
        assertThat(count, greaterThan(0));

        detailDO.setValueDesc("update");
        int updateCount = detailMapper.updateByPrimaryKey(detailDO);
        assertThat(updateCount, greaterThan(0));

        DetailDO fieldDO = detailMapper.selectByPrimaryKey(detailDO.getId());
        assertEquals(fieldDO.getValueDesc(), "update");
    }

    @Test
    public void testDeleteByPrimaryKey() {
        DetailDO detailDO = buildDetailDO();
        int count = detailMapper.insertSelective(detailDO);
        assertThat(count, greaterThan(0));
        int delete = detailMapper.deleteByPrimaryKey(detailDO.getId());
        assertEquals(delete, 1);
    }

}
