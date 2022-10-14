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


import java.sql.Timestamp;
import javax.annotation.Resource;
import org.apache.shenyu.admin.AbstractSpringIntegrationTest;
import org.apache.shenyu.admin.model.entity.TagRelationDO;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.greaterThan;

public class TagRelationMapperTest extends AbstractSpringIntegrationTest {

    @Resource
    private TagRelationMapper tagRelationMapper;

    @Test
    public void testInsert() {
        TagRelationDO record = buildTagRelationDO();
        int count = tagRelationMapper.insert(record);
        assertThat(count, greaterThan(0));

        int delete = tagRelationMapper.deleteByPrimaryKey(record.getId());
        assertThat(delete, equalTo(1));
    }

    @Test
    public void testInsertSelective() {
        TagRelationDO record = buildTagRelationDO();
        int count = tagRelationMapper.insertSelective(record);
        assertThat(count, greaterThan(0));
        int delete = tagRelationMapper.deleteByPrimaryKey(record.getId());
        assertThat(delete, equalTo(1));
    }

    @Test
    public void testDeleteByPrimaryKey() {
        TagRelationDO record = buildTagRelationDO();
        int count = tagRelationMapper.insertSelective(record);
        assertThat(count, greaterThan(0));
        int delete = tagRelationMapper.deleteByPrimaryKey(record.getId());
        assertThat(delete, equalTo(1));
    }

    @Test
    public void testSelectByPrimaryKey() {
        TagRelationDO record = buildTagRelationDO();
        int count = tagRelationMapper.insertSelective(record);
        assertThat(count, greaterThan(0));
        TagRelationDO tagRelationDO = tagRelationMapper.selectByPrimaryKey(record.getId());
        assertThat(tagRelationDO != null, equalTo(true));
    }

    @Test
    public void testUpdateByPrimaryKey() {
        TagRelationDO record = buildTagRelationDO();
        int count = tagRelationMapper.insertSelective(record);
        assertThat(count, greaterThan(0));
        record.setTagId("2222222");
        tagRelationMapper.updateByPrimaryKey(record);
        TagRelationDO tagRelationDO = tagRelationMapper.selectByPrimaryKey(record.getId());
        assertThat(tagRelationDO.getTagId().equals("2222222"), equalTo(true));
    }

    private TagRelationDO buildTagRelationDO() {
        Timestamp now = new Timestamp(System.currentTimeMillis());
        String id = UUIDUtils.getInstance().generateShortUuid();
        return TagRelationDO.builder()
                .id(id)
                .tagId("122")
                .apiId("123")
                .dateCreated(now)
                .dateUpdated(now)
                .build();
    }
}
