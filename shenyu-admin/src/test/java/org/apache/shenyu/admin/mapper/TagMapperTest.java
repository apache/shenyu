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
import org.apache.shenyu.admin.model.entity.TagDO;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.greaterThan;

/**
 * Test cases for ShenyuDictMapper.
 */
public final class TagMapperTest extends AbstractSpringIntegrationTest {

    @Resource
    private TagMapper tagMapper;

    @Test
    public void testInsert() {
        TagDO record = buildTagDO();
        int count = tagMapper.insert(record);
        assertThat(count, greaterThan(0));

        int delete = tagMapper.deleteByPrimaryKey(record.getId());
        assertThat(delete, equalTo(1));
    }

    @Test
    public void testInsertSelective() {
        TagDO record = buildTagDO();
        int count = tagMapper.insertSelective(record);
        assertThat(count, greaterThan(0));
        int delete = tagMapper.deleteByPrimaryKey(record.getId());
        assertThat(delete, equalTo(1));
    }

    @Test
    public void testDeleteByPrimaryKey() {
        TagDO record = buildTagDO();
        int count = tagMapper.insertSelective(record);
        assertThat(count, greaterThan(0));
        int delete = tagMapper.deleteByPrimaryKey(record.getId());
        assertThat(delete, equalTo(1));
    }

    @Test
    public void testSelectByPrimaryKey() {
        TagDO record = buildTagDO();
        int count = tagMapper.insertSelective(record);
        assertThat(count, greaterThan(0));
        TagDO tagDO = tagMapper.selectByPrimaryKey(record.getId());
        assertThat(tagDO != null, equalTo(true));
    }

    @Test
    public void testUpdateByPrimaryKey() {
        TagDO record = buildTagDO();
        int count = tagMapper.insertSelective(record);
        assertThat(count, greaterThan(0));
        record.setTagDesc("2222222");
        tagMapper.updateByPrimaryKey(record);
        TagDO tagDO = tagMapper.selectByPrimaryKey(record.getId());
        assertThat(tagDO.getTagDesc().equals("2222222"), equalTo(true));
    }

    private TagDO buildTagDO() {
        Timestamp now = new Timestamp(System.currentTimeMillis());
        String id = UUIDUtils.getInstance().generateShortUuid();
        return TagDO.builder()
                .id(id)
                .name("111")
                .tagDesc("test")
                .parentTagId("123")
                .ext("22222")
                .dateCreated(now)
                .dateUpdated(now)
                .build();
    }

}
