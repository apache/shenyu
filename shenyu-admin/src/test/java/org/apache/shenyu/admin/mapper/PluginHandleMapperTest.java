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
import org.apache.shenyu.admin.model.entity.PluginHandleDO;
import org.apache.shenyu.admin.model.query.PluginHandleQuery;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.Test;
import javax.annotation.Resource;
import java.sql.Timestamp;
import java.util.List;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * Test cases for PluginHandleMapper.
 */
public final class PluginHandleMapperTest extends AbstractSpringIntegrationTest {

    @Resource
    private PluginHandleMapper pluginHandleMapper;

    @Test
    public void selectById() {
        PluginHandleDO pluginHandleDO = buildPluginHandleDO();
        int insert = pluginHandleMapper.insert(pluginHandleDO);
        assertThat(insert, equalTo(1));

        PluginHandleDO resultPluginHandleDO = pluginHandleMapper.selectById(pluginHandleDO.getId());
        assertThat(pluginHandleDO, equalTo(resultPluginHandleDO));

        int delete = pluginHandleMapper.delete(pluginHandleDO.getId());
        assertThat(delete, equalTo(1));
    }

    @Test
    public void findByPluginId() {
        PluginHandleDO pluginHandleDO = buildPluginHandleDO();
        int insert = pluginHandleMapper.insert(pluginHandleDO);
        assertThat(insert, equalTo(1));

        String pluginId = pluginHandleDO.getPluginId();
        List<PluginHandleDO> pluginHandleDOS = pluginHandleMapper.findByPluginId(pluginId);
        assertThat(pluginHandleDOS.size(), equalTo(1));
        assertThat(pluginHandleDO, equalTo(pluginHandleDOS.get(0)));

        int delete = pluginHandleMapper.delete(pluginHandleDO.getId());
        assertThat(delete, equalTo(1));
    }

    @Test
    public void insert() {
        PluginHandleDO pluginHandleDO = buildPluginHandleDO();
        int insert = pluginHandleMapper.insert(pluginHandleDO);
        assertThat(insert, equalTo(1));

        int delete = pluginHandleMapper.delete(pluginHandleDO.getId());
        assertThat(delete, equalTo(1));
    }

    @Test
    public void insertSelective() {
        PluginHandleDO pluginHandleDO = buildPluginHandleDO();
        int insert = pluginHandleMapper.insertSelective(pluginHandleDO);
        assertThat(insert, equalTo(1));

        int delete = pluginHandleMapper.delete(pluginHandleDO.getId());
        assertThat(delete, equalTo(1));
    }

    @Test
    public void countByQuery() {
        PluginHandleDO pluginHandleDO = buildPluginHandleDO();
        int insert = pluginHandleMapper.insert(pluginHandleDO);
        assertThat(insert, equalTo(1));

        PluginHandleQuery pluginHandleQuery = new PluginHandleQuery();
        pluginHandleQuery.setPluginId(pluginHandleDO.getPluginId());
        Integer count = pluginHandleMapper.countByQuery(pluginHandleQuery);
        assertThat(count, equalTo(1));

        int delete = pluginHandleMapper.delete(pluginHandleDO.getId());
        assertThat(delete, equalTo(1));
    }

    @Test
    public void selectByQuery() {
        PluginHandleDO pluginHandleDO = buildPluginHandleDO();
        int insert = pluginHandleMapper.insert(pluginHandleDO);
        assertThat(insert, equalTo(1));

        PluginHandleQuery pluginHandleQuery = new PluginHandleQuery();
        pluginHandleQuery.setPluginId(pluginHandleDO.getPluginId());
        List<PluginHandleDO> pluginHandleDOS = pluginHandleMapper.selectByQuery(pluginHandleQuery);
        assertThat(pluginHandleDOS.size(), equalTo(1));
        assertThat(pluginHandleDO, equalTo(pluginHandleDOS.get(0)));

        int delete = pluginHandleMapper.delete(pluginHandleDO.getId());
        assertThat(delete, equalTo(1));
    }

    @Test
    public void updateByPrimaryKeySelective() {
        PluginHandleDO pluginHandleDO = buildPluginHandleDO();
        int insert = pluginHandleMapper.insert(pluginHandleDO);
        assertThat(insert, equalTo(1));

        pluginHandleDO.setField("test_field_2");
        int update = pluginHandleMapper.updateByPrimaryKeySelective(pluginHandleDO);
        assertThat(update, equalTo(1));

        PluginHandleDO resultPluginHandleDO = pluginHandleMapper.selectById(pluginHandleDO.getId());
        assertThat(pluginHandleDO, equalTo(resultPluginHandleDO));

        int delete = pluginHandleMapper.delete(pluginHandleDO.getId());
        assertThat(delete, equalTo(1));
    }

    @Test
    public void updateByPrimaryKey() {
        PluginHandleDO pluginHandleDO = buildPluginHandleDO();
        int insert = pluginHandleMapper.insert(pluginHandleDO);
        assertThat(insert, equalTo(1));

        pluginHandleDO.setDataType(2);
        pluginHandleDO.setType(2);
        int update = pluginHandleMapper.updateByPrimaryKey(pluginHandleDO);
        assertThat(update, equalTo(1));

        PluginHandleDO resultPluginHandleDO = pluginHandleMapper.selectById(pluginHandleDO.getId());
        assertThat(pluginHandleDO, equalTo(resultPluginHandleDO));

        int delete = pluginHandleMapper.delete(pluginHandleDO.getId());
        assertThat(delete, equalTo(1));
    }

    @Test
    public void delete() {
        PluginHandleDO pluginHandleDO = buildPluginHandleDO();
        int insert = pluginHandleMapper.insert(pluginHandleDO);
        assertThat(insert, equalTo(1));

        int delete = pluginHandleMapper.delete(pluginHandleDO.getId());
        assertThat(delete, equalTo(1));

        PluginHandleDO resultPluginHandleDO = pluginHandleMapper.selectById(pluginHandleDO.getId());
        assertThat(resultPluginHandleDO, equalTo(null));
    }

    private PluginHandleDO buildPluginHandleDO() {
        String id = UUIDUtils.getInstance().generateShortUuid();
        String pluginId = UUIDUtils.getInstance().generateShortUuid();
        Timestamp now = new Timestamp(System.currentTimeMillis());
        return PluginHandleDO.builder()
                .id(id)
                .pluginId(pluginId)
                .field("test_field")
                .label("test_label")
                .dataType(1)
                .type(1)
                .sort(1)
                .dateCreated(now)
                .dateUpdated(now)
                .build();
    }
}
