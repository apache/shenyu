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
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.SelectorQuery;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.Test;

import javax.annotation.Resource;
import java.sql.Timestamp;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.hamcrest.Matchers.hasItems;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.hamcrest.MatcherAssert.assertThat;


/**
 * Test Cases for SelectorMapper.
 */
public final class SelectorMapperTest extends AbstractSpringIntegrationTest {

    @Resource
    private SelectorMapper selectorMapper;

    @Test
    public void testSelectById() {
        SelectorDO selectorDO = buildSelectorDO();
        int insert = selectorMapper.insert(selectorDO);
        assertEquals(1, insert);

        SelectorDO selector = selectorMapper.selectById(selectorDO.getId());
        assertNotNull(selector);
        assertEquals(selectorDO.getId(), selector.getId());
        assertEquals(selectorDO.getContinued(), selector.getContinued());

        int delete = selectorMapper.delete(selectorDO.getId());
        assertEquals(1, delete);
    }

    @Test
    public void testSelectByIdList() {

        SelectorDO selectorDO1 = buildSelectorDO();
        int insert1 = selectorMapper.insert(selectorDO1);
        assertEquals(1, insert1);

        SelectorDO selectorDO = buildSelectorDO();
        int insert = selectorMapper.insert(selectorDO);
        assertEquals(1, insert);

        Set<String> idSet = Stream.of(selectorDO1.getId(), selectorDO.getId()).collect(Collectors.toSet());
        List<SelectorDO> selectorList = selectorMapper.selectByIdSet(idSet);
        assertNotNull(selectorList);
        assertThat(selectorList, hasItems(selectorDO1, selectorDO));

    }

    @Test
    public void testSelectByQuery() {
        SelectorDO selectorDO = buildSelectorDO();
        int insert = selectorMapper.insert(selectorDO);
        assertEquals(1, insert);

        SelectorQuery query = new SelectorQuery(selectorDO.getPluginId(), selectorDO.getName(), new PageParameter());
        List<SelectorDO> list = selectorMapper.selectByQuery(query);
        assertNotNull(list);
        assertEquals(list.size(), 1);
        assertNotNull(selectorDO.getPluginId(), list.get(0).getPluginId());

        int delete = selectorMapper.delete(selectorDO.getId());
        assertEquals(1, delete);
    }

    @Test
    public void testFindByPluginId() {
        SelectorDO selectorDO = buildSelectorDO();
        int insert = selectorMapper.insert(selectorDO);
        assertEquals(1, insert);

        List<SelectorDO> list = selectorMapper.findByPluginId(selectorDO.getPluginId());
        assertNotNull(list);
        assertEquals(list.size(), 1);
        assertNotNull(selectorDO.getPluginId(), list.get(0).getPluginId());

        int delete = selectorMapper.delete(selectorDO.getId());
        assertEquals(1, delete);
    }

    @Test
    public void testSelectByName() {
        SelectorDO selectorDO = buildSelectorDO();
        int insert = selectorMapper.insert(selectorDO);
        assertEquals(1, insert);

        SelectorDO selector = selectorMapper.selectByName(selectorDO.getName());
        assertNotNull(selector);
        assertEquals(selectorDO.getName(), selector.getName());

        int delete = selectorMapper.delete(selectorDO.getId());
        assertEquals(1, delete);
    }

    @Test
    public void testCountByQuery() {
        SelectorDO selectorDO = buildSelectorDO();
        int insert = selectorMapper.insert(selectorDO);
        assertEquals(1, insert);

        SelectorQuery query = new SelectorQuery(selectorDO.getPluginId(), selectorDO.getName(), new PageParameter());
        Integer count = selectorMapper.countByQuery(query);
        assertNotNull(count);
        assertEquals(Integer.valueOf(1), count);

        int delete = selectorMapper.delete(selectorDO.getId());
        assertEquals(1, delete);
    }

    @Test
    public void testInsert() {
        SelectorDO selectorDO = buildSelectorDO();
        int insert = selectorMapper.insert(selectorDO);
        assertEquals(1, insert);

        int delete = selectorMapper.delete(selectorDO.getId());
        assertEquals(1, delete);
    }

    @Test
    public void testInsertSelective() {
        SelectorDO selectorDO = buildSelectorDO();
        int insert = selectorMapper.insertSelective(selectorDO);
        assertEquals(1, insert);

        int delete = selectorMapper.delete(selectorDO.getId());
        assertEquals(1, delete);
    }

    @Test
    public void testUpdate() {
        SelectorDO selectorDO = buildSelectorDO();
        int insert = selectorMapper.insert(selectorDO);
        assertEquals(1, insert);

        selectorDO.setHandle("handle-test");
        int count = selectorMapper.update(selectorDO);
        assertEquals(1, count);

        int delete = selectorMapper.delete(selectorDO.getId());
        assertEquals(1, delete);
    }

    @Test
    public void testUpdateSelective() {
        SelectorDO selectorDO = buildSelectorDO();
        int insert = selectorMapper.insert(selectorDO);
        assertEquals(1, insert);

        selectorDO.setHandle("handle-test");
        int count = selectorMapper.updateSelective(selectorDO);
        assertEquals(1, count);

        int delete = selectorMapper.delete(selectorDO.getId());
        assertEquals(1, delete);
    }

    @Test
    public void testDelete() {
        SelectorDO selectorDO = buildSelectorDO();
        int insert = selectorMapper.insert(selectorDO);
        assertEquals(1, insert);

        int count = selectorMapper.delete(selectorDO.getId());
        assertEquals(1, count);
    }

    @Test
    public void testDeleteByPluginId() {
        SelectorDO selectorDO = buildSelectorDO();
        int insert = selectorMapper.insert(selectorDO);
        assertEquals(1, insert);

        int count = selectorMapper.deleteByPluginId(selectorDO.getPluginId());
        assertEquals(1, count);
    }

    @Test
    public void testSelectAll() {
        SelectorDO selectorDO = buildSelectorDO();
        int insert = selectorMapper.insert(selectorDO);
        assertEquals(1, insert);

        List<SelectorDO> list = selectorMapper.selectAll();
        assertNotNull(list);
        assertEquals(list.size(), 1);
        assertNotNull(selectorDO.getPluginId(), list.get(0).getPluginId());

        int delete = selectorMapper.delete(selectorDO.getId());
        assertEquals(1, delete);
    }

    private SelectorDO buildSelectorDO() {
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        return SelectorDO.builder()
                .id(UUIDUtils.getInstance().generateShortUuid())
                .dateCreated(currentTime)
                .dateUpdated(currentTime)
                .pluginId("test-plugin-id")
                .name("test-name")
                .matchMode(1)
                .type(1)
                .sort(1)
                .enabled(Boolean.TRUE)
                .loged(Boolean.TRUE)
                .matchRestful(false)
                .continued(Boolean.TRUE)
                .handle("handle")
                .build();
    }
}
