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
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.SelectorQuery;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.Test;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Timestamp;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItems;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test Cases for SelectorMapper.
 */
public final class SelectorMapperTest extends AbstractSpringIntegrationTest {

    /**
     * A namespace that is different from the default one, used to exercise the
     * namespace-scoped {@code selectByIdSet} / {@code deleteByIds} statements.
     */
    private static final String OTHER_NAMESPACE_ID = "2b8b2b3a-1f5f-4f1e-9c1a-000000000000";

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
        List<SelectorDO> selectorList = selectorMapper.selectByIdSet(idSet, SYS_DEFAULT_NAMESPACE_ID);
        assertNotNull(selectorList);
        assertThat(selectorList, hasItems(selectorDO1, selectorDO));

    }

    /**
     * Regression test for the cross-namespace isolation this PR introduces: a selector that
     * belongs to another namespace must not be returned by {@code selectByIdSet} nor removed
     * by {@code deleteByIds} when the default namespace id is passed. Removing the
     * {@code namespace_id} predicate from either statement makes this test fail.
     */
    @Test
    public void testSelectByIdSetAndDeleteByIdsAreNamespaceScoped() {
        SelectorDO defaultNamespaceSelector = buildSelectorDO();
        assertEquals(1, selectorMapper.insert(defaultNamespaceSelector));

        SelectorDO otherNamespaceSelector = buildSelectorDO(OTHER_NAMESPACE_ID);
        assertEquals(1, selectorMapper.insert(otherNamespaceSelector));

        Set<String> bothIds = Stream.of(defaultNamespaceSelector.getId(), otherNamespaceSelector.getId())
                .collect(Collectors.toSet());

        // selectByIdSet must only return the selector of the requested namespace.
        List<SelectorDO> selected = selectorMapper.selectByIdSet(bothIds, SYS_DEFAULT_NAMESPACE_ID);
        assertEquals(1, selected.size());
        assertEquals(defaultNamespaceSelector.getId(), selected.get(0).getId());

        // deleteByIds must not remove the selector of the other namespace (nor report it deleted).
        List<String> bothIdList = Stream.of(defaultNamespaceSelector.getId(), otherNamespaceSelector.getId())
                .collect(Collectors.toList());
        int deleted = selectorMapper.deleteByIds(bothIdList, SYS_DEFAULT_NAMESPACE_ID);
        assertEquals(1, deleted);
        assertNotNull(selectorMapper.selectById(otherNamespaceSelector.getId()));

        // Clean up the selector that the namespace-scoped delete intentionally left behind.
        assertEquals(1, selectorMapper.delete(otherNamespaceSelector.getId()));
    }

    @Test
    public void testSelectByQuery() {
        SelectorDO selectorDO = buildSelectorDO();
        int insert = selectorMapper.insert(selectorDO);
        assertEquals(1, insert);

        SelectorQuery query = new SelectorQuery(selectorDO.getPluginId(), selectorDO.getSelectorName(), new PageParameter(), SYS_DEFAULT_NAMESPACE_ID);
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

        List<SelectorDO> list = selectorMapper.findByPluginIdAndNamespaceId(selectorDO.getPluginId(), selectorDO.getNamespaceId());
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
        List<SelectorDO> doList = selectorMapper.selectByNameAndNamespaceId(selectorDO.getSelectorName(), SYS_DEFAULT_NAMESPACE_ID);
        assertEquals(doList.size(), 1);
        assertNotNull(doList.get(0));
        assertEquals(selectorDO.getSelectorName(), doList.get(0).getSelectorName());

        int delete = selectorMapper.delete(selectorDO.getId());
        assertEquals(1, delete);
    }

    @Test
    public void testCountByQuery() {
        SelectorDO selectorDO = buildSelectorDO();
        int insert = selectorMapper.insert(selectorDO);
        assertEquals(1, insert);

        SelectorQuery query = new SelectorQuery(selectorDO.getPluginId(), selectorDO.getSelectorName(), new PageParameter(), SYS_DEFAULT_NAMESPACE_ID);
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

    @Test
    @Transactional
    public void testCountMatchesFilteredList() {
        SelectorDO first = buildSelectorDO();
        first.setSelectorName("permission-keyword-first");
        SelectorDO second = buildSelectorDO();
        second.setSelectorName("permission-keyword-second");
        SelectorDO otherNamespace = buildSelectorDO();
        otherNamespace.setSelectorName("permission-keyword-other");
        otherNamespace.setNamespaceId("other-namespace");
        SelectorDO otherPlugin = buildSelectorDO();
        otherPlugin.setSelectorName("permission-keyword-plugin");
        otherPlugin.setPluginId("other-plugin");
        List<SelectorDO> selectors = List.of(first, second, otherNamespace, otherPlugin);
        selectors.forEach(selectorMapper::insert);
        SelectorQuery query = new SelectorQuery(List.of(first.getPluginId()), "keyword", new PageParameter(), SYS_DEFAULT_NAMESPACE_ID);
        query.setFilterIds(selectors.stream().map(SelectorDO::getId).collect(Collectors.toList()));
        assertEquals(2, selectorMapper.countByQuery(query));
        assertEquals(selectorMapper.selectByQuery(query).size(), selectorMapper.countByQuery(query));
        query.setName(first.getSelectorName());
        assertEquals(1, selectorMapper.countByQuery(query));
        query.setName("missing-keyword");
        assertEquals(0, selectorMapper.countByQuery(query));
    }

    private SelectorDO buildSelectorDO() {
        return buildSelectorDO(SYS_DEFAULT_NAMESPACE_ID);
    }

    private SelectorDO buildSelectorDO(final String namespaceId) {
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        return SelectorDO.builder()
                .id(UUIDUtils.getInstance().generateShortUuid())
                .dateCreated(currentTime)
                .dateUpdated(currentTime)
                .pluginId("test-plugin-id")
                .selectorName("test-name")
                .matchMode(1)
                .selectorType(1)
                .sortCode(1)
                .enabled(Boolean.TRUE)
                .loged(Boolean.TRUE)
                .matchRestful(false)
                .continued(Boolean.TRUE)
                .handle("handle")
                .namespaceId(namespaceId)
                .build();
    }
}
