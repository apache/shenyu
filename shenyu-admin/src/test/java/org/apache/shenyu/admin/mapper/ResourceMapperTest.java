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

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.admin.AbstractSpringIntegrationTest;
import org.apache.shenyu.admin.model.dto.ResourceDTO;
import org.apache.shenyu.admin.model.entity.ResourceDO;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.ResourceQuery;
import org.apache.shenyu.common.enums.AdminResourceEnum;
import org.assertj.core.util.Lists;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.sql.Timestamp;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.UUID;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * test for {@linkplain ResourceMapper}.
 */
public class ResourceMapperTest extends AbstractSpringIntegrationTest {

    private static final String MOCK_PARENT_RESOURCE_ID = "parentId";

    private static final String MOCK_RESOURCE_ID = "resourceId";

    private static final String MOCK_RESOURCE_TITLE = "resource title";

    private final long bootstrapTimestampMills = System.currentTimeMillis();

    @Autowired
    private ResourceMapper resourceMapper;

    @BeforeEach
    public void insertTestResources() {
        resourceMapper.insert(this.buildTestResource());
    }

    @AfterEach
    public void deleteTestResource() {
        resourceMapper.delete(Collections.singletonList(MOCK_RESOURCE_ID));
    }

    @Test
    public void testSelectById() {
        ResourceDO resourceDO = resourceMapper.selectById(MOCK_RESOURCE_ID);
        assertNotNull(resourceDO);
        assertThat(resourceDO, equalTo(this.buildTestResource()));
    }

    @Test
    public void testSelectByParentId() {
        List<ResourceDO> resources = resourceMapper.selectByParentId(MOCK_PARENT_RESOURCE_ID);
        assertTrue(CollectionUtils.isNotEmpty(resources));
        assertThat(resources.size(), equalTo(1));
    }

    @Test
    public void testResourceQuery() {
        final ResourceQuery resourceQuery = new ResourceQuery(MOCK_RESOURCE_TITLE, new PageParameter());
        List<ResourceDO> queryResult = resourceMapper.selectByQuery(resourceQuery);
        assertThat(queryResult.size(), equalTo(1));
        assertThat(queryResult.iterator().next().getTitle(), equalTo(MOCK_RESOURCE_TITLE));
        assertThat(resourceMapper.countByQuery(resourceQuery), equalTo(1));
    }

    @Test
    public void testInsertSelective() {
        final String mockResourceId = "MOCK_RESOURCE_ID";
        ResourceDTO resourceDTO = new ResourceDTO();
        resourceDTO.setId(mockResourceId);
        resourceDTO.setParentId(MOCK_PARENT_RESOURCE_ID);
        resourceDTO.setTitle("MOCK_RESOURCE_TITLE");
        resourceDTO.setName("resource name");
        resourceDTO.setUrl("resource url");
        resourceDTO.setComponent("resource component");
        resourceDTO.setResourceType(AdminResourceEnum.MAIN_MENU.getCode());
        resourceDTO.setIcon("icon");
        resourceDTO.setIsLeaf(Boolean.TRUE);
        resourceDTO.setIsRoute(1);
        resourceDTO.setSort(1);
        resourceDTO.setStatus(1);
        resourceDTO.setPerms("perms");
        ResourceDO resourceDO = ResourceDO.buildResourceDO(resourceDTO);
        resourceDO.setDateCreated(new Timestamp(bootstrapTimestampMills));
        resourceDO.setDateUpdated(new Timestamp(bootstrapTimestampMills));
        assertThat(resourceMapper.insertSelective(resourceDO), equalTo(1));
        assertThat(resourceMapper.selectById(mockResourceId), equalTo(resourceDO));
    }

    @Test
    public void testInsertBatch() {

        String id = UUID.randomUUID().toString();
        ResourceDO resourceDO = this.buildTestResource();
        resourceDO.setId(id);

        String id1 = UUID.randomUUID().toString();
        ResourceDO resourceDO1 = this.buildTestResource();
        resourceDO1.setId(id1);

        List<ResourceDO> resourceDOList = Lists.list(resourceDO, resourceDO1);
        assertThat(resourceMapper.insertBatch(resourceDOList), equalTo(resourceDOList.size()));

    }

    @Test
    public void testUpdate() {
        ResourceDO resourceDO = resourceMapper.selectById(MOCK_RESOURCE_ID);
        final String resourceNameAfterUpdate = "resource name after update";
        resourceDO.setName(resourceNameAfterUpdate);
        assertThat(resourceMapper.update(resourceDO), equalTo(1));
    }

    @Test
    public void testUpdateSelective() {
        final String resourceNameAfterUpdate = "resource name after update";
        ResourceDO updateParam = new ResourceDO();
        updateParam.setId(MOCK_RESOURCE_ID);
        updateParam.setName(resourceNameAfterUpdate);
        assertThat(resourceMapper.updateSelective(updateParam), equalTo(1));
        ResourceDO resourceDoAfterUpdate = resourceMapper.selectById(MOCK_RESOURCE_ID);
        ResourceDO resourceDO = resourceMapper.selectById(MOCK_RESOURCE_ID);
        assertNotNull(resourceDoAfterUpdate);
        assertThat(resourceDoAfterUpdate.getName(), equalTo(resourceNameAfterUpdate));
        assertThat(resourceDoAfterUpdate.getTitle(), equalTo(resourceDO.getTitle()));
    }

    @Test
    public void testSelectAll() {
        List<ResourceDO> resourceDOS = resourceMapper.selectAll();
        // for init schema resource insert, only assert resourceMapper.selectAll() result not null
        assertTrue(CollectionUtils.isNotEmpty(resourceDOS));
    }

    @Test
    public void testDelete() {
        ResourceDO resourceDO = resourceMapper.selectById(MOCK_RESOURCE_ID);
        assertTrue(Objects.nonNull(resourceDO));
        resourceMapper.delete(Collections.singletonList(MOCK_RESOURCE_ID));
        assertTrue(Objects.isNull(resourceMapper.selectById(MOCK_RESOURCE_ID)));
    }

    private ResourceDO buildTestResource() {
        ResourceDTO resourceDTO = new ResourceDTO();
        resourceDTO.setId(MOCK_RESOURCE_ID);
        resourceDTO.setParentId(MOCK_PARENT_RESOURCE_ID);
        resourceDTO.setTitle(MOCK_RESOURCE_TITLE);
        resourceDTO.setName("resource name");
        resourceDTO.setUrl("resource url");
        resourceDTO.setComponent("resource component");
        resourceDTO.setResourceType(AdminResourceEnum.MAIN_MENU.getCode());
        resourceDTO.setSort(0);
        resourceDTO.setIcon("resource icon");
        resourceDTO.setIsLeaf(true);
        resourceDTO.setIsRoute(1);
        resourceDTO.setPerms("perms");
        resourceDTO.setStatus(1);
        ResourceDO resourceDO = ResourceDO.buildResourceDO(resourceDTO);
        resourceDO.setDateCreated(new Timestamp(bootstrapTimestampMills));
        resourceDO.setDateUpdated(new Timestamp(bootstrapTimestampMills));
        return resourceDO;
    }
}
