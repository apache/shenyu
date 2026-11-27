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
import org.apache.shenyu.admin.model.dto.ApiDTO;
import org.apache.shenyu.admin.model.entity.ApiDO;
import org.apache.shenyu.admin.model.entity.TagDO;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.ApiQuery;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.vo.ApiVO;
import org.apache.shenyu.admin.service.ApiService;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.transaction.annotation.Transactional;

import jakarta.annotation.Resource;
import java.sql.Timestamp;
import java.util.Collections;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

/**
 * Test cases for ApiMapper.
 */
public final class ApiMapperTest extends AbstractSpringIntegrationTest {

    @Resource
    private ApiMapper apiMapper;

    @Resource
    private ApiService apiService;

    @Resource
    private TagMapper tagMapper;

    private final ApiDO apiDO = buildApiDO();

    @BeforeEach
    public void before() {
        int count = apiMapper.insert(apiDO);
        assertEquals(1, count);
    }

    @ParameterizedTest
    @NullSource
    @ValueSource(ints = {2, 4})
    public void testUpdateHistoricalApiRetainsProvenance(final Integer submittedSource) {
        apiDO.setApiSource(4);
        apiMapper.updateByPrimaryKeySelective(apiDO);
        ApiDTO update = new ApiDTO();
        update.setId(apiDO.getId());
        update.setDocument(apiDO.getDocument());
        update.setApiSource(submittedSource);
        update.setApiDesc("edited historical API");
        update.setApiOwner("new owner");
        update.setState(1);

        apiService.createOrUpdate(update);

        ApiDO stored = apiMapper.selectByPrimaryKey(apiDO.getId());
        assertEquals(4, stored.getApiSource());
        assertEquals("edited historical API", stored.getApiDesc());
        assertEquals("new owner", stored.getApiOwner());
        assertEquals(1, stored.getState());
    }

    @Test
    public void testApiListPaginationExcludesDocument() {
        ApiDO first = buildApiDO();
        String path = "/pagination/" + first.getId();
        first.setApiPath(path);
        first.setDocument("large document");
        ApiDO second = buildApiDO();
        second.setApiPath(path);
        apiMapper.insert(first);
        apiMapper.insert(second);
        try {
            ApiQuery query = new ApiQuery();
            query.setApiPath(path);
            query.setPageParameter(new PageParameter(1, 1));
            CommonPager<ApiVO> pageOne = apiService.listByPage(query);
            query.setPageParameter(new PageParameter(2, 1));
            CommonPager<ApiVO> pageTwo = apiService.listByPage(query);
            assertEquals(2, pageOne.getPage().getTotalCount());
            assertEquals(1, pageOne.getDataList().size());
            assertEquals(1, pageTwo.getDataList().size());
            assertNotEquals(pageOne.getDataList().get(0).getId(), pageTwo.getDataList().get(0).getId());
            assertNull(pageOne.getDataList().get(0).getDocument());
            assertNull(pageTwo.getDataList().get(0).getDocument());
            assertEquals("large document", apiMapper.selectByPrimaryKey(first.getId()).getDocument());
        } finally {
            apiMapper.deleteByPrimaryKey(first.getId());
            apiMapper.deleteByPrimaryKey(second.getId());
        }
    }

    @Test
    @Transactional
    public void testTagFilterPaginatesDistinctApis() {
        ApiDO newest = buildApiDO();
        String prefix = "/tag-pagination/" + newest.getId();
        newest.setApiPath(prefix + "/newest");
        newest.setDateCreated(Timestamp.valueOf("2026-01-02 00:00:00"));
        ApiDO older = buildApiDO();
        older.setApiPath(prefix + "/older");
        older.setDateCreated(Timestamp.valueOf("2026-01-01 00:00:00"));
        ApiDO untagged = buildApiDO();
        untagged.setApiPath(prefix + "/untagged");
        apiMapper.insert(newest);
        apiMapper.insert(older);
        apiMapper.insert(untagged);
        String tagId = UUIDUtils.getInstance().generateShortUuid();
        Timestamp time = Timestamp.valueOf("2026-01-01 00:00:00");
        tagMapper.insert(TagDO.builder().id(tagId).tagName("pagination").tagDesc("pagination")
                .parentTagId("0").ext("{}").dateCreated(time).dateUpdated(time).build());
        saveTags(newest, List.of(tagId, tagId));
        saveTags(older, List.of(tagId));

        CommonPager<ApiVO> firstPage = apiService.listByPage(new ApiQuery(prefix, null, tagId, new PageParameter(1, 1)));
        CommonPager<ApiVO> secondPage = apiService.listByPage(new ApiQuery(prefix, null, tagId, new PageParameter(2, 1)));
        assertAll(
                () -> assertEquals(2, firstPage.getPage().getTotalCount()),
                () -> assertEquals(2, firstPage.getPage().getTotalPage()),
                () -> assertEquals(List.of(newest.getId()), firstPage.getDataList().stream().map(ApiVO::getId).toList()),
                () -> assertEquals(List.of(older.getId()), secondPage.getDataList().stream().map(ApiVO::getId).toList()));
        assertEquals(3, apiService.listByPage(new ApiQuery(prefix, null, null, new PageParameter(1, 10))).getPage().getTotalCount());
    }

    @Test
    public void testInsert() {
        ApiDO newApiDO = buildApiDO();
        int count = apiMapper.insert(newApiDO);
        assertEquals(1, count);
    }

    @Test
    public void testInsertSelective() {
        ApiDO newApiDO = buildApiDO();
        int count = apiMapper.insertSelective(newApiDO);
        assertEquals(1, count);
    }

    @Test
    public void testSelectByPrimaryKey() {
        ApiDO apiDO = apiMapper.selectByPrimaryKey(this.apiDO.getId());
        assertNotNull(apiDO);
    }

    @Test
    public void testUpdateByPrimaryKeySelective() {
        Timestamp now = new Timestamp(System.currentTimeMillis());
        this.apiDO.setDateUpdated(now);
        this.apiDO.setContextPath("/dubbo1");
        this.apiDO.setApiPath("/demo/findById1");
        this.apiDO.setHttpMethod(1);
        this.apiDO.setVersion("V0.02");
        this.apiDO.setRpcType("dubbo1");
        this.apiDO.setState(1);
        int count = apiMapper.updateByPrimaryKeySelective(this.apiDO);
        assertEquals(1, count);
    }

    @Test
    public void testUpdateByPrimaryKey() {
        Timestamp now = new Timestamp(System.currentTimeMillis());
        this.apiDO.setDateUpdated(now);
        this.apiDO.setContextPath("/dubbo2");
        this.apiDO.setApiPath("/demo/findById2");
        this.apiDO.setHttpMethod(2);
        this.apiDO.setVersion("V0.03");
        this.apiDO.setRpcType("dubbo2");
        this.apiDO.setState(2);
        this.apiDO.setApiSource(3);
        int count = apiMapper.updateByPrimaryKeySelective(this.apiDO);
        assertEquals(1, count);
    }

    @Test
    public void testSelectByQuery() {
        ApiQuery query = new ApiQuery();
        query.setState(0);
        query.setApiPath("/demo/findById");
        query.setPageParameter(new PageParameter(1, 10));
        List<ApiDO> apiDOS = apiMapper.selectByQuery(query);
        assertThat(apiDOS.size(), greaterThan(0));
    }

    @Test
    public void testSelectByIds() {
        List<String> strings = Collections.singletonList(apiDO.getId());
        List<ApiDO> apiDOS = apiMapper.selectByIds(strings);
        assertThat(apiDOS.size(), greaterThan(0));
    }

    @Test
    public void testDeleteByPrimaryKey() {
        int count = apiMapper.deleteByPrimaryKey(this.apiDO.getId());
        assertEquals(1, count);
    }

    @Test
    public void testDeleteByIds() {
        ApiDO newApiDO = buildApiDO();
        apiMapper.insertSelective(newApiDO);
        List<String> ids = Collections.singletonList(newApiDO.getId());
        int deleteRows = apiMapper.deleteByIds(ids);
        assertEquals(1, deleteRows);
    }

    private void saveTags(final ApiDO api, final List<String> tagIds) {
        ApiDTO update = new ApiDTO();
        update.setId(api.getId());
        update.setDocument(api.getDocument());
        update.setTagIds(tagIds);
        apiService.createOrUpdate(update);
    }

    private ApiDO buildApiDO() {
        Timestamp now = new Timestamp(System.currentTimeMillis());
        ApiDO apiDO = new ApiDO();
        apiDO.setId(UUIDUtils.getInstance().generateShortUuid());
        apiDO.setContextPath("/dubbo");
        apiDO.setApiPath("/demo/findById");
        apiDO.setHttpMethod(2);
        apiDO.setConsume("application/json");
        apiDO.setProduce("accept");
        apiDO.setVersion("V0.01");
        apiDO.setRpcType("dubbo");
        apiDO.setState(0);
        apiDO.setExt("ext");
        apiDO.setApiOwner("admin");
        apiDO.setApiDesc("hello world api");
        apiDO.setDocument("{\"request\":{\"id\":\"123\"},\"response\":{\"id\":\"123\"}}");
        apiDO.setDocumentMd5("933833690e1710c7c1f26f9a9cb84659");
        apiDO.setApiSource(2);
        apiDO.setDateCreated(now);
        apiDO.setDateUpdated(now);
        return apiDO;
    }
}
