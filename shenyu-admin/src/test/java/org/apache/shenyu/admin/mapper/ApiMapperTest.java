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
import org.apache.shenyu.admin.model.entity.ApiDO;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.ApiQuery;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import javax.annotation.Resource;
import java.sql.Timestamp;
import java.util.Collections;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test cases for ApiMapper.
 */
public final class ApiMapperTest extends AbstractSpringIntegrationTest {

    @Resource
    private ApiMapper apiMapper;

    private final ApiDO apiDO = buildApiDO();

    @BeforeEach
    public void before() {
        int count = apiMapper.insert(apiDO);
        assertEquals(1, count);
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
