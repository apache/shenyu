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

package org.apache.shenyu.admin.service;

import org.apache.shenyu.admin.mapper.ApiMapper;
import org.apache.shenyu.admin.mapper.TagMapper;
import org.apache.shenyu.admin.mapper.TagRelationMapper;
import org.apache.shenyu.admin.model.dto.ApiDTO;
import org.apache.shenyu.admin.model.entity.ApiDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.ApiQuery;
import org.apache.shenyu.admin.model.vo.ApiVO;
import org.apache.shenyu.admin.service.impl.ApiServiceImpl;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.platform.commons.util.StringUtils;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.when;

/**
 * Test cases for ApiService.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class ApiServiceTest {

    @InjectMocks
    private ApiServiceImpl apiService;

    @Mock
    private ApiMapper apiMapper;

    @Mock
    private TagRelationMapper tagRelationMapper;

    @Mock
    private TagMapper tagMapper;

    @BeforeEach
    public void setUp() {
        apiService = new ApiServiceImpl(apiMapper, tagRelationMapper, tagMapper);
    }

    @Test
    public void testCreateOrUpdate() {
        testCreate();
        testUpdate("123");
    }

    @Test
    public void testDelete() {
        List<ApiDO> apis = Collections.singletonList(buildApiDO("123"));
        when(apiMapper.selectByIds(Collections.singletonList("123"))).thenReturn(apis);
        when(apiMapper.deleteByIds(Collections.singletonList("123"))).thenReturn(1);
        assertEquals(org.apache.commons.lang3.StringUtils.EMPTY, apiService.delete(Collections.singletonList("123")));
    }

    @Test
    public void testFindById() {
        String id = "123";
        final ApiDO apiDO = buildApiDO(id);
        given(this.apiMapper.selectByPrimaryKey(eq(id))).willReturn(apiDO);
        ApiVO byId = this.apiService.findById(id);
        assertNotNull(byId);
    }

    @Test
    public void testListByPage() {
        PageParameter pageParameter = new PageParameter();
        pageParameter.setPageSize(5);
        pageParameter.setTotalCount(10);
        pageParameter.setTotalPage(pageParameter.getTotalCount() / pageParameter.getPageSize());
        ApiQuery apiQuery = new ApiQuery(null, 0, "", pageParameter);
        List<ApiDO> apiDOList = IntStream.range(0, 10).mapToObj(i -> buildApiDO("" + i)).collect(Collectors.toList());
        given(this.apiMapper.selectByQuery(apiQuery)).willReturn(apiDOList);
        final CommonPager<ApiVO> apiDOCommonPager = this.apiService.listByPage(apiQuery);
        assertEquals(apiDOCommonPager.getDataList().size(), apiDOList.size());
    }

    private ApiDO buildApiDO(final String id) {
        ApiDO apiDO = ApiDO.buildApiDO(buildApiDTO(id));
        Timestamp now = Timestamp.valueOf(LocalDateTime.now());
        apiDO.setDateCreated(now);
        apiDO.setDateUpdated(now);
        return apiDO;
    }

    private void testCreate() {
        ApiDTO apiDTO = buildApiDTO("");
        assertEquals(ShenyuResultMessage.CREATE_SUCCESS, this.apiService.createOrUpdate(apiDTO));
    }

    private ApiDTO buildApiDTO(final String id) {
        ApiDTO apiDTO = new ApiDTO();
        if (StringUtils.isNotBlank(id)) {
            apiDTO.setId(id);
        }
        apiDTO.setContextPath("string");
        apiDTO.setApiPath("string");
        apiDTO.setHttpMethod(0);
        apiDTO.setConsume("string");
        apiDTO.setProduce("string");
        apiDTO.setVersion("string");
        apiDTO.setRpcType("string");
        apiDTO.setState(0);
        apiDTO.setApiOwner("string");
        apiDTO.setApiDesc("string");
        apiDTO.setApiSource(0);
        apiDTO.setDocument("document");
        return apiDTO;
    }

    private void testUpdate(final String id) {
        ApiDTO apiDTO = new ApiDTO();
        apiDTO.setId(id);
        apiDTO.setApiPath("test");
        apiDTO.setDocument("testDocument");
        assertEquals(ShenyuResultMessage.UPDATE_SUCCESS, this.apiService.createOrUpdate(apiDTO));
    }
}
