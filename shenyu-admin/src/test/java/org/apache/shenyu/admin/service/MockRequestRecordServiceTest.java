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

import com.google.common.collect.Lists;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.apache.shenyu.admin.mapper.MockRequestRecordMapper;
import org.apache.shenyu.admin.model.dto.MockRequestRecordDTO;
import org.apache.shenyu.admin.model.entity.MockRequestRecordDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.query.MockRequestRecordQuery;
import org.apache.shenyu.admin.model.vo.MockRequestRecordVO;
import org.apache.shenyu.admin.service.impl.MockRequestRecordServiceImpl;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;

/**
 * Test cases for MockRequestRecordService.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class MockRequestRecordServiceTest {

    @InjectMocks
    private MockRequestRecordServiceImpl mockRequestRecordService;

    @Mock
    private MockRequestRecordMapper mockRequestRecordMapper;

    @Test
    public void testCreateOrUpdate() {
        MockRequestRecordDTO mockRequestRecordDTO = buildMockRequestRecordDTO();
        given(this.mockRequestRecordMapper.insert(any())).willReturn(1);
        given(this.mockRequestRecordMapper.update(any())).willReturn(1);
        int cnt = mockRequestRecordService.createOrUpdate(mockRequestRecordDTO);
        assertEquals(1, cnt);
        mockRequestRecordDTO.setId("1");
        cnt = mockRequestRecordService.createOrUpdate(mockRequestRecordDTO);
        assertEquals(1, cnt);
    }

    @Test
    public void testFindById() {
        given(this.mockRequestRecordMapper.queryById("1")).willReturn(buildMockRequestRecordDO());
        MockRequestRecordVO mockRequestRecordVO = mockRequestRecordService.findById("1");
        assertEquals("123", mockRequestRecordVO.getApiId());
    }

    @Test
    public void testDelete() {
        given(this.mockRequestRecordMapper.deleteById(any())).willReturn(1);
        given(this.mockRequestRecordMapper.queryById("1")).willReturn(new MockRequestRecordDO());
        int cnt = mockRequestRecordService.delete("1");
        assertEquals(1, cnt);
    }

    @Test
    public void testListByPage() {
        MockRequestRecordQuery mockRequestRecordQuery = new MockRequestRecordQuery();
        mockRequestRecordQuery.setApiId("123");
        List<MockRequestRecordDO> list = new ArrayList<>();
        list.add(buildMockRequestRecordDO());
        given(this.mockRequestRecordMapper.selectByQuery(mockRequestRecordQuery)).willReturn(list);
        CommonPager<MockRequestRecordVO> commonPager = mockRequestRecordService.listByPage(mockRequestRecordQuery);
        assertEquals(1, commonPager.getDataList().size());
    }

    @Test
    public void testBatchDelete() {
        given(this.mockRequestRecordMapper.batchDelete(any())).willReturn(1);
        int cnt = this.mockRequestRecordService.batchDelete(Lists.newArrayList("1"));
        assertEquals(1, cnt);
    }

    private MockRequestRecordDTO buildMockRequestRecordDTO() {
        MockRequestRecordDTO mockRequestRecordDTO = new MockRequestRecordDTO();
        mockRequestRecordDTO.setApiId("1");
        mockRequestRecordDTO.setHost("127.0.0.1");
        mockRequestRecordDTO.setQuery("");
        mockRequestRecordDTO.setUrl("http://127.0.0.1:8080/v1/test");
        mockRequestRecordDTO.setDateUpdated(new Date());
        mockRequestRecordDTO.setPort(8080);
        return mockRequestRecordDTO;
    }

    private MockRequestRecordDO buildMockRequestRecordDO() {
        MockRequestRecordDO mockRequestRecordDO = new MockRequestRecordDO();
        mockRequestRecordDO.setApiId("123");
        return mockRequestRecordDO;
    }

}
