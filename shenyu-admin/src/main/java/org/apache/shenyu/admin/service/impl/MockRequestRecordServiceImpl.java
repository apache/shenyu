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

package org.apache.shenyu.admin.service.impl;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.mapper.MockRequestRecordMapper;
import org.apache.shenyu.admin.model.dto.MockRequestRecordDTO;
import org.apache.shenyu.admin.model.entity.MockRequestRecordDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.MockRequestRecordQuery;
import org.apache.shenyu.admin.model.vo.MockRequestRecordVO;
import org.apache.shenyu.admin.service.MockRequestRecordService;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.springframework.stereotype.Service;

import java.sql.Timestamp;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link MockRequestRecordService}.
 */
@Service
public class MockRequestRecordServiceImpl implements MockRequestRecordService {

    private final MockRequestRecordMapper mockRequestRecordMapper;

    public MockRequestRecordServiceImpl(final MockRequestRecordMapper mockRequestRecordMapper) {
        this.mockRequestRecordMapper = mockRequestRecordMapper;

    }

    @Override
    public int createOrUpdate(final MockRequestRecordDTO mockRequestRecordDTO) {
        return StringUtils.isBlank(mockRequestRecordDTO.getId()) ? this.create(mockRequestRecordDTO) : this.update(mockRequestRecordDTO);
    }

    @Override
    public int delete(final String id) {
        MockRequestRecordDO mockRequestRecordDO = mockRequestRecordMapper.queryById(id);
        if (mockRequestRecordDO == null) {
            return 0;
        }
        return mockRequestRecordMapper.deleteById(id);
    }

    @Override
    public int batchDelete(final List<String> ids) {
        return mockRequestRecordMapper.batchDelete(ids);
    }

    @Override
    public MockRequestRecordVO findById(final String id) {
        MockRequestRecordVO mockRequestRecordVO = new MockRequestRecordVO();
        if (StringUtils.isBlank(id)) {
            return mockRequestRecordVO;
        }
        MockRequestRecordDO mockRequestRecordDO = mockRequestRecordMapper.queryById(id);
        if (mockRequestRecordDO == null) {
            return mockRequestRecordVO;
        }
        return MockRequestRecordVO.buildMockRequestRecordVO(mockRequestRecordDO);
    }

    @Override
    public CommonPager<MockRequestRecordVO> listByPage(final MockRequestRecordQuery mockRequestRecordQuery) {
        List<MockRequestRecordDO> list = mockRequestRecordMapper.selectByQuery(mockRequestRecordQuery);
        return PageResultUtils.result(mockRequestRecordQuery.getPageParameter(), () -> list.stream().map(MockRequestRecordVO::buildMockRequestRecordVO).collect(Collectors.toList()));
    }

    private int update(final MockRequestRecordDTO mockRequestRecordDTO) {
        if (mockRequestRecordDTO == null || mockRequestRecordDTO.getId() == null) {
            return 0;
        }
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        MockRequestRecordDO mockRequestRecordDO = MockRequestRecordDO.builder()
                .id(mockRequestRecordDTO.getId())
                .apiId(mockRequestRecordDTO.getApiId())
                .header(mockRequestRecordDTO.getHeader())
                .host(mockRequestRecordDTO.getHost())
                .port(mockRequestRecordDTO.getPort())
                .query(mockRequestRecordDTO.getQuery())
                .url(mockRequestRecordDTO.getUrl())
                .pathVariable(mockRequestRecordDTO.getPathVariable())
                .body(mockRequestRecordDTO.getBody())
                .dateUpdated(currentTime)
                .build();
        return mockRequestRecordMapper.update(mockRequestRecordDO);
    }

    private int create(final MockRequestRecordDTO mockRequestRecordDTO) {
        if (mockRequestRecordDTO == null) {
            return 0;
        }
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        MockRequestRecordDO mockRequestRecordDO = MockRequestRecordDO.builder()
                .id(UUIDUtils.getInstance().generateShortUuid())
                .apiId(mockRequestRecordDTO.getApiId())
                .header(mockRequestRecordDTO.getHeader())
                .host(mockRequestRecordDTO.getHost())
                .query(mockRequestRecordDTO.getQuery())
                .port(mockRequestRecordDTO.getPort())
                .url(mockRequestRecordDTO.getUrl())
                .pathVariable(mockRequestRecordDTO.getPathVariable())
                .body(mockRequestRecordDTO.getBody())
                .dateUpdated(currentTime)
                .dateCreated(currentTime)
                .build();
        return mockRequestRecordMapper.insert(mockRequestRecordDO);
    }

    @Override
    public MockRequestRecordVO queryByApiId(final String apiId) {
        MockRequestRecordQuery mockRequestRecordQuery = new MockRequestRecordQuery();
        mockRequestRecordQuery.setApiId(apiId);
        List<MockRequestRecordDO> mockRequestRecordDOList = mockRequestRecordMapper.selectByQuery(mockRequestRecordQuery);
        return mockRequestRecordDOList.isEmpty()
                ? new MockRequestRecordVO()
                : MockRequestRecordVO.buildMockRequestRecordVO(mockRequestRecordDOList.get(0));
    }
}
