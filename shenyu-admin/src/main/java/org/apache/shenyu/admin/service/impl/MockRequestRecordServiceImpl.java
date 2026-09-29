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
import org.apache.shenyu.admin.jpa.repository.MockRequestRecordRepository;
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
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link MockRequestRecordService}.
 */
@Service
public class MockRequestRecordServiceImpl implements MockRequestRecordService {

    private final MockRequestRecordRepository mockRequestRecordRepository;

    public MockRequestRecordServiceImpl(final MockRequestRecordRepository mockRequestRecordRepository) {
        this.mockRequestRecordRepository = mockRequestRecordRepository;

    }

    @Override
    public int createOrUpdate(final MockRequestRecordDTO mockRequestRecordDTO) {
        return StringUtils.isBlank(mockRequestRecordDTO.getId()) ? this.create(mockRequestRecordDTO) : this.update(mockRequestRecordDTO);
    }

    @Override
    public int delete(final String id) {
        return mockRequestRecordRepository.findById(id)
                .map(mockRequestRecordDO -> {
                    mockRequestRecordRepository.delete(mockRequestRecordDO);
                    return 1;
                })
                .orElse(0);
    }

    @Override
    public int batchDelete(final List<String> ids) {
        return mockRequestRecordRepository.deleteByIds(ids);
    }

    @Override
    public MockRequestRecordVO findById(final String id) {
        MockRequestRecordVO mockRequestRecordVO = new MockRequestRecordVO();
        if (StringUtils.isBlank(id)) {
            return mockRequestRecordVO;
        }
        MockRequestRecordDO mockRequestRecordDO = mockRequestRecordRepository.findById(id).orElse(null);
        if (Objects.isNull(mockRequestRecordDO)) {
            return mockRequestRecordVO;
        }
        return MockRequestRecordVO.buildMockRequestRecordVO(mockRequestRecordDO);
    }

    @Override
    public CommonPager<MockRequestRecordVO> listByPage(final MockRequestRecordQuery mockRequestRecordQuery) {
        List<MockRequestRecordDO> list = mockRequestRecordRepository.selectByQuery(mockRequestRecordQuery);
        return PageResultUtils.result(mockRequestRecordQuery.getPageParameter(), () -> list.stream().map(MockRequestRecordVO::buildMockRequestRecordVO).collect(Collectors.toList()));
    }

    private int update(final MockRequestRecordDTO mockRequestRecordDTO) {
        if (Objects.isNull(mockRequestRecordDTO) || Objects.isNull(mockRequestRecordDTO.getId())) {
            return 0;
        }
        return mockRequestRecordRepository.findById(mockRequestRecordDTO.getId())
                .map(mockRequestRecordDO -> {
                    if (StringUtils.isNotBlank(mockRequestRecordDTO.getApiId())) {
                        mockRequestRecordDO.setApiId(mockRequestRecordDTO.getApiId());
                    }
                    if (StringUtils.isNotBlank(mockRequestRecordDTO.getHeader())) {
                        mockRequestRecordDO.setHeader(mockRequestRecordDTO.getHeader());
                    }
                    if (StringUtils.isNotBlank(mockRequestRecordDTO.getHost())) {
                        mockRequestRecordDO.setHost(mockRequestRecordDTO.getHost());
                    }
                    if (Objects.nonNull(mockRequestRecordDTO.getPort())) {
                        mockRequestRecordDO.setPort(mockRequestRecordDTO.getPort());
                    }
                    if (StringUtils.isNotBlank(mockRequestRecordDTO.getQuery())) {
                        mockRequestRecordDO.setQuery(mockRequestRecordDTO.getQuery());
                    }
                    if (StringUtils.isNotBlank(mockRequestRecordDTO.getUrl())) {
                        mockRequestRecordDO.setUrl(mockRequestRecordDTO.getUrl());
                    }
                    if (StringUtils.isNotBlank(mockRequestRecordDTO.getPathVariable())) {
                        mockRequestRecordDO.setPathVariable(mockRequestRecordDTO.getPathVariable());
                    }
                    if (StringUtils.isNotBlank(mockRequestRecordDTO.getBody())) {
                        mockRequestRecordDO.setBody(mockRequestRecordDTO.getBody());
                    }
                    mockRequestRecordDO.setDateUpdated(new Timestamp(System.currentTimeMillis()));
                    mockRequestRecordRepository.save(mockRequestRecordDO);
                    return 1;
                })
                .orElse(0);
    }

    private int create(final MockRequestRecordDTO mockRequestRecordDTO) {
        if (Objects.isNull(mockRequestRecordDTO)) {
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
        mockRequestRecordRepository.save(mockRequestRecordDO);
        return 1;
    }

    @Override
    public MockRequestRecordVO queryByApiId(final String apiId) {
        List<MockRequestRecordDO> mockRequestRecordDOList = mockRequestRecordRepository.findByApiId(apiId);
        return mockRequestRecordDOList.isEmpty()
                ? new MockRequestRecordVO()
                : MockRequestRecordVO.buildMockRequestRecordVO(mockRequestRecordDOList.get(0));
    }
}
