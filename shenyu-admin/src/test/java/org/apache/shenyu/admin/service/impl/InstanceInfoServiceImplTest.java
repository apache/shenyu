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

import org.apache.shenyu.admin.mapper.InstanceInfoMapper;
import org.apache.shenyu.admin.model.entity.InstanceInfoDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.InstanceQuery;
import org.apache.shenyu.admin.model.vo.InstanceInfoVO;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;

/**
 * Test case for {@link InstanceInfoServiceImpl}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class InstanceInfoServiceImplTest {

    @InjectMocks
    private InstanceInfoServiceImpl instanceInfoService;

    @Mock
    private InstanceInfoMapper instanceInfoMapper;

    @Test
    public void testFindByIdExposesRecordId() {
        given(instanceInfoMapper.selectById("instance-1")).willReturn(buildDO("instance-1"));
        InstanceInfoVO result = instanceInfoService.findById("instance-1");
        assertNotNull(result);
        assertEquals("instance-1", result.getId());
    }

    @Test
    public void testListByPageExposesDistinctIds() {
        given(instanceInfoMapper.selectByQuery(any(InstanceQuery.class)))
                .willReturn(Arrays.asList(buildDO("instance-1"), buildDO("instance-2")));
        CommonPager<InstanceInfoVO> pager = instanceInfoService.listByPage(
                new InstanceQuery(new PageParameter(1, 10), "grpc", null, null, "ns"));
        assertEquals(2, pager.getDataList().size());
        InstanceInfoVO first = pager.getDataList().get(0);
        InstanceInfoVO second = pager.getDataList().get(1);
        assertEquals("instance-1", first.getId());
        assertEquals("instance-2", second.getId());
        assertNotEquals(first.getId(), second.getId());
    }

    private InstanceInfoDO buildDO(final String id) {
        return InstanceInfoDO.builder()
                .id(id)
                .instanceIp("127.0.0.1")
                .instancePort("8080")
                .instanceType("grpc")
                .instanceInfo("info")
                .instanceState(1)
                .namespaceId("ns")
                .build();
    }
}
