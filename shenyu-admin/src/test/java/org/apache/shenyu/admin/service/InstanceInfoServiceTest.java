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

import org.apache.shenyu.admin.mapper.InstanceInfoMapper;
import org.apache.shenyu.admin.model.entity.InstanceInfoDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.InstanceQuery;
import org.apache.shenyu.admin.model.vo.InstanceInfoVO;
import org.apache.shenyu.admin.service.impl.InstanceInfoServiceImpl;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.sql.Timestamp;
import java.util.Collections;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public final class InstanceInfoServiceTest {

    @InjectMocks
    private InstanceInfoServiceImpl instanceInfoService;

    @Mock
    private InstanceInfoMapper instanceInfoMapper;

    private InstanceInfoVO vo;

    @BeforeEach
    void setUp() {
        instanceInfoService = new InstanceInfoServiceImpl(instanceInfoMapper);
        vo = buildVO();
    }

    @Test
    void testCreateOrUpdateCreateBranch() {
        when(instanceInfoMapper.selectOneByQuery(any())).thenReturn(null);
        when(instanceInfoMapper.insert(any())).thenReturn(1);
        instanceInfoService.createOrUpdate(vo);
        verify(instanceInfoMapper).insert(any(InstanceInfoDO.class));
        verify(instanceInfoMapper, never()).updateById(any());
    }

    @Test
    void testCreateOrUpdateUpdateBranch() {
        InstanceInfoDO existing = buildDO();
        when(instanceInfoMapper.selectOneByQuery(any())).thenReturn(existing);
        when(instanceInfoMapper.updateById(any())).thenReturn(1);
        instanceInfoService.createOrUpdate(vo);
        verify(instanceInfoMapper).updateById(any(InstanceInfoDO.class));
    }

    @Test
    void testListByPage() {
        InstanceQuery query = new InstanceQuery();
        query.setPageParameter(new PageParameter(1, 10));
        when(instanceInfoMapper.selectByQuery(any())).thenReturn(Collections.singletonList(buildDO()));
        CommonPager<InstanceInfoVO> pager = instanceInfoService.listByPage(query);
        assertNotNull(pager);
        assertThat(pager.getDataList(), hasSize(1));
    }

    @Test
    void testList() {
        when(instanceInfoMapper.selectAll()).thenReturn(Collections.singletonList(buildDO()));
        List<InstanceInfoVO> list = instanceInfoService.list();
        assertThat(list, hasSize(1));
    }

    @Test
    void testFindById() {
        // current implementation returns null
        assertEquals(null, instanceInfoService.findById("any"));
    }

    private InstanceInfoVO buildVO() {
        InstanceInfoVO v = new InstanceInfoVO();
        v.setInstanceIp("127.0.0.1");
        v.setInstancePort("8080");
        v.setInstanceType("grpc");
        v.setInstanceInfo("info");
        v.setNamespaceId("default");
        v.setInstanceState(1);
        v.setLastHeartBeatTime(System.currentTimeMillis());
        v.setDateCreated(new Timestamp(System.currentTimeMillis()));
        v.setDateUpdated(new Timestamp(System.currentTimeMillis()));
        return v;
    }

    private InstanceInfoDO buildDO() {
        InstanceInfoDO d = new InstanceInfoDO();
        d.setId("id-1");
        d.setInstanceIp("127.0.0.1");
        d.setInstancePort("8080");
        d.setInstanceType("grpc");
        d.setInstanceInfo("info");
        d.setNamespaceId("default");
        d.setInstanceState(1);
        d.setLastHeartBeatTime(System.currentTimeMillis());
        d.setDateCreated(new Timestamp(System.currentTimeMillis()));
        d.setDateUpdated(new Timestamp(System.currentTimeMillis()));
        return d;
    }
}
