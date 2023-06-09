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

import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.admin.service.impl.DiscoveryUpstreamServiceImpl;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.BDDMockito.given;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class DiscoveryUpstreamServiceTest {

    @InjectMocks
    private DiscoveryUpstreamServiceImpl discoveryUpstreamService;

    @Mock
    private DiscoveryUpstreamMapper discoveryUpstreamMapper;

    @BeforeEach
    void setUp() {

        discoveryUpstreamService = new DiscoveryUpstreamServiceImpl(discoveryUpstreamMapper);
    }

    @Test
    void createOrUpdate() {

        DiscoveryUpstreamDTO discoveryUpstreamDTO = new DiscoveryUpstreamDTO();
        discoveryUpstreamDTO.setDiscoveryHandlerId("1");
        discoveryUpstreamDTO.setProps("test");
        discoveryUpstreamDTO.setProtocol("test");
        discoveryUpstreamDTO.setUrl("test");
        discoveryUpstreamDTO.setWeight(1);
        discoveryUpstreamDTO.setStatus(1);
        given(discoveryUpstreamMapper.insert(DiscoveryUpstreamDO.buildDiscoveryUpstreamDO(discoveryUpstreamDTO)))
                .willReturn(1);
        assertEquals(ShenyuResultMessage.CREATE_SUCCESS, discoveryUpstreamService.createOrUpdate(discoveryUpstreamDTO));
        discoveryUpstreamDTO.setId("1");
        given(discoveryUpstreamMapper.update(DiscoveryUpstreamDO.buildDiscoveryUpstreamDO(discoveryUpstreamDTO)))
                .willReturn(1);
        assertEquals(ShenyuResultMessage.UPDATE_SUCCESS, discoveryUpstreamService.createOrUpdate(discoveryUpstreamDTO));

    }

    @Test
    void delete() {

        given(discoveryUpstreamMapper.deleteByIds(Arrays.asList("1"))).willReturn(1);
        assertEquals(ShenyuResultMessage.DELETE_SUCCESS, discoveryUpstreamService.delete(Arrays.asList("1")));
    }
}
