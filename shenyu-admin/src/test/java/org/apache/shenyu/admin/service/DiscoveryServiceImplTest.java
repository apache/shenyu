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

import org.apache.shenyu.admin.discovery.DiscoveryProcessor;
import org.apache.shenyu.admin.discovery.DiscoveryProcessorHolder;
import org.apache.shenyu.admin.mapper.DiscoveryHandlerMapper;
import org.apache.shenyu.admin.mapper.DiscoveryMapper;
import org.apache.shenyu.admin.mapper.DiscoveryRelMapper;
import org.apache.shenyu.admin.mapper.ProxySelectorMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.service.impl.DiscoveryServiceImpl;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.exception.ShenyuException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.Collections;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

/**
 * Test cases for DiscoveryServiceImpl.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class DiscoveryServiceImplTest {

    @Mock
    private DiscoveryMapper discoveryMapper;

    @Mock
    private ProxySelectorMapper proxySelectorMapper;

    @Mock
    private DiscoveryRelMapper discoveryRelMapper;

    @Mock
    private DiscoveryHandlerMapper discoveryHandlerMapper;

    @Mock
    private SelectorService selectorService;

    @Mock
    private SelectorMapper selectorMapper;

    @Mock
    private DiscoveryProcessorHolder discoveryProcessorHolder;

    @Mock
    private DiscoveryProcessor discoveryProcessor;

    private DiscoveryServiceImpl discoveryService;

    @BeforeEach
    public void setUp() {
        discoveryService = new DiscoveryServiceImpl(discoveryMapper, proxySelectorMapper, discoveryRelMapper,
                discoveryHandlerMapper, selectorService, selectorMapper, discoveryProcessorHolder);
        given(discoveryProcessorHolder.chooseProcessor(anyString())).willReturn(discoveryProcessor);
    }

    @Test
    public void testDelete() {
        DiscoveryDO discoveryDO = DiscoveryDO.builder()
                .id("discovery-1")
                .discoveryType("local")
                .namespaceId(SYS_DEFAULT_NAMESPACE_ID)
                .build();
        given(discoveryHandlerMapper.selectByDiscoveryId("discovery-1")).willReturn(Collections.emptyList());
        given(discoveryMapper.selectById("discovery-1")).willReturn(discoveryDO);
        given(discoveryMapper.delete("discovery-1", SYS_DEFAULT_NAMESPACE_ID)).willReturn(1);

        assertEquals(ShenyuResultMessage.DELETE_SUCCESS, discoveryService.delete("discovery-1", SYS_DEFAULT_NAMESPACE_ID));
        verify(discoveryProcessor).removeDiscovery(discoveryDO);
        verify(discoveryMapper).delete("discovery-1", SYS_DEFAULT_NAMESPACE_ID);
    }

    @Test
    public void testDeleteWithMismatchedNamespace() {
        DiscoveryDO discoveryDO = DiscoveryDO.builder()
                .id("discovery-1")
                .discoveryType("local")
                .namespaceId("another-namespace-id")
                .build();
        given(discoveryHandlerMapper.selectByDiscoveryId("discovery-1")).willReturn(Collections.emptyList());
        given(discoveryMapper.selectById("discovery-1")).willReturn(discoveryDO);

        assertThrows(ShenyuException.class, () -> discoveryService.delete("discovery-1", SYS_DEFAULT_NAMESPACE_ID));
        verify(discoveryMapper, never()).delete(any(), any());
        verify(discoveryProcessor, never()).removeDiscovery(any());
    }

    @Test
    public void testDeleteWithDiscoveryNotFound() {
        given(discoveryHandlerMapper.selectByDiscoveryId("discovery-1")).willReturn(Collections.emptyList());
        given(discoveryMapper.selectById("discovery-1")).willReturn(null);

        assertThrows(ShenyuException.class, () -> discoveryService.delete("discovery-1", SYS_DEFAULT_NAMESPACE_ID));
        verify(discoveryMapper, never()).delete(any(), any());
        verify(discoveryProcessor, never()).removeDiscovery(any());
    }
}
