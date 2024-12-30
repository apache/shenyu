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

import org.apache.shenyu.admin.mapper.NamespacePluginRelMapper;
import org.apache.shenyu.admin.mapper.PluginHandleMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.entity.PluginHandleDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.vo.NamespacePluginVO;
import org.apache.shenyu.admin.model.vo.PluginSnapshotVO;
import org.apache.shenyu.admin.service.impl.NamespacePluginServiceImpl;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.ArrayList;
import java.util.List;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class NamespacePluginServiceTest {

    @InjectMocks
    private NamespacePluginServiceImpl namespacePluginService;

    @Mock
    private NamespacePluginRelMapper namespacePluginRelMapper;

    @Mock
    private SelectorMapper selectorMapper;

    @Mock
    private PluginHandleMapper pluginHandleMapper;

    @Test
    public void testActivePluginSnapshot() {
        String namespaceId = "testNamespaceId";
        String pluginId1 = "pluginId1";

        // mock plugin
        NamespacePluginVO mockPlugin1 = Mockito.mock(NamespacePluginVO.class);
        Mockito.when(mockPlugin1.getPluginId()).thenReturn(pluginId1);
        Mockito.when(mockPlugin1.getEnabled()).thenReturn(true);
        List<NamespacePluginVO> namespacePluginVOList = List.of(
                mockPlugin1
        );
        Mockito.when(namespacePluginRelMapper.selectByNamespaceId(namespaceId)).thenReturn(namespacePluginVOList);

        // mock selector
        SelectorDO mockSelector1 = Mockito.mock(SelectorDO.class);
        Mockito.when(mockSelector1.getPluginId()).thenReturn(pluginId1);
        List<SelectorDO> selectorDOList = List.of(
                mockSelector1
        );
        Mockito.when(selectorMapper.selectAllByNamespaceId(namespaceId)).thenReturn(selectorDOList);

        // mock plugin handler
        PluginHandleDO mockPluginHandle1 = Mockito.mock(PluginHandleDO.class);
        Mockito.when(mockPluginHandle1.getPluginId()).thenReturn(pluginId1);
        List<PluginHandleDO> pluginHandleDOList = List.of(
                mockPluginHandle1
        );
        Mockito.when(pluginHandleMapper.selectByPluginIdList(List.of(pluginId1))).thenReturn(pluginHandleDOList);

        // test
        List<PluginSnapshotVO> result = namespacePluginService.activePluginSnapshot(namespaceId);

        // verify
        Assertions.assertEquals(1, result.size());
        PluginSnapshotVO pluginSnapshotVO = result.get(0);
        Assertions.assertEquals(pluginId1, pluginSnapshotVO.getId());
    }

    @Test
    public void testActivePluginSnapshotEmpty() {
        String namespaceId = "testNamespaceId";

        // mock plugin is empty
        Mockito.when(namespacePluginRelMapper.selectByNamespaceId(namespaceId)).thenReturn(new ArrayList<>());

        // test
        List<PluginSnapshotVO> result = namespacePluginService.activePluginSnapshot(namespaceId);

        // verify
        Assertions.assertEquals(0, result.size());
    }

    @Test
    public void testActivePluginSnapshotEmptyEnabled() {
        String namespaceId = "testNamespaceId";
        String pluginId1 = "pluginId1";

        // mock plugin not enabled
        NamespacePluginVO mockPlugin1 = Mockito.mock(NamespacePluginVO.class);
        Mockito.when(mockPlugin1.getPluginId()).thenReturn(pluginId1);
        Mockito.when(mockPlugin1.getEnabled()).thenReturn(false);
        List<NamespacePluginVO> namespacePluginVOList = List.of(mockPlugin1);
        Mockito.when(namespacePluginRelMapper.selectByNamespaceId(namespaceId)).thenReturn(namespacePluginVOList);

        // test
        List<PluginSnapshotVO> result = namespacePluginService.activePluginSnapshot(namespaceId);

        // verify
        Assertions.assertEquals(0, result.size());
    }
}
