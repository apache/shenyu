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

import org.apache.shenyu.admin.mapper.PluginHandleMapper;
import org.apache.shenyu.admin.mapper.ShenyuDictMapper;
import org.apache.shenyu.admin.model.dto.PluginHandleDTO;
import org.apache.shenyu.admin.model.entity.PluginHandleDO;
import org.apache.shenyu.admin.model.result.ConfigImportResult;
import org.apache.shenyu.admin.service.configs.ConfigsImportContext;
import org.apache.shenyu.admin.service.publish.PluginHandleEventPublisher;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.sql.Timestamp;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link PluginHandleServiceImpl}.
 */
@ExtendWith(MockitoExtension.class)
public final class PluginHandleServiceImplTest {

    @InjectMocks
    private PluginHandleServiceImpl pluginHandleService;

    @Mock
    private PluginHandleMapper pluginHandleMapper;

    @Mock
    private ShenyuDictMapper shenyuDictMapper;

    @Mock
    private PluginHandleEventPublisher eventPublisher;

    @Test
    public void findByIdShouldNotThrowForUnknownId() {
        PluginHandleServiceImpl service = new PluginHandleServiceImpl(pluginHandleMapper, shenyuDictMapper, eventPublisher);
        when(pluginHandleMapper.selectById("missing")).thenReturn(null);
        assertNull(service.findById("missing"));
    }

    @Test
    public void importShouldInsertSameFieldWithDifferentType() {
        when(pluginHandleMapper.selectByQuery(any())).thenReturn(List.of(existingHandle("target-plugin", "sharedField", 1)));
        when(pluginHandleMapper.insertSelective(any())).thenReturn(1);
        PluginHandleDTO handle = importHandle("source-plugin", "sharedField", 2);

        ConfigImportResult result = pluginHandleService.importData(List.of(handle), importContext());

        assertEquals(1, result.getSuccessCount());
        ArgumentCaptor<PluginHandleDO> captor = ArgumentCaptor.forClass(PluginHandleDO.class);
        verify(pluginHandleMapper).insertSelective(captor.capture());
        PluginHandleDO inserted = captor.getValue();
        assertEquals("target-plugin", inserted.getPluginId());
        assertEquals("sharedField", inserted.getField());
        assertEquals(2, inserted.getType());
        verify(eventPublisher).onCreated(inserted);
    }

    @Test
    public void importShouldSkipExistingFieldAndType() {
        when(pluginHandleMapper.selectByQuery(any())).thenReturn(List.of(existingHandle("target-plugin", "sharedField", 1)));

        ConfigImportResult result = pluginHandleService.importData(List.of(importHandle("source-plugin", "sharedField", 1)), importContext());

        assertEquals(0, result.getSuccessCount());
        verify(pluginHandleMapper, never()).insertSelective(any());
        verifyNoInteractions(eventPublisher);
    }

    @Test
    public void importShouldTrackInsertedFieldAndType() {
        when(pluginHandleMapper.selectByQuery(any())).thenReturn(List.of(existingHandle("target-plugin", "sharedField", 1)));
        when(pluginHandleMapper.insertSelective(any())).thenReturn(1);
        List<PluginHandleDTO> handles = List.of(importHandle("source-plugin", "sharedField", 1),
                importHandle("source-plugin", "sharedField", 2), importHandle("source-plugin", "sharedField", 2),
                importHandle("source-plugin", "otherField", 2));

        ConfigImportResult result = pluginHandleService.importData(handles, importContext());

        assertEquals(2, result.getSuccessCount());
        ArgumentCaptor<PluginHandleDO> captor = ArgumentCaptor.forClass(PluginHandleDO.class);
        verify(pluginHandleMapper, times(2)).insertSelective(captor.capture());
        assertEquals(List.of("sharedField", "otherField"), captor.getAllValues().stream().map(PluginHandleDO::getField).toList());
        assertEquals(List.of(2, 2), captor.getAllValues().stream().map(PluginHandleDO::getType).toList());
        assertEquals(List.of("target-plugin", "target-plugin"), captor.getAllValues().stream().map(PluginHandleDO::getPluginId).toList());
        verify(eventPublisher, times(2)).onCreated(any(PluginHandleDO.class));
    }

    @Test
    public void importShouldDeduplicateHandlesForNewPlugin() {
        when(pluginHandleMapper.selectByQuery(any())).thenReturn(Collections.emptyList());
        when(pluginHandleMapper.insertSelective(any())).thenReturn(1);
        List<PluginHandleDTO> handles = List.of(importHandle("source-plugin", "sharedField", 1),
                importHandle("source-plugin", "sharedField", 1), importHandle("source-plugin", "sharedField", 2));

        ConfigImportResult result = pluginHandleService.importData(handles, importContext());

        assertEquals(2, result.getSuccessCount());
        ArgumentCaptor<PluginHandleDO> captor = ArgumentCaptor.forClass(PluginHandleDO.class);
        verify(pluginHandleMapper, times(2)).insertSelective(captor.capture());
        assertEquals(List.of(1, 2), captor.getAllValues().stream().map(PluginHandleDO::getType).toList());
        verify(eventPublisher, times(2)).onCreated(any(PluginHandleDO.class));
    }

    @Test
    public void importShouldDeduplicateAfterPluginIdRemapping() {
        when(pluginHandleMapper.selectByQuery(any())).thenReturn(Collections.emptyList());
        when(pluginHandleMapper.insertSelective(any())).thenReturn(1);
        ConfigsImportContext context = importContext();
        context.getPluginTemplateIdMapping().put("other-source-plugin", "target-plugin");
        List<PluginHandleDTO> handles = List.of(importHandle("source-plugin", "sharedField", 1),
                importHandle("other-source-plugin", "sharedField", 1));

        ConfigImportResult result = pluginHandleService.importData(handles, context);

        assertEquals(1, result.getSuccessCount());
        ArgumentCaptor<PluginHandleDO> captor = ArgumentCaptor.forClass(PluginHandleDO.class);
        verify(pluginHandleMapper).insertSelective(captor.capture());
        assertEquals("target-plugin", captor.getValue().getPluginId());
        verify(eventPublisher).onCreated(captor.getValue());
    }

    @Test
    public void importShouldSkipUnmappedPlugin() {
        when(pluginHandleMapper.selectByQuery(any())).thenReturn(Collections.emptyList());

        ConfigImportResult result = pluginHandleService.importData(List.of(importHandle("missing-plugin", "sharedField", 1)), importContext());

        assertEquals(0, result.getSuccessCount());
        verify(pluginHandleMapper, never()).insertSelective(any());
        verifyNoInteractions(eventPublisher);
    }

    @Test
    public void importShouldTrackOnlySuccessfulInsertions() {
        when(pluginHandleMapper.selectByQuery(any())).thenReturn(Collections.emptyList());
        when(pluginHandleMapper.insertSelective(any())).thenReturn(0, 1);
        List<PluginHandleDTO> handles = List.of(importHandle("source-plugin", "sharedField", 1),
                importHandle("source-plugin", "sharedField", 1));

        ConfigImportResult result = pluginHandleService.importData(handles, importContext());

        assertEquals(1, result.getSuccessCount());
        verify(pluginHandleMapper, times(2)).insertSelective(any());
        verify(eventPublisher).onCreated(any(PluginHandleDO.class));
    }

    private ConfigsImportContext importContext() {
        ConfigsImportContext context = new ConfigsImportContext();
        context.getPluginTemplateIdMapping().put("source-plugin", "target-plugin");
        return context;
    }

    private PluginHandleDO existingHandle(final String pluginId, final String field, final Integer type) {
        Timestamp timestamp = Timestamp.valueOf("2026-10-07 00:00:00");
        return PluginHandleDO.builder().pluginId(pluginId).field(field).dataType(2).type(type).sort(0)
                .dateCreated(timestamp).dateUpdated(timestamp).build();
    }

    private PluginHandleDTO importHandle(final String pluginId, final String field, final Integer type) {
        PluginHandleDTO handle = new PluginHandleDTO();
        handle.setPluginId(pluginId);
        handle.setField(field);
        handle.setDataType(2);
        handle.setType(type);
        handle.setSort(0);
        return handle;
    }
}
