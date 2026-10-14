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

import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.mapper.RegistryMapper;
import org.apache.shenyu.admin.model.dto.RegistryDTO;
import org.apache.shenyu.admin.model.entity.RegistryDO;
import org.apache.shenyu.admin.model.vo.RegistryVO;
import org.apache.shenyu.admin.service.impl.RegistryServiceImpl;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.Mock;

import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/** Tests for {@link RegistryServiceImpl}. */
@ExtendWith(MockitoExtension.class)
public final class RegistryServiceImplTest {

    @Mock
    private RegistryMapper registryMapper;

    @InjectMocks
    private RegistryServiceImpl registryService;

    @Test
    public void testCreateAndFindMethods() {
        RegistryDTO dto = new RegistryDTO();
        dto.setRegistryId("r1");
        dto.setProtocol("http");
        dto.setAddress("localhost:8848");
        when(registryMapper.selectByRegistryId("r1")).thenReturn(null);
        when(registryMapper.selectAll()).thenReturn(Collections.singletonList(RegistryDO.builder().id("id").registryId("r1").build()));
        assertNotNull(registryService.createOrUpdate(dto));
        verify(registryMapper).insert(any(RegistryDO.class));
        registryService.findById("id");
        registryService.findByRegistryId("r1");
        registryService.listAll();
        verify(registryMapper).selectById("id");
        verify(registryMapper, org.mockito.Mockito.times(2)).selectByRegistryId("r1");
    }

    @Test
    public void testCreateRejectsDuplicateAndUpdateBranches() {
        RegistryDTO dto = new RegistryDTO();
        dto.setRegistryId("r1");
        when(registryMapper.selectByRegistryId("r1")).thenReturn(RegistryDO.builder().id("other").build());
        assertThrows(ShenyuAdminException.class, () -> registryService.createOrUpdate(dto));

        RegistryDTO missingId = new RegistryDTO();
        missingId.setRegistryId("r1");
        missingId.setId(null);
        when(registryMapper.selectByRegistryId("r1")).thenReturn(null);
        when(registryMapper.insert(any(RegistryDO.class))).thenReturn(1);
        assertNotNull(registryService.createOrUpdate(missingId));

        RegistryDTO update = new RegistryDTO();
        update.setId("id");
        update.setRegistryId("r1");
        when(registryMapper.selectByRegistryId("r1")).thenReturn(RegistryDO.builder().id("id").build());
        when(registryMapper.updateSelective(any(RegistryDO.class))).thenReturn(0);
        assertNull(registryService.createOrUpdate(update));

        when(registryMapper.selectByRegistryId("r1")).thenReturn(RegistryDO.builder().id("other").build());
        assertThrows(ShenyuAdminException.class, () -> registryService.createOrUpdate(update));
        verify(registryMapper, never()).deleteByIds(any());
    }

    @Test
    public void testUpdateSuccessReturnsRegistry() {
        RegistryDTO update = new RegistryDTO();
        update.setId("id");
        update.setRegistryId("r1");
        update.setProtocol("http");
        update.setAddress("localhost:8848");
        update.setNamespace("registry-namespace");
        update.setGroup("group");
        update.setUsername("user");
        update.setPassword("password");
        when(registryMapper.selectByRegistryId("r1")).thenReturn(RegistryDO.builder().id("id").build());
        when(registryMapper.updateSelective(any(RegistryDO.class))).thenReturn(1);
        RegistryVO result = registryService.createOrUpdate(update);
        assertNotNull(result);
        assertEquals("id", result.getId());
        assertEquals("r1", result.getRegistryId());
        assertEquals("http", result.getProtocol());
        assertEquals("localhost:8848", result.getAddress());
        assertEquals("registry-namespace", result.getNamespace());
        assertEquals("group", result.getGroup());
        assertEquals("user", result.getUsername());
        assertEquals("password", result.getPassword());
    }

    @Test
    public void testListAndDelete() {
        when(registryMapper.countByQuery(any())).thenReturn(0);
        org.apache.shenyu.admin.model.query.RegistryQuery query = new org.apache.shenyu.admin.model.query.RegistryQuery();
        query.setPageParameter(new org.apache.shenyu.admin.model.page.PageParameter());
        registryService.listByPage(query);
        assertEquals(org.apache.shenyu.admin.utils.ShenyuResultMessage.DELETE_SUCCESS,
                registryService.delete(Collections.singletonList("id")));
        verify(registryMapper).deleteByIds(any());
    }

    @Test
    public void testListByPageWithResultsAndFindMappings() {
        org.apache.shenyu.admin.model.query.RegistryQuery query = new org.apache.shenyu.admin.model.query.RegistryQuery();
        query.setPageParameter(new org.apache.shenyu.admin.model.page.PageParameter());
        RegistryDO registry = RegistryDO.builder().id("id").registryId("r1").build();
        when(registryMapper.countByQuery(query)).thenReturn(1);
        when(registryMapper.selectByQuery(query)).thenReturn(Collections.singletonList(registry));
        assertEquals(1, registryService.listByPage(query).getDataList().size());
        when(registryMapper.selectById("id")).thenReturn(registry);
        when(registryMapper.selectByRegistryId("r1")).thenReturn(registry);
        assertEquals("id", registryService.findById("id").getId());
        assertEquals("id", registryService.findByRegistryId("r1").getId());
    }
}
