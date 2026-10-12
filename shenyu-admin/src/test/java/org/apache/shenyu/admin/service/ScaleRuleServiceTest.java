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

import org.apache.shenyu.admin.mapper.ScaleRuleMapper;
import org.apache.shenyu.admin.model.dto.ScaleRuleDTO;
import org.apache.shenyu.admin.model.entity.ScaleRuleDO;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.ScaleRuleQuery;
import org.apache.shenyu.admin.scale.monitor.subject.cache.ScaleRuleCache;
import org.apache.shenyu.admin.service.impl.ScaleRuleServiceImpl;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.Test;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.Mock;
import org.springframework.transaction.support.TransactionSynchronizationManager;
import org.springframework.transaction.support.TransactionSynchronizationUtils;

import java.sql.Timestamp;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * Test cases for ScaleRuleService.
 */
@ExtendWith(MockitoExtension.class)
public final class ScaleRuleServiceTest {

    @Mock
    private ScaleRuleMapper scaleRuleMapper;

    @Mock
    private ScaleRuleCache scaleRuleCache;

    private ScaleRuleService scaleRuleService;

    @BeforeEach
    public void setUp() {
        scaleRuleService = new ScaleRuleServiceImpl(scaleRuleMapper, scaleRuleCache);
    }

    @AfterEach
    public void tearDown() {
        if (TransactionSynchronizationManager.isSynchronizationActive()) {
            TransactionSynchronizationManager.clearSynchronization();
        }
    }

    @Test
    public void testCreateUpdatesCacheAfterCommit() {
        when(scaleRuleMapper.insertSelective(any(ScaleRuleDO.class))).thenReturn(1);
        TransactionSynchronizationManager.initSynchronization();

        scaleRuleService.create(new ScaleRuleDTO());

        verify(scaleRuleCache, never()).addOrUpdateRuleToCache(any(ScaleRuleDO.class));
        TransactionSynchronizationUtils.invokeAfterCommit(TransactionSynchronizationManager.getSynchronizations());
        verify(scaleRuleCache).addOrUpdateRuleToCache(any(ScaleRuleDO.class));
    }

    @Test
    public void testCreateUpdatesCacheImmediatelyWithoutTransaction() {
        when(scaleRuleMapper.insertSelective(any(ScaleRuleDO.class))).thenReturn(1);

        scaleRuleService.create(new ScaleRuleDTO());

        verify(scaleRuleCache).addOrUpdateRuleToCache(any(ScaleRuleDO.class));
    }

    @Test
    public void testReadUpdateAndDelete() {
        Timestamp timestamp = new Timestamp(System.currentTimeMillis());
        when(scaleRuleMapper.selectAll()).thenReturn(java.util.Collections.singletonList(ScaleRuleDO.builder().id("r")
                .dateCreated(timestamp).dateUpdated(timestamp).build()));
        scaleRuleService.selectAll();
        scaleRuleService.findById("r");
        ScaleRuleQuery query = new ScaleRuleQuery();
        query.setPageParameter(new PageParameter());
        when(scaleRuleMapper.selectByQuery(query)).thenReturn(java.util.Collections.emptyList());
        scaleRuleService.listByPage(query);
        ScaleRuleDTO dto = new ScaleRuleDTO();
        dto.setId("r");
        dto.setMetricName("new");
        when(scaleRuleMapper.selectByPrimaryKey("r")).thenReturn(ScaleRuleDO.builder().id("r").metricName("old").build(),
                ScaleRuleDO.builder().id("r").metricName("new").build());
        when(scaleRuleMapper.updateByPrimaryKeySelective(any(ScaleRuleDO.class))).thenReturn(1);
        TransactionSynchronizationManager.initSynchronization();
        scaleRuleService.update(dto);
        TransactionSynchronizationUtils.invokeAfterCommit(TransactionSynchronizationManager.getSynchronizations());
        verify(scaleRuleCache).removeRulesFromCache(any());
        verify(scaleRuleCache).addOrUpdateRuleToCache(any(ScaleRuleDO.class));
        TransactionSynchronizationManager.clearSynchronization();
        when(scaleRuleMapper.delete(any())).thenReturn(1);
        scaleRuleService.delete(java.util.Collections.singletonList("r"));
        verify(scaleRuleCache).removeRulesByIdsFromCache(any());
    }

    @Test
    public void testCreateOrUpdateDispatchesAndCreateSkipsCacheWhenInsertFails() {
        when(scaleRuleMapper.insertSelective(any(ScaleRuleDO.class))).thenReturn(0);
        assertEquals(0, scaleRuleService.createOrUpdate(new ScaleRuleDTO()));
        verifyNoInteractions(scaleRuleCache);
        ScaleRuleDTO update = new ScaleRuleDTO();
        update.setId("r");
        ScaleRuleDO persisted = ScaleRuleDO.builder().id("r").metricName("metric").build();
        when(scaleRuleMapper.selectByPrimaryKey("r")).thenReturn(null, persisted);
        when(scaleRuleMapper.updateByPrimaryKeySelective(any(ScaleRuleDO.class))).thenReturn(1);
        assertEquals(1, scaleRuleService.createOrUpdate(update));
        verify(scaleRuleMapper).insertSelective(any(ScaleRuleDO.class));
        verify(scaleRuleMapper).updateByPrimaryKeySelective(any(ScaleRuleDO.class));
        verify(scaleRuleCache).addOrUpdateRuleToCache(persisted);
    }

    @Test
    public void testReadMethodsWithResults() {
        Timestamp timestamp = new Timestamp(System.currentTimeMillis());
        ScaleRuleDO rule = ScaleRuleDO.builder().id("r").metricName("metric")
                .dateCreated(timestamp).dateUpdated(timestamp).build();
        ScaleRuleQuery query = new ScaleRuleQuery();
        query.setPageParameter(new PageParameter());
        when(scaleRuleMapper.selectByQuery(query)).thenReturn(java.util.Collections.singletonList(rule));
        assertEquals(1, scaleRuleService.listByPage(query).getDataList().size());
        when(scaleRuleMapper.selectByPrimaryKey("r")).thenReturn(rule);
        assertEquals("r", scaleRuleService.findById("r").getId());
    }

    @Test
    public void testUpdateAndDeleteSkipCacheWhenMapperReturnsZero() {
        ScaleRuleDTO dto = new ScaleRuleDTO();
        dto.setId("r");
        when(scaleRuleMapper.selectByPrimaryKey("r")).thenReturn(null);
        when(scaleRuleMapper.updateByPrimaryKeySelective(any(ScaleRuleDO.class))).thenReturn(0);
        assertEquals(0, scaleRuleService.update(dto));
        when(scaleRuleMapper.delete(any())).thenReturn(0);
        assertEquals(0, scaleRuleService.delete(java.util.Collections.singletonList("r")));
        verifyNoInteractions(scaleRuleCache);
    }

    @Test
    public void testUpdatePreservesCacheWhenMetricNameIsUnchanged() {
        ScaleRuleDTO dto = new ScaleRuleDTO();
        dto.setId("r");
        dto.setMetricName("metric");
        ScaleRuleDO persisted = ScaleRuleDO.builder().id("r").metricName("metric").build();
        when(scaleRuleMapper.selectByPrimaryKey("r")).thenReturn(persisted);
        when(scaleRuleMapper.updateByPrimaryKeySelective(any(ScaleRuleDO.class))).thenReturn(1);
        assertEquals(1, scaleRuleService.update(dto));
        verify(scaleRuleCache, never()).removeRulesFromCache(any());
        verify(scaleRuleCache).addOrUpdateRuleToCache(persisted);
    }

    @Test
    public void testUpdateWithoutPreviousRuleAndDeleteAfterCommit() {
        ScaleRuleDTO dto = new ScaleRuleDTO();
        dto.setId("r");
        when(scaleRuleMapper.selectByPrimaryKey("r")).thenReturn(null,
                ScaleRuleDO.builder().id("r").metricName("metric").build());
        when(scaleRuleMapper.updateByPrimaryKeySelective(any(ScaleRuleDO.class))).thenReturn(1);
        TransactionSynchronizationManager.initSynchronization();
        scaleRuleService.update(dto);
        TransactionSynchronizationUtils.invokeAfterCommit(TransactionSynchronizationManager.getSynchronizations());
        verify(scaleRuleCache, never()).removeRulesFromCache(any());
        verify(scaleRuleCache).addOrUpdateRuleToCache(any(ScaleRuleDO.class));
        TransactionSynchronizationManager.clearSynchronization();
        when(scaleRuleMapper.delete(any())).thenReturn(1);
        TransactionSynchronizationManager.initSynchronization();
        scaleRuleService.delete(java.util.Collections.singletonList("r"));
        verify(scaleRuleCache, never()).removeRulesByIdsFromCache(any());
        TransactionSynchronizationUtils.invokeAfterCommit(TransactionSynchronizationManager.getSynchronizations());
        verify(scaleRuleCache).removeRulesByIdsFromCache(any());
    }
}
