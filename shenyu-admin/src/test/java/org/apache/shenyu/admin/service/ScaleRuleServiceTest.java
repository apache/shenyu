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
import org.apache.shenyu.admin.scale.monitor.subject.cache.ScaleRuleCache;
import org.apache.shenyu.admin.service.impl.ScaleRuleServiceImpl;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.transaction.support.TransactionSynchronizationManager;
import org.springframework.transaction.support.TransactionSynchronizationUtils;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
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
}
