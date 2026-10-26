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

import org.apache.shenyu.admin.mapper.ScalePolicyMapper;
import org.apache.shenyu.admin.model.dto.ScalePolicyDTO;
import org.apache.shenyu.admin.model.entity.ScalePolicyDO;
import org.apache.shenyu.admin.scale.scaler.cache.ScalePolicyCache;
import org.apache.shenyu.admin.scale.scaler.ScaleService;
import org.apache.shenyu.admin.service.impl.ScalePolicyServiceImpl;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.Mock;
import org.springframework.transaction.support.TransactionSynchronizationManager;
import org.springframework.transaction.support.TransactionSynchronizationUtils;

import java.sql.Timestamp;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/** Tests for {@link ScalePolicyServiceImpl}. */
@ExtendWith(MockitoExtension.class)
public final class ScalePolicyServiceImplTest {
    @Mock
    private ScalePolicyMapper mapper;

    @Mock
    private ScalePolicyCache cache;

    @Mock
    private ScaleService scaleService;

    @InjectMocks
    private ScalePolicyServiceImpl service;

    @AfterEach
    public void tearDown() {
        if (TransactionSynchronizationManager.isSynchronizationActive()) {
            TransactionSynchronizationManager.clearSynchronization();
        }
    }

    @Test
    public void testSelectAndFind() {
        Timestamp timestamp = new Timestamp(System.currentTimeMillis());
        ScalePolicyDO policy = ScalePolicyDO.builder().id("p").dateCreated(timestamp).dateUpdated(timestamp).build();
        when(mapper.selectAll()).thenReturn(Collections.singletonList(policy));
        assertEquals("p", service.selectAll().get(0).getId());
        when(mapper.selectByPrimaryKey("p")).thenReturn(policy);
        assertEquals("p", service.findById("p").getId());
        verify(mapper).selectByPrimaryKey("p");
    }

    @Test
    public void testUpdateImmediatelyAndAfterCommit() {
        when(mapper.updateByPrimaryKeySelective(any(ScalePolicyDO.class))).thenReturn(1);
        service.update(ScalePolicyDTO.builder().id("p").status(1).build());
        verify(cache, times(1)).updatePolicy(any(ScalePolicyDO.class));
        verify(scaleService).executeScaling();

        TransactionSynchronizationManager.initSynchronization();
        service.update(ScalePolicyDTO.builder().id("p2").status(0).build());
        verify(cache, times(1)).updatePolicy(any(ScalePolicyDO.class));
        TransactionSynchronizationUtils.invokeAfterCommit(TransactionSynchronizationManager.getSynchronizations());
        verify(cache, times(2)).updatePolicy(any(ScalePolicyDO.class));
        verify(scaleService, times(2)).executeScaling();
    }

    @Test
    public void testSelectAllReturnsEmptyList() {
        when(mapper.selectAll()).thenReturn(Collections.emptyList());
        assertEquals(Collections.emptyList(), service.selectAll());
    }

    @Test
    public void testUpdateSkipsPolicyApplicationWhenMapperReturnsZero() {
        when(mapper.updateByPrimaryKeySelective(any(ScalePolicyDO.class))).thenReturn(0);
        assertEquals(0, service.update(ScalePolicyDTO.builder().id("p").status(1).build()));
        verify(cache, org.mockito.Mockito.never()).updatePolicy(any(ScalePolicyDO.class));
        verify(scaleService, org.mockito.Mockito.never()).executeScaling();
    }
}
