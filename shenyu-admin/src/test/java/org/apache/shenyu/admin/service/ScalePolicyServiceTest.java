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
import org.apache.shenyu.admin.scale.scaler.ScaleService;
import org.apache.shenyu.admin.scale.scaler.cache.ScalePolicyCache;
import org.apache.shenyu.admin.service.impl.ScalePolicyServiceImpl;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Date;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link ScalePolicyService}.
 */
@ExtendWith(MockitoExtension.class)
public final class ScalePolicyServiceTest {

    @InjectMocks
    private ScalePolicyServiceImpl scalePolicyService;

    @Mock
    private ScalePolicyMapper scalePolicyMapper;

    @Mock
    private ScalePolicyCache scalePolicyCache;

    @Mock
    private ScaleService scaleService;

    @Test
    public void testUpdateCachesPersistedPolicy() {
        String policyId = "policy-id";
        ScalePolicyDTO partialPolicy = new ScalePolicyDTO(policyId, 1, 1, null, null, null);
        ScalePolicyDO persistedPolicy = ScalePolicyDO.builder()
                .id(policyId)
                .sort(1)
                .status(1)
                .num(3)
                .beginTime(new Date())
                .endTime(new Date())
                .build();
        when(scalePolicyMapper.updateByPrimaryKeySelective(any(ScalePolicyDO.class))).thenReturn(1);
        when(scalePolicyMapper.selectByPrimaryKey(policyId)).thenReturn(persistedPolicy);

        int rows = scalePolicyService.update(partialPolicy);

        assertEquals(1, rows);
        verify(scalePolicyCache).updatePolicy(persistedPolicy);
        verify(scaleService).executeScaling();
    }
}
