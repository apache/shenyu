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

package org.apache.shenyu.admin.scale.scaler;

import org.apache.shenyu.admin.mapper.ScalePolicyMapper;
import org.apache.shenyu.admin.mapper.ScaleRuleMapper;
import org.apache.shenyu.admin.model.entity.ScalePolicyDO;
import org.apache.shenyu.admin.model.entity.ScaleRuleDO;
import org.apache.shenyu.admin.scale.monitor.subject.cache.ScaleRuleCache;
import org.apache.shenyu.admin.scale.scaler.cache.ScalePolicyCache;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InOrder;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;

import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link ScaleLifecycleService}.
 */
@ExtendWith(MockitoExtension.class)
public final class ScaleLifecycleServiceTest {

    @Mock
    private ScalePolicyMapper scalePolicyMapper;

    @Mock
    private ScalePolicyCache scalePolicyCache;

    @Mock
    private ScaleRuleMapper scaleRuleMapper;

    @Mock
    private ScaleRuleCache scaleRuleCache;

    @Mock
    private ScaleService scaleService;

    private ScaleLifecycleService scaleLifecycleService;

    @BeforeEach
    public void setUp() {
        scaleLifecycleService = new ScaleLifecycleService(
                scalePolicyMapper, scalePolicyCache, scaleService, scaleRuleMapper, scaleRuleCache);
    }

    @Test
    public void testStartLoadsCachesAndStartsScaling() {
        final List<ScalePolicyDO> policies = List.of(new ScalePolicyDO());
        final List<ScaleRuleDO> rules = List.of(new ScaleRuleDO());
        when(scalePolicyMapper.selectAll()).thenReturn(policies);
        when(scaleRuleMapper.selectAll()).thenReturn(rules);

        scaleLifecycleService.start();

        final InOrder order = inOrder(scalePolicyMapper, scalePolicyCache, scaleRuleMapper, scaleRuleCache, scaleService);
        order.verify(scalePolicyMapper).selectAll();
        order.verify(scalePolicyCache).initialize(policies);
        order.verify(scaleRuleMapper).selectAll();
        order.verify(scaleRuleCache).initialize(rules);
        order.verify(scaleService).start();
    }

    @Test
    public void testStartReloadsCachesEachTime() {
        when(scalePolicyMapper.selectAll()).thenReturn(List.of());
        when(scaleRuleMapper.selectAll()).thenReturn(List.of());

        scaleLifecycleService.start();
        scaleLifecycleService.start();

        verify(scalePolicyMapper, times(2)).selectAll();
        verify(scalePolicyCache, times(2)).initialize(List.of());
        verify(scaleRuleMapper, times(2)).selectAll();
        verify(scaleRuleCache, times(2)).initialize(List.of());
        verify(scaleService, times(2)).start();
    }

    @Test
    public void testStopStopsScaling() {
        scaleLifecycleService.stop();

        verify(scaleService).stop();
    }
}
