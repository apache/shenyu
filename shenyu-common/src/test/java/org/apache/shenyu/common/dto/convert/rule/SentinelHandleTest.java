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

package org.apache.shenyu.common.dto.convert.rule;

import com.google.common.collect.ImmutableSet;
import org.apache.shenyu.common.constant.Constants;
import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.core.Is.is;
import static org.hamcrest.number.IsCloseTo.closeTo;

/**
 * Test case for SentinelHandle.
 */
public class SentinelHandleTest {
    
    @Test
    public void testGetterSetter() {
        SentinelHandle handle = new SentinelHandle();
        handle.setFlowRuleEnable(0);
        handle.setFlowRuleGrade(2);
        handle.setFlowRuleCount(10);
        handle.setFlowRuleControlBehavior(0);
        handle.setDegradeRuleEnable(0);
        handle.setDegradeRuleGrade(2);
        handle.setDegradeRuleCount(Double.valueOf(10));
        handle.setDegradeRuleTimeWindow(100);
        handle.setDegradeRuleMinRequestAmount(200);
        handle.setDegradeRuleSlowRatioThreshold(Double.valueOf(10));
        handle.setDegradeRuleStatIntervals(100);
        handle.setFallbackUri("fallbackUri");
        
        assertThat(handle.getFlowRuleEnable(), is(0));
        assertThat(handle.getFlowRuleGrade(), is(2));
        assertThat(handle.getFlowRuleCount(), is(10));
        assertThat(handle.getFlowRuleControlBehavior(), is(0));
        assertThat(handle.getDegradeRuleEnable(), is(0));
        assertThat(handle.getDegradeRuleGrade(), is(2));
        assertThat(handle.getDegradeRuleCount(), closeTo(10, 0.1));
        assertThat(handle.getDegradeRuleTimeWindow(), is(100));
        assertThat(handle.getDegradeRuleMinRequestAmount(), is(200));
        assertThat(handle.getDegradeRuleSlowRatioThreshold(), closeTo(10, 0.1));
        assertThat(handle.getDegradeRuleStatIntervals(), is(100));
        assertThat(handle.getFallbackUri(), is("fallbackUri"));
    }
    
    @Test
    public void testEqualsAndHashCode() {
        SentinelHandle handle1 = new SentinelHandle();
        SentinelHandle handle2 = new SentinelHandle();
        
        assertThat(ImmutableSet.of(handle1, handle2), hasSize(1));
    }
    
    @Test
    public void testCheckData() {
        SentinelHandle handle = new SentinelHandle();
        handle.setFlowRuleEnable(0);
        handle.setDegradeRuleEnable(1);
        
        handle.checkData(handle);
        
        assertThat(handle.getFlowRuleEnable(), is(0));
        assertThat(handle.getDegradeRuleEnable(), is(1));
    
        handle.setFlowRuleEnable(-1);
        handle.setDegradeRuleEnable(3);
    
        handle.checkData(handle);
        
        assertThat(handle.getFlowRuleEnable(), is(Constants.SENTINEL_ENABLE_FLOW_RULE));
        assertThat(handle.getDegradeRuleEnable(), is(Constants.SENTINEL_ENABLE_DEGRADE_RULE));
    }
    
}
