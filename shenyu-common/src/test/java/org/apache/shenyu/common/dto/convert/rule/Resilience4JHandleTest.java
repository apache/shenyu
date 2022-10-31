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
import static org.hamcrest.Matchers.comparesEqualTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.core.Is.is;

/**
 * Test case for Resilience4JHandle.
 */
public class Resilience4JHandleTest {
    
    @Test
    public void testGetterSetter() {
        Resilience4JHandle handle = new Resilience4JHandle();
        handle.setTimeoutDurationRate(1000);
        handle.setLimitRefreshPeriod(200);
        handle.setLimitForPeriod(100);
        handle.setCircuitEnable(0);
        handle.setTimeoutDuration(100L);
        handle.setFallbackUri("fallbackUri");
        handle.setSlidingWindowSize(200);
        handle.setSlidingWindowType(2);
        handle.setMinimumNumberOfCalls(100);
        handle.setWaitIntervalFunctionInOpenState(20);
        handle.setPermittedNumberOfCallsInHalfOpenState(10);
        handle.setFailureRateThreshold(60F);
        handle.setAutomaticTransitionFromOpenToHalfOpenEnabled(false);
        
        assertThat(handle.getTimeoutDurationRate(), is(1000));
        assertThat(handle.getLimitRefreshPeriod(), is(200));
        assertThat(handle.getLimitForPeriod(), is(100));
        assertThat(handle.getCircuitEnable(), is(0));
        assertThat(handle.getTimeoutDuration(), is(100L));
        assertThat(handle.getFallbackUri(), is("fallbackUri"));
        assertThat(handle.getSlidingWindowSize(), is(200));
        assertThat(handle.getSlidingWindowType(), is(2));
        assertThat(handle.getMinimumNumberOfCalls(), is(100));
        assertThat(handle.getWaitIntervalFunctionInOpenState(), is(20));
        assertThat(handle.getPermittedNumberOfCallsInHalfOpenState(), is(10));
        assertThat(handle.getFailureRateThreshold(), is(comparesEqualTo(60F)));
        assertThat(handle.getAutomaticTransitionFromOpenToHalfOpenEnabled(), is(false));
    }
    
    @Test
    public void testEqualsAndHashCode() {
        Resilience4JHandle handle1 = new Resilience4JHandle();
        Resilience4JHandle handle2 = new Resilience4JHandle();
        
        assertThat(ImmutableSet.of(handle1, handle2), hasSize(1));
    }
    
    @Test
    public void testCheckData() {
        Resilience4JHandle handle = new Resilience4JHandle();
        handle.setTimeoutDurationRate(-1);
        handle.setLimitRefreshPeriod(-2);
        handle.setLimitForPeriod(-3);
        handle.setCircuitEnable(-4);
        handle.setTimeoutDuration(-5);
        handle.setFallbackUri("uri");
        handle.setSlidingWindowSize(-6);
        handle.setSlidingWindowType(-7);
        handle.setMinimumNumberOfCalls(-8);
        handle.setWaitIntervalFunctionInOpenState(-9);
        handle.setPermittedNumberOfCallsInHalfOpenState(-10);
        handle.setFailureRateThreshold(-11);
        
        handle.checkData(handle);
    
        assertThat(handle.getTimeoutDurationRate(), is(Constants.TIMEOUT_DURATION_RATE));
        assertThat(handle.getLimitRefreshPeriod(), is(Constants.LIMIT_REFRESH_PERIOD));
        assertThat(handle.getLimitForPeriod(), is(Constants.LIMIT_FOR_PERIOD));
        assertThat(handle.getCircuitEnable(), is(Constants.CIRCUIT_DISABLE));
        assertThat(handle.getTimeoutDuration(), is(Constants.TIMEOUT_DURATION));
        assertThat(handle.getFallbackUri(), is("uri"));
        assertThat(handle.getSlidingWindowSize(), is(Constants.SLIDING_WINDOW_SIZE));
        assertThat(handle.getSlidingWindowType(), is(Constants.SLIDING_WINDOW_TYPE));
        assertThat(handle.getMinimumNumberOfCalls(), is(Constants.MINIMUM_NUMBER_OF_CALLS));
        assertThat(handle.getWaitIntervalFunctionInOpenState(), is(Constants.WAIT_INTERVAL_FUNCTION_IN_OPEN_STATE));
        assertThat(handle.getPermittedNumberOfCallsInHalfOpenState(), is(Constants.PERMITTED_NUMBER_OF_CALLS_IN_HALF_OPEN_STATE));
        assertThat(handle.getFailureRateThreshold(), is(Constants.FAILURE_RATE_THRESHOLD));
    }
    
}
