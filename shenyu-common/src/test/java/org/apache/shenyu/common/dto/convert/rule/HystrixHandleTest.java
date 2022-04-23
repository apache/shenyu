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
import org.apache.shenyu.common.enums.HystrixIsolationModeEnum;
import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.core.Is.is;

/**
 * Test case for HystrixHandle.
 */
public class HystrixHandleTest {
    
    @Test
    public void testGetterSetter() {
        HystrixHandle handle = new HystrixHandle();
        handle.setGroupKey("groupKey");
        handle.setCommandKey("commandKey");
        handle.setMaxConcurrentRequests(10);
        handle.setErrorThresholdPercentage(50);
        handle.setRequestVolumeThreshold(50);
        handle.setSleepWindowInMilliseconds(1000);
        handle.setTimeout(1000L);
        handle.setCallBackUri("uri");
        handle.setExecutionIsolationStrategy(HystrixIsolationModeEnum.THREAD_POOL.getCode());
        handle.setHystrixThreadPoolConfig(new HystrixHandle.HystrixThreadPoolConfig());
        
        assertThat(handle.getGroupKey(), is("groupKey"));
        assertThat(handle.getCommandKey(), is("commandKey"));
        assertThat(handle.getMaxConcurrentRequests(), is(10));
        assertThat(handle.getErrorThresholdPercentage(), is(50));
        assertThat(handle.getRequestVolumeThreshold(), is(50));
        assertThat(handle.getSleepWindowInMilliseconds(), is(1000));
        assertThat(handle.getTimeout(), is(1000L));
        assertThat(handle.getCallBackUri(), is("uri"));
        assertThat(handle.getExecutionIsolationStrategy(), is(HystrixIsolationModeEnum.THREAD_POOL.getCode()));
        assertThat(handle.getHystrixThreadPoolConfig(), is(notNullValue()));
    }
    
    @Test
    public void testEqualsAndHashCode() {
        HystrixHandle handle1 = new HystrixHandle();
        HystrixHandle handle2 = new HystrixHandle();
        
        assertThat(ImmutableSet.of(handle1, handle2), hasSize(1));
    }
    
    @Test
    public void testGetterSetterOfHystrixThreadPoolConfig() {
        HystrixHandle.HystrixThreadPoolConfig config = new HystrixHandle.HystrixThreadPoolConfig();
        config.setCoreSize(10);
        config.setMaximumSize(20);
        config.setKeepAliveTimeMinutes(2);
        config.setMaxQueueSize(5);
        
        assertThat(config.getCoreSize(), is(10));
        assertThat(config.getMaximumSize(), is(20));
        assertThat(config.getKeepAliveTimeMinutes(), is(2));
        assertThat(config.getMaxQueueSize(), is(5));
    }
    
}
