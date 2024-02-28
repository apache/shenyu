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

package org.apache.shenyu.plugin.hystrix.builder;

import org.apache.shenyu.common.dto.convert.rule.HystrixHandle;
import org.apache.shenyu.common.dto.convert.rule.HystrixHandle.HystrixThreadPoolConfig;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * The Test Case For HystrixBuilder.
 */
public final class HystrixBuilderTest {

    @Test
    public void testBuild() {
        HystrixHandle hystrixHandle = new HystrixHandle();
        hystrixHandle.setGroupKey("groupKey");
        hystrixHandle.setCommandKey("commandKey");
        hystrixHandle.setMaxConcurrentRequests(0);
        hystrixHandle.setErrorThresholdPercentage(0);
        hystrixHandle.setRequestVolumeThreshold(0);
        hystrixHandle.setSleepWindowInMilliseconds(0);
        hystrixHandle.setHystrixThreadPoolConfig(null);
        assertNotNull(HystrixBuilder.build(hystrixHandle));
        HystrixThreadPoolConfig hystrixThreadPoolConfig = new HystrixThreadPoolConfig();
        hystrixThreadPoolConfig.setCoreSize(0);
        hystrixThreadPoolConfig.setMaximumSize(0);
        hystrixThreadPoolConfig.setMaxQueueSize(0);
        hystrixHandle.setHystrixThreadPoolConfig(hystrixThreadPoolConfig);
        assertNotNull(HystrixBuilder.build(hystrixHandle));
        assertNotNull(HystrixBuilder.buildForHystrixCommand(hystrixHandle));
    }
}
