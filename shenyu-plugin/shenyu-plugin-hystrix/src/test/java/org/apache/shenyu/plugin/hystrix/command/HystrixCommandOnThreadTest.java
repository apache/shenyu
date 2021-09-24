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

package org.apache.shenyu.plugin.hystrix.command;

import com.netflix.hystrix.HystrixCommand;
import com.netflix.hystrix.HystrixCommandGroupKey;
import com.netflix.hystrix.HystrixCommandKey;
import com.netflix.hystrix.HystrixCommandProperties;
import com.netflix.hystrix.HystrixThreadPoolProperties;
import org.apache.shenyu.common.dto.convert.rule.HystrixHandle;
import org.apache.shenyu.common.dto.convert.rule.HystrixHandle.HystrixThreadPoolConfig;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.junit.Before;
import org.junit.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.test.StepVerifier;

import java.net.InetSocketAddress;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;

/**
 * The Test Case For HystrixCommandOnThread.
 */
public final class HystrixCommandOnThreadTest {

    private HystrixCommandOnThread hystrixCommandOnThread;

    @Before
    public void setUp() {
        HystrixHandle hystrixHandle = new HystrixHandle();
        hystrixHandle.setGroupKey("groupKey");
        hystrixHandle.setCommandKey("commandKey");
        hystrixHandle.setHystrixThreadPoolConfig(new HystrixThreadPoolConfig());
        HystrixCommandProperties.Setter propertiesSetter =
                HystrixCommandProperties.Setter()
                        .withExecutionTimeoutInMilliseconds((int) hystrixHandle.getTimeout())
                        .withCircuitBreakerEnabled(true)
                        .withCircuitBreakerErrorThresholdPercentage(hystrixHandle.getErrorThresholdPercentage())
                        .withCircuitBreakerRequestVolumeThreshold(hystrixHandle.getRequestVolumeThreshold())
                        .withCircuitBreakerSleepWindowInMilliseconds(hystrixHandle.getSleepWindowInMilliseconds());
        HystrixThreadPoolConfig hystrixThreadPoolConfig = hystrixHandle.getHystrixThreadPoolConfig();
        HystrixThreadPoolProperties.Setter threadPoolPropertiesSetter =
                HystrixThreadPoolProperties.Setter()
                        .withCoreSize(hystrixThreadPoolConfig.getCoreSize())
                        .withMaximumSize(hystrixThreadPoolConfig.getMaximumSize())
                        .withMaxQueueSize(hystrixThreadPoolConfig.getMaxQueueSize())
                        .withKeepAliveTimeMinutes(hystrixThreadPoolConfig.getKeepAliveTimeMinutes())
                        .withAllowMaximumSizeToDivergeFromCoreSize(true);
        HystrixCommand.Setter setter = HystrixCommand.Setter
                .withGroupKey(HystrixCommandGroupKey.Factory.asKey(hystrixHandle.getGroupKey()))
                .andCommandKey(HystrixCommandKey.Factory.asKey(hystrixHandle.getCommandKey()))
                .andCommandPropertiesDefaults(propertiesSetter)
                .andThreadPoolPropertiesDefaults(threadPoolPropertiesSetter);
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost:8080/http")
                .remoteAddress(new InetSocketAddress(8092))
                .header("MetaDataCache", "Hello")
                .build());
        hystrixCommandOnThread = new HystrixCommandOnThread(setter, exchange, mock(ShenyuPluginChain.class), "http://callback:8093/test");
    }

    @Test
    public void testGetCallBackUri() {
        assertEquals(hystrixCommandOnThread.getCallBackUri().getHost(), "callback");
    }

    @Test(expected = NullPointerException.class)
    public void testGetFallback() {
        StepVerifier.create(hystrixCommandOnThread.getFallback()).expectSubscription().verifyComplete();
    }
}
