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

package org.apache.shenyu.plugin.alibaba.dubbo.proxy;

import com.alibaba.dubbo.common.URL;
import com.alibaba.dubbo.rpc.Invocation;
import com.alibaba.dubbo.rpc.Invoker;
import com.alibaba.dubbo.rpc.support.MockInvoker;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.convert.rule.impl.DubboRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.DubboUpstream;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.loadbalancer.factory.LoadBalancerFactory;
import org.apache.shenyu.plugin.alibaba.dubbo.handler.AlibabaDubboPluginDataHandler;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * AlibabaDubboGrayLoadBalanceTest.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class AlibabaDubboGrayLoadBalanceTest {

    private AlibabaDubboGrayLoadBalance alibabaDubboGrayLoadBalance;

    @Mock
    private Invocation invocation;

    private List<Invoker<Object>> invokers;

    @Mock
    private DubboRuleHandle dubboRuleHandle;

    @Mock
    private List<DubboUpstream> dubboUpstreams;

    @Mock
    private Upstream upstream;

    private MockInvoker<Object> mockInvoker;

    @Mock
    private URL url;

    @BeforeEach
    public void setup() {
        alibabaDubboGrayLoadBalance = new AlibabaDubboGrayLoadBalance();

        AlibabaDubboPluginDataHandler.RULE_CACHED_HANDLE.get()
                .cachedHandle(Constants.DUBBO_RULE_ID, dubboRuleHandle);
        AlibabaDubboPluginDataHandler.SELECTOR_CACHED_HANDLE.get()
                .cachedHandle(Constants.DUBBO_SELECTOR_ID, dubboUpstreams);

        when(dubboRuleHandle.getLoadbalance()).thenReturn("random");
        mockInvoker = new MockInvoker<>(URL.valueOf("localhost"));
        invokers = Collections.singletonList(mockInvoker);
        MockedStatic<LoadBalancerFactory> loadBalancerFactoryMockedStatic = mockStatic(LoadBalancerFactory.class);
        loadBalancerFactoryMockedStatic.when(() -> LoadBalancerFactory.selector(any(), any(), any()))
                .thenReturn(upstream);
        when(invocation.getAttachment(Constants.DUBBO_SELECTOR_ID)).thenReturn(Constants.DUBBO_SELECTOR_ID);
        when(invocation.getAttachment(Constants.DUBBO_RULE_ID)).thenReturn(Constants.DUBBO_RULE_ID);
        when(invocation.getAttachment(Constants.DUBBO_REMOTE_ADDRESS)).thenReturn("localhost");
    }

    @Test
    public void alibabaDubboLoadBalanceTest() {
        assertEquals(alibabaDubboGrayLoadBalance.select(invokers, url, invocation), mockInvoker);
        when(upstream.getUrl()).thenReturn("localhost");
        assertEquals(alibabaDubboGrayLoadBalance.select(invokers, url, invocation), mockInvoker);
        when(upstream.getUrl()).thenReturn(null);
        when(upstream.getGroup()).thenReturn("group");
        assertEquals(alibabaDubboGrayLoadBalance.select(invokers, url, invocation), mockInvoker);
        when(upstream.getGroup()).thenReturn(null);
        when(upstream.getVersion()).thenReturn("version");
        assertEquals(alibabaDubboGrayLoadBalance.select(invokers, url, invocation), mockInvoker);

        AlibabaDubboPluginDataHandler.SELECTOR_CACHED_HANDLE.get()
                .removeHandle(Constants.DUBBO_SELECTOR_ID);
        assertEquals(alibabaDubboGrayLoadBalance.select(invokers, url, invocation), mockInvoker);
    }
}
