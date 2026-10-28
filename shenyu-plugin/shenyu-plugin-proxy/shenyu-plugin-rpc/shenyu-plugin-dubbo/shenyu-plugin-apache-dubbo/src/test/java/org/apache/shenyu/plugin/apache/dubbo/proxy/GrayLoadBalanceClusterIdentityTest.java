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

package org.apache.shenyu.plugin.apache.dubbo.proxy;

import org.apache.dubbo.common.URL;
import org.apache.dubbo.rpc.Invocation;
import org.apache.dubbo.rpc.Invoker;
import org.apache.dubbo.rpc.RpcException;
import org.apache.dubbo.rpc.RpcInvocation;
import org.apache.dubbo.rpc.cluster.Directory;
import org.apache.dubbo.rpc.cluster.support.AbstractClusterInvoker;
import org.apache.dubbo.rpc.support.MockInvoker;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.convert.rule.impl.DubboRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.DubboUpstream;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.loadbalancer.factory.LoadBalancerFactory;
import org.apache.shenyu.plugin.apache.dubbo.handler.ApacheDubboPluginDataHandler;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.ArrayList;
import java.util.List;
import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * Drives the real Dubbo 3.2.x cluster selection/reselection over the gray load balance.
 *
 * <p>Issue #8132: a counting wrapper returned by the load balance entered Dubbo's already-tried
 * list, where raw directory candidates can never compare equal to it, so a retried provider was
 * admitted back into the reselection candidates. The gray load balance must return the bare
 * directory invoker so every Dubbo bookkeeping list keeps canonical provider identity.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class GrayLoadBalanceClusterIdentityTest {

    @Mock
    private DubboRuleHandle dubboRuleHandle;

    @Mock
    private List<DubboUpstream> dubboUpstreams;

    @Mock
    private Directory<Object> directory;

    private MockedStatic<LoadBalancerFactory> loadBalancerFactoryMockedStatic;

    private ApacheDubboGrayLoadBalance grayLoadBalance;

    private Upstream upstreamA;

    private Upstream upstreamB;

    private MockInvoker<Object> providerA;

    private MockInvoker<Object> providerB;

    private RpcInvocation invocation;

    @BeforeEach
    @SuppressWarnings("unchecked")
    public void setup() {
        grayLoadBalance = new ApacheDubboGrayLoadBalance();
        ApacheDubboPluginDataHandler.RULE_CACHED_HANDLE.get()
                .cachedHandle(Constants.DUBBO_RULE_ID, dubboRuleHandle);
        ApacheDubboPluginDataHandler.SELECTOR_CACHED_HANDLE.get()
                .cachedHandle(Constants.DUBBO_SELECTOR_ID, dubboUpstreams);
        when(dubboRuleHandle.getLoadBalance()).thenReturn("leastActive");

        upstreamA = Upstream.builder().url("a:1").build();
        upstreamA.getInflight().set(1);
        upstreamB = Upstream.builder().url("b:1").build();
        upstreamB.getInflight().set(1);
        providerA = new MockInvoker<>(URL.valueOf("dubbo://a:1"), Object.class);
        providerB = new MockInvoker<>(URL.valueOf("dubbo://b:1"), Object.class);

        when(directory.getConsumerUrl()).thenReturn(URL.valueOf("dubbo://consumer:1?loadbalance=shenyuDeterministic"));
        loadBalancerFactoryMockedStatic = mockStatic(LoadBalancerFactory.class);
        // the gray pick is deterministic: the first attempt targets provider A, the reselect targets B
        loadBalancerFactoryMockedStatic.when(() -> LoadBalancerFactory.selector(any(), any(), any()))
                .thenReturn(upstreamA);

        invocation = new RpcInvocation();
        invocation.setMethodName("m");
        invocation.setAttachment(Constants.DUBBO_SELECTOR_ID, Constants.DUBBO_SELECTOR_ID);
        invocation.setAttachment(Constants.DUBBO_RULE_ID, Constants.DUBBO_RULE_ID);
    }

    @AfterEach
    public void tearDown() {
        loadBalancerFactoryMockedStatic.close();
        ApacheDubboPluginDataHandler.RULE_CACHED_HANDLE.get().getAllCache().clear();
        ApacheDubboPluginDataHandler.SELECTOR_CACHED_HANDLE.get().getAllCache().clear();
    }

    @Test
    public void reselectMustExcludeTheProviderALreadyTriedThroughTheGrayPath() throws Exception {
        TestClusterInvoker clusterInvoker = new TestClusterInvoker(directory);
        ArrayList<Invoker<Object>> alreadyTried = new ArrayList<>();

        Invoker<Object> firstPick = clusterInvoker.pick(grayLoadBalance, invocation,
                Arrays.asList(providerA, providerB), alreadyTried);
        alreadyTried.add(firstPick);
        assertEquals("a:1", firstPick.getUrl().getAddress(), "the first pick targets provider A");

        Invoker<Object> secondPick = clusterInvoker.pick(grayLoadBalance, invocation,
                Arrays.asList(providerA, providerB), alreadyTried);

        assertEquals("b:1", secondPick.getUrl().getAddress(),
                "provider A is already tried, so the reselect must choose provider B");
    }

    /**
     * Exposes the protected cluster selection, which is where Dubbo performs its
     * already-tried bookkeeping and reselection.
     */
    private static final class TestClusterInvoker extends AbstractClusterInvoker<Object> {

        TestClusterInvoker(final Directory<Object> directory) {
            super(directory, URL.valueOf("dubbo://consumer:1"));
        }

        Invoker<Object> pick(final org.apache.dubbo.rpc.cluster.LoadBalance loadBalance, final Invocation inv,
                             final List<Invoker<Object>> candidates, final List<Invoker<Object>> tried) throws RpcException {
            return select(loadBalance, inv, candidates, tried);
        }

        @Override
        protected org.apache.dubbo.rpc.Result doInvoke(final Invocation inv, final List<Invoker<Object>> invokers,
                                                       final org.apache.dubbo.rpc.cluster.LoadBalance loadBalance) {
            return null;
        }
    }
}
