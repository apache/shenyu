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
import org.apache.dubbo.rpc.AppResponse;
import org.apache.dubbo.rpc.AsyncRpcResult;
import org.apache.dubbo.rpc.Invocation;
import org.apache.dubbo.rpc.Invoker;
import org.apache.dubbo.rpc.RpcException;
import org.apache.dubbo.rpc.RpcInvocation;
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

import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicLong;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * AlibabaDubboGrayLoadBalanceTest.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ApacheDubboGrayLoadBalanceTest {

    private ApacheDubboGrayLoadBalance apacheDubboGrayLoadBalance;

    private MockedStatic<LoadBalancerFactory> loadBalancerFactoryMockedStatic;

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
        apacheDubboGrayLoadBalance = new ApacheDubboGrayLoadBalance();

        ApacheDubboPluginDataHandler.RULE_CACHED_HANDLE.get()
                .cachedHandle(Constants.DUBBO_RULE_ID, dubboRuleHandle);
        ApacheDubboPluginDataHandler.SELECTOR_CACHED_HANDLE.get()
                .cachedHandle(Constants.DUBBO_SELECTOR_ID, dubboUpstreams);

        when(dubboRuleHandle.getLoadBalance()).thenReturn("random");
        when(upstream.getInflight()).thenReturn(new AtomicLong(1));
        mockInvoker = new MockInvoker<>(URL.valueOf("localhost"), Object.class);
        invokers = Collections.singletonList(mockInvoker);
        loadBalancerFactoryMockedStatic = mockStatic(LoadBalancerFactory.class);
        loadBalancerFactoryMockedStatic.when(() -> LoadBalancerFactory.selector(any(), any(), any()))
                .thenReturn(upstream);
        when(invocation.getAttachment(Constants.DUBBO_SELECTOR_ID)).thenReturn(Constants.DUBBO_SELECTOR_ID);
        when(invocation.getAttachment(Constants.DUBBO_RULE_ID)).thenReturn(Constants.DUBBO_RULE_ID);
        when(invocation.getAttachment(Constants.DUBBO_REMOTE_ADDRESS)).thenReturn("localhost");
    }

    @AfterEach
    public void tearDown() {
        // Release the thread-local static mock so later tests use the real selector.
        loadBalancerFactoryMockedStatic.close();
    }

    @Test
    public void leastActiveShouldCountTheRpcWhileItRuns() {
        RpcInvocation rpcInvocation = grayInvocation();
        when(dubboRuleHandle.getLoadBalance()).thenReturn("leastActive");
        when(upstream.getUrl()).thenReturn("localhost");
        CompletableFuture<AppResponse> responseFuture = new CompletableFuture<>();
        Invoker<Object> rpcInvoker = mock(Invoker.class);
        when(rpcInvoker.getUrl()).thenReturn(mockInvoker.getUrl());
        when(rpcInvoker.invoke(rpcInvocation)).thenReturn(new AsyncRpcResult(responseFuture, rpcInvocation));

        long initial = upstream.getInflight().get();
        Invoker<Object> selected = apacheDubboGrayLoadBalance.select(Collections.singletonList(rpcInvoker), url, rpcInvocation);
        selected.invoke(rpcInvocation);
        assertEquals(initial + 1, upstream.getInflight().get());

        responseFuture.complete(new AppResponse("ok"));
        assertEquals(initial, upstream.getInflight().get());
    }

    @Test
    public void leastActiveShouldReleaseWhenTheRpcThrows() {
        RpcInvocation rpcInvocation = grayInvocation();
        when(dubboRuleHandle.getLoadBalance()).thenReturn("leastActive");
        when(upstream.getUrl()).thenReturn("localhost");
        Invoker<Object> failing = mock(Invoker.class);
        when(failing.getUrl()).thenReturn(mockInvoker.getUrl());
        when(failing.invoke(rpcInvocation)).thenThrow(new RpcException("provider gone"));

        long initial = upstream.getInflight().get();
        Invoker<Object> selected = apacheDubboGrayLoadBalance.select(Collections.singletonList(failing), url, rpcInvocation);
        assertThrows(RpcException.class, () -> selected.invoke(rpcInvocation));

        assertEquals(initial, upstream.getInflight().get());
    }

    /**
     * An Invoker is a reusable object, so the same wrapper may serve several calls; each one releases its own
     * count rather than sharing a single guard.
     */
    @Test
    public void leastActiveShouldReleaseEveryInvocationOnAReusedWrapper() {
        RpcInvocation firstCall = grayInvocation();
        RpcInvocation secondCall = grayInvocation();
        when(dubboRuleHandle.getLoadBalance()).thenReturn("leastActive");
        when(upstream.getUrl()).thenReturn("localhost");
        Invoker<Object> rpcInvoker = mock(Invoker.class);
        when(rpcInvoker.getUrl()).thenReturn(mockInvoker.getUrl());
        when(rpcInvoker.invoke(firstCall)).thenReturn(AsyncRpcResult.newDefaultAsyncResult("first", firstCall));
        when(rpcInvoker.invoke(secondCall)).thenReturn(AsyncRpcResult.newDefaultAsyncResult("second", secondCall));

        long initial = upstream.getInflight().get();
        Invoker<Object> selected = apacheDubboGrayLoadBalance.select(Collections.singletonList(rpcInvoker), url, firstCall);
        selected.invoke(firstCall);
        selected.invoke(secondCall);

        assertEquals(initial, upstream.getInflight().get());
    }

    @Test
    public void otherAlgorithmsShouldReturnTheSelectedInvokerUnchanged() {
        when(upstream.getUrl()).thenReturn("localhost");

        assertSame(mockInvoker, apacheDubboGrayLoadBalance.select(invokers, url, invocation));
    }

    /**
     * Dubbo's cluster keeps the returned invoker in its already-tried and connectivity-invalidation sets and
     * compares by identity, so a counting wrapper has to compare equal to the invoker it wraps.
     */
    @Test
    public void countingWrapperShouldKeepTheIdentityOfTheWrappedInvoker() {
        when(dubboRuleHandle.getLoadBalance()).thenReturn("leastActive");
        when(upstream.getUrl()).thenReturn("localhost");

        Invoker<Object> selected = apacheDubboGrayLoadBalance.select(invokers, url, invocation);

        assertNotSame(mockInvoker, selected);
        assertEquals(selected, mockInvoker);
        assertEquals(mockInvoker.hashCode(), selected.hashCode());
        assertTrue(invokers.contains(selected));
        assertEquals(0, invokers.indexOf(selected));
    }

    /**
     * An invocation carrying the attachments gray routing reads.
     *
     * @return the invocation
     */
    private RpcInvocation grayInvocation() {
        RpcInvocation rpcInvocation = new RpcInvocation();
        rpcInvocation.setAttachment(Constants.DUBBO_SELECTOR_ID, Constants.DUBBO_SELECTOR_ID);
        rpcInvocation.setAttachment(Constants.DUBBO_RULE_ID, Constants.DUBBO_RULE_ID);
        rpcInvocation.setAttachment(Constants.DUBBO_REMOTE_ADDRESS, "localhost");
        return rpcInvocation;
    }

    @Test
    public void apacheDubboLoadBalanceTest() {
        assertEquals(apacheDubboGrayLoadBalance.select(invokers, url, invocation), mockInvoker);
        when(upstream.getUrl()).thenReturn("localhost");
        assertEquals(apacheDubboGrayLoadBalance.select(invokers, url, invocation), mockInvoker);
        when(upstream.getUrl()).thenReturn(null);
        when(upstream.getGroup()).thenReturn("group");
        assertEquals(apacheDubboGrayLoadBalance.select(invokers, url, invocation), mockInvoker);
        when(upstream.getGroup()).thenReturn(null);
        when(upstream.getVersion()).thenReturn("version");
        assertEquals(apacheDubboGrayLoadBalance.select(invokers, url, invocation), mockInvoker);

        ApacheDubboPluginDataHandler.SELECTOR_CACHED_HANDLE.get()
                .removeHandle(Constants.DUBBO_SELECTOR_ID);
        assertEquals(apacheDubboGrayLoadBalance.select(invokers, url, invocation), mockInvoker);
    }
}
