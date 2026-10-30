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
import org.apache.dubbo.rpc.Invoker;
import org.apache.dubbo.rpc.RpcException;
import org.apache.dubbo.rpc.RpcInvocation;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.Arrays;
import java.util.Collections;
import java.util.concurrent.CompletableFuture;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.when;

/**
 * Tests the inflight accounting that {@link DubboGrayInflightFilter} performs for the
 * gray least-active path: the count covers exactly the RPC, is released exactly once,
 * and the local marker attachment never travels towards the provider.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class DubboGrayInflightFilterTest {

    private static final String SELECTOR_ID = Constants.DUBBO_SELECTOR_ID;

    private final DubboGrayInflightFilter filter = new DubboGrayInflightFilter();

    @Mock
    private Invoker<Object> nextInvoker;

    private Upstream upstream;

    private RpcInvocation invocation;

    @BeforeEach
    public void setUp() {
        upstream = Upstream.builder().url("localhost").build();
        upstream.getInflight().set(1);
        UpstreamCacheManager.getInstance().submit(SELECTOR_ID, Collections.singletonList(upstream));
        invocation = new RpcInvocation();
        invocation.setMethodName("m");
        invocation.setAttachment(Constants.DUBBO_SELECTOR_ID, SELECTOR_ID);
        when(nextInvoker.getUrl()).thenReturn(URL.valueOf("dubbo://localhost"));
    }

    @AfterEach
    public void tearDown() {
        UpstreamCacheManager.getInstance().removeByKey(SELECTOR_ID);
    }

    @Test
    public void countsTheRpcWhileItRuns() {
        markUpstream(invocation);
        CompletableFuture<AppResponse> responseFuture = new CompletableFuture<>();
        when(nextInvoker.invoke(invocation)).thenReturn(new AsyncRpcResult(responseFuture, invocation));

        long initial = upstream.getInflight().get();
        filter.invoke(nextInvoker, invocation);
        assertEquals(initial + 1, upstream.getInflight().get());
        assertFalse(invocation.getObjectAttachments().containsKey(Constants.DUBBO_INFLIGHT_UPSTREAM_IDENTITY),
                "the local marker must be removed before the call continues");

        responseFuture.complete(new AppResponse("ok"));
        assertEquals(initial, upstream.getInflight().get());
    }

    @Test
    public void releasesWhenTheRpcThrows() {
        markUpstream(invocation);
        when(nextInvoker.invoke(invocation)).thenThrow(new RpcException("provider gone"));

        long initial = upstream.getInflight().get();
        assertThrows(RpcException.class, () -> filter.invoke(nextInvoker, invocation));

        assertEquals(initial, upstream.getInflight().get());
    }

    /**
     * An Invoker is a reusable object, so several calls may be in flight at once; each releases
     * its own count rather than sharing a single guard.
     */
    @Test
    public void releasesEveryInvocationOnce() {
        RpcInvocation firstCall = markedInvocation();
        RpcInvocation secondCall = markedInvocation();
        when(nextInvoker.invoke(firstCall)).thenReturn(AsyncRpcResult.newDefaultAsyncResult("first", firstCall));
        when(nextInvoker.invoke(secondCall)).thenReturn(AsyncRpcResult.newDefaultAsyncResult("second", secondCall));

        long initial = upstream.getInflight().get();
        filter.invoke(nextInvoker, firstCall);
        filter.invoke(nextInvoker, secondCall);

        assertEquals(initial, upstream.getInflight().get());
    }

    @Test
    public void countsTheCommittedProviderNotTheMarkedOne() {
        Upstream markedProposal = newUpstream("a:1");
        Upstream committed = newUpstream("b:1");
        cacheUpstreams(markedProposal, committed);
        mark(invocation, markedProposal);
        when(nextInvoker.getUrl()).thenReturn(URL.valueOf("dubbo://b:1"));
        CompletableFuture<AppResponse> responseFuture = new CompletableFuture<>();
        when(nextInvoker.invoke(invocation)).thenReturn(new AsyncRpcResult(responseFuture, invocation));

        long markedInitial = markedProposal.getInflight().get();
        long committedInitial = committed.getInflight().get();
        filter.invoke(nextInvoker, invocation);

        assertEquals(markedInitial, markedProposal.getInflight().get(),
                "the marked proposal is not necessarily the committed provider");
        assertEquals(committedInitial + 1, committed.getInflight().get(),
                "the committed provider carries the attempt");
        assertFalse(invocation.getObjectAttachments().containsKey(Constants.DUBBO_INFLIGHT_UPSTREAM_IDENTITY));

        responseFuture.complete(new AppResponse("ok"));
        assertEquals(committedInitial, committed.getInflight().get());
    }

    @Test
    public void countsTheUpstreamMatchingTheCommittedGroupAndVersion() {
        Upstream versionOne = newUpstream("shared:1", "g", "1");
        Upstream versionTwo = newUpstream("shared:1", "g", "2");
        cacheUpstreams(versionOne, versionTwo);
        mark(invocation, versionTwo);
        when(nextInvoker.getUrl()).thenReturn(URL.valueOf("dubbo://shared:1?group=g&version=2"));
        CompletableFuture<AppResponse> responseFuture = new CompletableFuture<>();
        when(nextInvoker.invoke(invocation)).thenReturn(new AsyncRpcResult(responseFuture, invocation));

        long versionOneInitial = versionOne.getInflight().get();
        long versionTwoInitial = versionTwo.getInflight().get();
        filter.invoke(nextInvoker, invocation);

        assertEquals(versionOneInitial, versionOne.getInflight().get(),
                "the same-address upstream of another version must not be counted");
        assertEquals(versionTwoInitial + 1, versionTwo.getInflight().get(),
                "the committed group/version identifies the accounting upstream");

        responseFuture.complete(new AppResponse("ok"));
        assertEquals(versionTwoInitial, versionTwo.getInflight().get());
    }

    @Test
    public void doesNotCountWhenTheCommittedProviderIsUnknown() {
        Upstream markedProposal = newUpstream("a:1");
        cacheUpstreams(markedProposal);
        mark(invocation, markedProposal);
        when(nextInvoker.getUrl()).thenReturn(URL.valueOf("dubbo://c:1"));
        CompletableFuture<AppResponse> responseFuture = new CompletableFuture<>();
        when(nextInvoker.invoke(invocation)).thenReturn(new AsyncRpcResult(responseFuture, invocation));

        long markedInitial = markedProposal.getInflight().get();
        filter.invoke(nextInvoker, invocation);

        assertEquals(markedInitial, markedProposal.getInflight().get(),
                "an unknown committed provider must not fall back to the marked proposal");
        assertFalse(invocation.getObjectAttachments().containsKey(Constants.DUBBO_INFLIGHT_UPSTREAM_IDENTITY));
        responseFuture.complete(new AppResponse("ok"));
    }

    @Test
    public void countsWhenTheConfiguredGroupIsAbsent() {
        Upstream addressOnly = newUpstream("a:1");
        cacheUpstreams(addressOnly);
        mark(invocation, addressOnly);
        when(nextInvoker.getUrl()).thenReturn(URL.valueOf("dubbo://a:1?group=g&version=2"));
        CompletableFuture<AppResponse> responseFuture = new CompletableFuture<>();
        when(nextInvoker.invoke(invocation)).thenReturn(new AsyncRpcResult(responseFuture, invocation));

        long initial = addressOnly.getInflight().get();
        filter.invoke(nextInvoker, invocation);

        assertEquals(initial + 1, addressOnly.getInflight().get(),
                "a blank configured group/version is a wildcard, matching the load balance");

        responseFuture.complete(new AppResponse("ok"));
        assertEquals(initial, addressOnly.getInflight().get());
    }

    @Test
    public void countsTheSelectedSpecificUpstreamWhenWildcardAndExactOverlap() {
        Upstream wildcard = newUpstream("shared:1");
        Upstream specific = newUpstream("shared:1", "g", "2");
        cacheUpstreams(wildcard, specific);
        mark(invocation, specific);
        when(nextInvoker.getUrl()).thenReturn(URL.valueOf("dubbo://shared:1?group=g&version=2"));
        CompletableFuture<AppResponse> responseFuture = new CompletableFuture<>();
        when(nextInvoker.invoke(invocation)).thenReturn(new AsyncRpcResult(responseFuture, invocation));

        long wildcardInitial = wildcard.getInflight().get();
        long specificInitial = specific.getInflight().get();
        filter.invoke(nextInvoker, invocation);

        assertEquals(wildcardInitial, wildcard.getInflight().get(),
                "the overlapping wildcard entry did not receive this attempt");
        assertEquals(specificInitial + 1, specific.getInflight().get(),
                "the selected specific entry carries the attempt");

        responseFuture.complete(new AppResponse("ok"));
        assertEquals(specificInitial, specific.getInflight().get());
    }

    @Test
    public void keepsTheSelectedWildcardUpstreamWhenItIsCompatible() {
        Upstream wildcard = newUpstream("shared:1");
        Upstream specific = newUpstream("shared:1", "g", "2");
        cacheUpstreams(wildcard, specific);
        mark(invocation, wildcard);
        when(nextInvoker.getUrl()).thenReturn(URL.valueOf("dubbo://shared:1?group=g&version=2"));
        CompletableFuture<AppResponse> responseFuture = new CompletableFuture<>();
        when(nextInvoker.invoke(invocation)).thenReturn(new AsyncRpcResult(responseFuture, invocation));

        long wildcardInitial = wildcard.getInflight().get();
        long specificInitial = specific.getInflight().get();
        filter.invoke(nextInvoker, invocation);

        assertEquals(wildcardInitial + 1, wildcard.getInflight().get(),
                "the entry the load balance selected stays the accounting target");
        assertEquals(specificInitial, specific.getInflight().get());

        responseFuture.complete(new AppResponse("ok"));
    }

    @Test
    public void reselectPrefersTheMostSpecificCompatibleUpstream() {
        Upstream versionOne = newUpstream("shared:1", "g", "1");
        Upstream wildcard = newUpstream("shared:1");
        Upstream versionTwo = newUpstream("shared:1", "g", "2");
        cacheUpstreams(versionOne, wildcard, versionTwo);
        mark(invocation, versionOne);
        when(nextInvoker.getUrl()).thenReturn(URL.valueOf("dubbo://shared:1?group=g&version=2"));
        CompletableFuture<AppResponse> responseFuture = new CompletableFuture<>();
        when(nextInvoker.invoke(invocation)).thenReturn(new AsyncRpcResult(responseFuture, invocation));

        long wildcardInitial = wildcard.getInflight().get();
        long versionTwoInitial = versionTwo.getInflight().get();
        filter.invoke(nextInvoker, invocation);

        assertEquals(wildcardInitial, wildcard.getInflight().get(),
                "a reselection must not fall back to the wildcard entry by list order");
        assertEquals(versionTwoInitial + 1, versionTwo.getInflight().get(),
                "the committed provider's most specific identity carries the attempt");

        responseFuture.complete(new AppResponse("ok"));
    }

    @Test
    public void doesNotCountAmbiguousPartialOverlaps() {
        Upstream groupOnly = newUpstream("shared:1", "g", null);
        Upstream versionOnly = newUpstream("shared:1", null, "2");
        cacheUpstreams(groupOnly, versionOnly);
        mark(invocation, newUpstream("other:1"));
        when(nextInvoker.getUrl()).thenReturn(URL.valueOf("dubbo://shared:1?group=g&version=2"));
        CompletableFuture<AppResponse> responseFuture = new CompletableFuture<>();
        when(nextInvoker.invoke(invocation)).thenReturn(new AsyncRpcResult(responseFuture, invocation));

        long groupOnlyInitial = groupOnly.getInflight().get();
        long versionOnlyInitial = versionOnly.getInflight().get();
        filter.invoke(nextInvoker, invocation);

        assertEquals(groupOnlyInitial, groupOnly.getInflight().get(),
                "an ambiguous overlap must not be attributed by list order");
        assertEquals(versionOnlyInitial, versionOnly.getInflight().get());

        responseFuture.complete(new AppResponse("ok"));
    }

    @Test
    public void passesThroughWithoutTheMarker() {
        when(nextInvoker.invoke(invocation)).thenReturn(AsyncRpcResult.newDefaultAsyncResult("ok", invocation));

        filter.invoke(nextInvoker, invocation);

        assertEquals(1, upstream.getInflight().get(), "no marker, no counting");
    }

    @Test
    public void releasesEvenWhenTheUpstreamMarkerCannotBeResolved() {
        markUpstream(invocation);
        // no upstream under this selector matches the marker url: the marker is still consumed,
        // the call continues, and no counter is touched
        invocation.setAttachment(Constants.DUBBO_SELECTOR_ID, "unknown-selector");
        when(nextInvoker.invoke(invocation)).thenReturn(AsyncRpcResult.newDefaultAsyncResult("ok", invocation));

        assertEquals(1, upstream.getInflight().get());
        filter.invoke(nextInvoker, invocation);
    }

    private Upstream newUpstream(final String url) {
        return newUpstream(url, null, null);
    }

    private Upstream newUpstream(final String url, final String group, final String version) {
        Upstream result = Upstream.builder().url(url).group(group).version(version).build();
        result.getInflight().set(1);
        return result;
    }

    private void cacheUpstreams(final Upstream... upstreams) {
        UpstreamCacheManager.getInstance().submit(SELECTOR_ID, Arrays.asList(upstreams));
    }

    private void markUpstream(final RpcInvocation rpcInvocation) {
        mark(rpcInvocation, upstream);
    }

    private void mark(final RpcInvocation rpcInvocation, final Upstream selected) {
        rpcInvocation.setObjectAttachment(Constants.DUBBO_INFLIGHT_UPSTREAM_IDENTITY, selected.identity().markerKey());
    }

    private RpcInvocation markedInvocation() {
        RpcInvocation rpcInvocation = new RpcInvocation();
        rpcInvocation.setMethodName("m");
        rpcInvocation.setAttachment(Constants.DUBBO_SELECTOR_ID, SELECTOR_ID);
        mark(rpcInvocation, upstream);
        return rpcInvocation;
    }
}
