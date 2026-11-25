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

package org.apache.shenyu.plugin.grpc.resolver;

import io.grpc.NameResolver;
import io.grpc.Status;
import io.grpc.SynchronizationContext;
import io.grpc.internal.SharedResourceHolder;
import org.apache.shenyu.common.dto.convert.selector.GrpcUpstream;
import org.apache.shenyu.plugin.grpc.cache.ApplicationConfigCache;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.Executor;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Tests for {@link ShenyuNameResolver} upstream notifications.
 */
public class ShenyuNameResolverTest {

    private static final String APP_NAME = "resolver-notification-test";

    private ApplicationConfigCache cache;

    private ShenyuNameResolver resolver;

    private NameResolver.Listener2 listener;

    @BeforeEach
    @SuppressWarnings("unchecked")
    public void setUp() {
        cache = ApplicationConfigCache.getInstance();
        cache.invalidate(APP_NAME);
        NameResolver.Args args = mock(NameResolver.Args.class);
        when(args.getSynchronizationContext()).thenReturn(new SynchronizationContext((thread, error) -> {
            throw new AssertionError(error);
        }));
        SharedResourceHolder.Resource<Executor> resource = mock(SharedResourceHolder.Resource.class);
        when(resource.create()).thenReturn(Runnable::run);
        resolver = new ShenyuNameResolver(APP_NAME, args, resource);
        listener = mock(NameResolver.Listener2.class);
    }

    @AfterEach
    public void tearDown() {
        resolver.shutdown();
        cache.invalidate(APP_NAME);
    }

    @Test
    public void testRepeatedUnchangedNotificationsDoNotResolveAgain() {
        updateUpstream("127.0.0.1:8080");
        resolver.start(listener);

        updateUpstream("127.0.0.1:8080");
        updateUpstream("127.0.0.1:8080");
        resolver.refresh();

        verify(listener, times(1)).onResult(any(NameResolver.ResolutionResult.class));
        verify(listener, never()).onError(any(Status.class));
    }

    @Test
    public void testChangedUpstreamResolvesAfterUnchangedNotification() {
        updateUpstream("127.0.0.1:8080");
        resolver.start(listener);

        updateUpstream("127.0.0.1:8080");
        updateUpstream("127.0.0.1:8081");
        updateUpstream("127.0.0.1:8081");
        updateUpstream("127.0.0.1:8081");

        ArgumentCaptor<NameResolver.ResolutionResult> results = ArgumentCaptor.forClass(NameResolver.ResolutionResult.class);
        verify(listener, times(2)).onResult(results.capture());
        assertEquals(new InetSocketAddress("127.0.0.1", 8081), results.getValue().getAddresses().get(0).getAddresses().get(0));
        verify(listener, never()).onError(any(Status.class));
    }

    @Test
    public void testEmptyUpstreamsReportErrorAndCanRecover() {
        updateUpstream("127.0.0.1:8080");
        resolver.start(listener);
        updateUpstream("127.0.0.1:8080");

        cache.invalidate(APP_NAME);
        resolver.refresh();
        updateUpstream("127.0.0.1:8080");
        resolver.refresh();

        verify(listener, times(2)).onResult(any(NameResolver.ResolutionResult.class));
        ArgumentCaptor<Status> error = ArgumentCaptor.forClass(Status.class);
        verify(listener).onError(error.capture());
        assertEquals(Status.Code.UNAVAILABLE, error.getValue().getCode());
        assertEquals("No servers found for " + APP_NAME, error.getValue().getDescription());
    }

    @Test
    public void testInvalidUpstreamReportsErrorAndCanRecover() {
        updateUpstream("127.0.0.1:8080");
        resolver.start(listener);

        updateUpstream("127.0.0.1:invalid");
        updateUpstream("127.0.0.1:8080");

        verify(listener, times(2)).onResult(any(NameResolver.ResolutionResult.class));
        ArgumentCaptor<Status> error = ArgumentCaptor.forClass(Status.class);
        verify(listener).onError(error.capture());
        assertEquals(Status.Code.UNAVAILABLE, error.getValue().getCode());
        assertEquals("Failed to update server list for " + APP_NAME, error.getValue().getDescription());
    }

    private void updateUpstream(final String url) {
        cache.handlerUpstream(APP_NAME, Collections.singletonList(GrpcUpstream.builder()
                .upstreamUrl(url).weight(50).status(true).build()));
    }

    @Test
    public void testReorderedUpstreamsDoNotPublishAnotherResult() {
        cache.handlerUpstream(APP_NAME, upstreams(3));
        resolver.start(listener);
        List<GrpcUpstream> reordered = upstreams(3);
        Collections.reverse(reordered);
        cache.handlerUpstream(APP_NAME, reordered);
        resolver.refresh();

        verify(listener, times(1)).onResult(any(NameResolver.ResolutionResult.class));
        verify(listener, never()).onError(any(Status.class));
    }

    @Test
    public void testSameSizeReplacementPreservesPublishedOrder() {
        cache.handlerUpstream(APP_NAME, upstreams(3));
        resolver.start(listener);
        List<GrpcUpstream> changed = upstreams(3);
        changed.set(1, GrpcUpstream.builder().upstreamUrl("127.0.0.1:9000").weight(50).status(true).build());
        cache.handlerUpstream(APP_NAME, changed);

        ArgumentCaptor<NameResolver.ResolutionResult> results = ArgumentCaptor.forClass(NameResolver.ResolutionResult.class);
        verify(listener, times(2)).onResult(results.capture());
        assertEquals(3, results.getValue().getAddresses().size());
        assertEquals(new InetSocketAddress("127.0.0.1", 8080), results.getValue().getAddresses().get(0).getAddresses().get(0));
        assertEquals(new InetSocketAddress("127.0.0.1", 9000), results.getValue().getAddresses().get(1).getAddresses().get(0));
        assertEquals(new InetSocketAddress("127.0.0.1", 8082), results.getValue().getAddresses().get(2).getAddresses().get(0));
        verify(listener, never()).onError(any(Status.class));
    }

    @Test
    public void testAddedAndRemovedUpstreamsPublishResults() {
        cache.handlerUpstream(APP_NAME, upstreams(2));
        resolver.start(listener);
        cache.handlerUpstream(APP_NAME, upstreams(3));
        cache.handlerUpstream(APP_NAME, upstreams(1));

        ArgumentCaptor<NameResolver.ResolutionResult> results = ArgumentCaptor.forClass(NameResolver.ResolutionResult.class);
        verify(listener, times(3)).onResult(results.capture());
        assertEquals(3, results.getAllValues().get(1).getAddresses().size());
        assertEquals(1, results.getAllValues().get(2).getAddresses().size());
        verify(listener, never()).onError(any(Status.class));
    }

    @Test
    public void testDuplicateUpstreamsKeepExistingMembershipSemantics() {
        List<GrpcUpstream> initial = upstreams(2);
        cache.handlerUpstream(APP_NAME, Arrays.asList(initial.get(0), initial.get(0), initial.get(1)));
        resolver.start(listener);
        List<GrpcUpstream> updated = upstreams(2);
        cache.handlerUpstream(APP_NAME, Arrays.asList(updated.get(0), updated.get(1), updated.get(1)));

        verify(listener, times(1)).onResult(any(NameResolver.ResolutionResult.class));
        verify(listener, never()).onError(any(Status.class));
    }

    @Test
    public void testLargeUnchangedUpstreamListDoesNotPublishAnotherResult() {
        cache.handlerUpstream(APP_NAME, upstreams(1024));
        resolver.start(listener);
        cache.handlerUpstream(APP_NAME, upstreams(1024));

        verify(listener, times(1)).onResult(any(NameResolver.ResolutionResult.class));
        verify(listener, never()).onError(any(Status.class));
    }

    @Test
    public void testNamespaceChangePublishesAnotherResult() {
        cache.handlerUpstream(APP_NAME, upstreams(2));
        resolver.start(listener);
        List<GrpcUpstream> changed = upstreams(2);
        changed.get(1).setNamespaceId("another-namespace");
        cache.handlerUpstream(APP_NAME, changed);

        verify(listener, times(2)).onResult(any(NameResolver.ResolutionResult.class));
        verify(listener, never()).onError(any(Status.class));
    }

    private List<GrpcUpstream> upstreams(final int size) {
        List<GrpcUpstream> result = new ArrayList<>();
        for (int i = 0; i < size; i++) {
            result.add(GrpcUpstream.builder().upstreamUrl("127.0.0.1:" + (8080 + i)).weight(50).status(true).build());
        }
        return result;
    }
}
