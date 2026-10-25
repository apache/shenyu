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

package org.apache.shenyu.sync.data.http.refresh;

import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.List;

import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

public final class DiscoveryUpstreamDataRefreshTest {

    @Test
    public void testRefreshWithEmptyData() {
        DiscoveryUpstreamDataSubscriber subscriber = mock(DiscoveryUpstreamDataSubscriber.class);
        DiscoveryUpstreamDataRefresh dataRefresh = new DiscoveryUpstreamDataRefresh(Collections.singletonList(subscriber));
        DiscoverySyncData discoverySyncData = discoverySyncData("selector-id", "selector-name");

        dataRefresh.refresh(Collections.singletonList(discoverySyncData));
        dataRefresh.refresh(Collections.emptyList());
        dataRefresh.refresh(Collections.emptyList());

        verify(subscriber).onSubscribe(discoverySyncData);
        verify(subscriber, times(2)).refresh();
        verify(subscriber, times(1)).unSubscribe(argThat(item -> "selector-id".equals(item.selectorId())
                && "selector-name".equals(item.selectorName())));
    }

    @Test
    public void testRefreshRemovesOnlyMissingSelector() {
        DiscoveryUpstreamDataSubscriber subscriber = mock(DiscoveryUpstreamDataSubscriber.class);
        DiscoveryUpstreamDataRefresh dataRefresh = new DiscoveryUpstreamDataRefresh(Collections.singletonList(subscriber));
        DiscoverySyncData removed = discoverySyncData("removed", "removed-name");
        DiscoverySyncData retained = discoverySyncData("retained", "retained-name");
        dataRefresh.refresh(List.of(removed, retained));

        dataRefresh.refresh(Collections.singletonList(retained));

        verify(subscriber).onSubscribe(removed);
        verify(subscriber, times(2)).onSubscribe(retained);
        verify(subscriber).unSubscribe(argThat(item -> "removed".equals(item.selectorId())));
        verify(subscriber, never()).unSubscribe(argThat(item -> "retained".equals(item.selectorId())));
    }

    @Test
    public void testRefreshRemovesOldSelectorNameBeforeReplacement() {
        DiscoveryUpstreamDataSubscriber subscriber = mock(DiscoveryUpstreamDataSubscriber.class);
        DiscoveryUpstreamDataRefresh dataRefresh = new DiscoveryUpstreamDataRefresh(Collections.singletonList(subscriber));
        DiscoverySyncData previous = discoverySyncData("selector-id", "old-name");
        DiscoverySyncData replacement = discoverySyncData("selector-id", "new-name");
        dataRefresh.refresh(Collections.singletonList(previous));

        dataRefresh.refresh(Collections.singletonList(replacement));

        verify(subscriber).unSubscribe(argThat(item -> "old-name".equals(item.selectorName())));
        verify(subscriber).onSubscribe(replacement);
    }

    private static DiscoverySyncData discoverySyncData(final String selectorId, final String selectorName) {
        DiscoverySyncData data = new DiscoverySyncData();
        data.setNamespaceId("default");
        data.setPluginName("tcp");
        data.setSelectorId(selectorId);
        data.setSelectorName(selectorName);
        return data;
    }
}
