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

package org.apache.shenyu.plugin.tcp.handler;

import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamKey;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.protocol.tcp.BootstrapServer;
import org.apache.shenyu.protocol.tcp.UpstreamProvider;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.clearInvocations;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * Test cases for {@link TcpUpstreamDataHandler}.
 */
public final class TcpUpstreamDataHandlerTest {

    private static final String SELECTOR = "tcp-upstream-selector";

    private static final String SELECTOR_ID = "tcp-upstream-selector-id";

    private final TcpBootstrapFactory factory = TcpBootstrapFactory.getSingleton();

    private final TcpUpstreamDataHandler dataHandler = new TcpUpstreamDataHandler();

    @BeforeEach
    public void setUp() {
        factory.clearCache();
        // TcpBootstrapFactory initializes an empty entry before the first discovery upstream update.
        UpstreamProvider.getSingleton().createUpstreams(SELECTOR, Collections.emptyList());
    }

    @AfterEach
    public void tearDown() {
        factory.clearCache();
    }

    @Test
    public void pluginNameShouldReturnTcp() {
        assertEquals("tcp", dataHandler.pluginName());
    }

    @Test
    public void handlerShouldRemoveUpstreamsMissingFromTheNewSnapshot() {
        DiscoveryUpstreamData kept = upstream("127.0.0.1:10001");
        DiscoveryUpstreamData removed = upstream("127.0.0.1:10002");
        DiscoveryUpstreamData added = upstream("127.0.0.1:10003");
        UpstreamProvider.getSingleton().createUpstreams(SELECTOR, Arrays.asList(kept, removed));
        BootstrapServer bootstrapServer = mock(BootstrapServer.class);
        factory.cache(SELECTOR, bootstrapServer);

        dataHandler.handlerDiscoveryUpstreamData(
                syncData(kept, added));

        @SuppressWarnings("unchecked")
        ArgumentCaptor<List<DiscoveryUpstreamData>> captor = ArgumentCaptor.forClass(List.class);
        verify(bootstrapServer).removeCommonUpstream(captor.capture());
        List<DiscoveryUpstreamData> removeList = captor.getValue();
        assertEquals(1, removeList.size());
        assertEquals("127.0.0.1:10002", removeList.get(0).getUrl());
        assertEquals(Arrays.asList("127.0.0.1:10001", "127.0.0.1:10003"),
                UpstreamProvider.getSingleton().provide(SELECTOR).stream()
                        .map(DiscoveryUpstreamData::getUrl)
                        .collect(Collectors.toList()));
    }

    @Test
    public void handlerShouldStillRefreshCacheWhenServerIsAbsent() {
        DiscoveryUpstreamData kept = upstream("127.0.0.1:10001");
        DiscoveryUpstreamData added = upstream("127.0.0.1:10003");
        UpstreamProvider.getSingleton().createUpstreams(SELECTOR, Collections.singletonList(kept));

        assertDoesNotThrow(() -> dataHandler.handlerDiscoveryUpstreamData(syncData(kept, added)));

        List<String> urls = UpstreamProvider.getSingleton().provide(SELECTOR).stream()
                .map(DiscoveryUpstreamData::getUrl)
                .collect(Collectors.toList());
        assertTrue(urls.contains("127.0.0.1:10003"));
    }

    @Test
    public void removeShouldResolveSelectorNameByIdAndEvictUpstreams() {
        DiscoveryUpstreamData upstream = upstream("127.0.0.1:10001");
        BootstrapServer bootstrapServer = mock(BootstrapServer.class);
        factory.cache(SELECTOR, bootstrapServer);
        dataHandler.handlerDiscoveryUpstreamData(syncData(upstream));
        assertTrue(UpstreamProvider.getSingleton().inCache(SELECTOR));
        assertEquals(SELECTOR, UpstreamProvider.getSingleton().getSelectorName(SELECTOR_ID));
        DiscoveryUpstreamKey deleteData = new DiscoveryUpstreamKey("tcp", SELECTOR_ID, SELECTOR);

        dataHandler.removeDiscoveryUpstreamData(deleteData);

        assertTrue(UpstreamProvider.getSingleton().provide(SELECTOR).isEmpty());
        assertFalse(UpstreamProvider.getSingleton().inCache(SELECTOR));
        assertNull(UpstreamProvider.getSingleton().getSelectorName(SELECTOR_ID));
        verify(bootstrapServer).removeCommonUpstream(Collections.singletonList(upstream));
    }

    @Test
    public void removeShouldPreferLocalSelectorNameOverEventName() {
        DiscoveryUpstreamData upstream = upstream("127.0.0.1:10001");
        BootstrapServer bootstrapServer = mock(BootstrapServer.class);
        BootstrapServer otherServer = mock(BootstrapServer.class);
        factory.cache(SELECTOR, bootstrapServer);
        factory.cache("new-name", otherServer);
        UpstreamProvider.getSingleton().createUpstreams("new-name", Collections.singletonList(upstream));
        dataHandler.handlerDiscoveryUpstreamData(syncData(upstream));

        dataHandler.removeDiscoveryUpstreamData(new DiscoveryUpstreamKey("tcp", SELECTOR_ID, "new-name"));

        assertFalse(UpstreamProvider.getSingleton().inCache(SELECTOR));
        assertEquals(Collections.singletonList(upstream), UpstreamProvider.getSingleton().provide("new-name"));
        verify(bootstrapServer).removeCommonUpstream(Collections.singletonList(upstream));
        verifyNoInteractions(otherServer);
    }

    @Test
    public void removeShouldFallBackToEventNameWithoutLocalMapping() {
        DiscoveryUpstreamData upstream = upstream("127.0.0.1:10001");
        BootstrapServer bootstrapServer = mock(BootstrapServer.class);
        factory.cache(SELECTOR, bootstrapServer);
        UpstreamProvider.getSingleton().createUpstreams(SELECTOR, Collections.singletonList(upstream));

        dataHandler.removeDiscoveryUpstreamData(new DiscoveryUpstreamKey("tcp", "unknown", SELECTOR));

        assertFalse(UpstreamProvider.getSingleton().inCache(SELECTOR));
        verify(bootstrapServer).removeCommonUpstream(Collections.singletonList(upstream));
    }

    @Test
    public void removeShouldIgnoreUnknownSelectorId() {
        DiscoveryUpstreamData upstream = upstream("127.0.0.1:10001");
        BootstrapServer bootstrapServer = mock(BootstrapServer.class);
        factory.cache(SELECTOR, bootstrapServer);
        dataHandler.handlerDiscoveryUpstreamData(syncData(upstream));
        clearInvocations(bootstrapServer);
        DiscoveryUpstreamKey deleteData = new DiscoveryUpstreamKey("tcp", "unknown", null);

        dataHandler.removeDiscoveryUpstreamData(deleteData);

        assertEquals(Collections.singletonList(upstream), UpstreamProvider.getSingleton().provide(SELECTOR));
        assertEquals(SELECTOR, UpstreamProvider.getSingleton().getSelectorName(SELECTOR_ID));
        verifyNoInteractions(bootstrapServer);
    }

    @Test
    public void removeShouldIgnoreMissingSelectorIdentity() {
        assertDoesNotThrow(() -> dataHandler.removeDiscoveryUpstreamData(new DiscoveryUpstreamKey("tcp", null, null)));
    }

    private DiscoverySyncData syncData(final DiscoveryUpstreamData... upstreams) {
        DiscoverySyncData discoverySyncData = new DiscoverySyncData();
        discoverySyncData.setSelectorId(SELECTOR_ID);
        discoverySyncData.setSelectorName(SELECTOR);
        discoverySyncData.setUpstreamDataList(Arrays.asList(upstreams));
        return discoverySyncData;
    }

    private DiscoveryUpstreamData upstream(final String url) {
        return DiscoveryUpstreamData.builder().url(url).status(0).weight(1).build();
    }
}
