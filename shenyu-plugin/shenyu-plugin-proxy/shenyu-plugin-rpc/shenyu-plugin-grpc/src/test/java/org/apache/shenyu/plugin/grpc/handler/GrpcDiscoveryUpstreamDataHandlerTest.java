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

package org.apache.shenyu.plugin.grpc.handler;

import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.dto.convert.selector.GrpcUpstream;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamKey;
import org.apache.shenyu.plugin.grpc.cache.ApplicationConfigCache;
import org.apache.shenyu.plugin.grpc.cache.GrpcClientCache;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

public final class GrpcDiscoveryUpstreamDataHandlerTest {

    private static final String SELECTOR_ID = "selector-id";

    @AfterEach
    public void tearDown() {
        ApplicationConfigCache.getInstance().invalidate(SELECTOR_ID);
    }

    @Test
    public void testHandlerDiscoveryUpstreamData() {
        DiscoveryUpstreamData discoveryUpstreamData = DiscoveryUpstreamData.builder()
                .protocol("grpc://")
                .url("127.0.0.1:9090")
                .weight(100)
                .status(0)
                .props("{\"healthCheckEnabled\":\"false\"}")
                .build();
        DiscoverySyncData discoverySyncData = new DiscoverySyncData();
        discoverySyncData.setSelectorId(SELECTOR_ID);
        discoverySyncData.setUpstreamDataList(Collections.singletonList(discoveryUpstreamData));

        new GrpcDiscoveryUpstreamDataHandler().handlerDiscoveryUpstreamData(discoverySyncData);

        List<GrpcUpstream> upstreamList = ApplicationConfigCache.getInstance().getGrpcUpstreamListCache(SELECTOR_ID);
        assertEquals(1, upstreamList.size());
        assertEquals("grpc://", upstreamList.get(0).getProtocol());
        assertFalse(upstreamList.get(0).isHealthCheckEnabled());
        assertNotNull(GrpcClientCache.getGrpcClient(SELECTOR_ID));
    }

    @Test
    public void testRemoveDiscoveryUpstreamData() {
        GrpcClientCache.initGrpcClient(SELECTOR_ID);
        assertNotNull(GrpcClientCache.getGrpcClient(SELECTOR_ID));
        new GrpcDiscoveryUpstreamDataHandler().removeDiscoveryUpstreamData(new DiscoveryUpstreamKey("grpc", SELECTOR_ID, null));

        assertNull(GrpcClientCache.getGrpcClient(SELECTOR_ID));
    }
}
