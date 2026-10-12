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

import org.apache.shenyu.sync.data.api.DiscoveryUpstreamKey;
import org.apache.shenyu.plugin.grpc.cache.GrpcClientCache;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

public final class GrpcDiscoveryUpstreamDataHandlerTest {

    private static final String SELECTOR_ID = "selector-id";

    @AfterEach
    public void tearDown() {
        GrpcClientCache.removeClient(SELECTOR_ID);
    }

    @Test
    public void testRemoveDiscoveryUpstreamData() {
        GrpcClientCache.initGrpcClient(SELECTOR_ID);
        assertNotNull(GrpcClientCache.getGrpcClient(SELECTOR_ID));
        new GrpcDiscoveryUpstreamDataHandler().removeDiscoveryUpstreamData(new DiscoveryUpstreamKey("grpc", SELECTOR_ID, null));

        assertNull(GrpcClientCache.getGrpcClient(SELECTOR_ID));
    }
}
