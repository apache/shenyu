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

package org.apache.shenyu.plugin.divide.handler;

import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.UpstreamCheckUtils;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.sql.Timestamp;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class DivideUpstreamDataHandlerTest {

    private DiscoverySyncData discoverySyncData;

    private DivideUpstreamDataHandler divideUpstreamDataHandler;

    private MockedStatic<UpstreamCheckUtils> mockCheckUtils;

    @BeforeEach
    public void setUp() {
        this.divideUpstreamDataHandler = new DivideUpstreamDataHandler();
        List<DiscoveryUpstreamData> divideUpstreamList = Stream.of(3)
                .map(weight -> DiscoveryUpstreamData.builder()
                        .url("mock-" + weight)
                        .dateUpdated(new Timestamp(System.currentTimeMillis()))
                        .build())
                .collect(Collectors.toList());
        this.discoverySyncData = mock(DiscoverySyncData.class);
        when(discoverySyncData.getSelectorId()).thenReturn("handler");
        when(discoverySyncData.getUpstreamDataList()).thenReturn(divideUpstreamList);

        // mock static
        mockCheckUtils = mockStatic(UpstreamCheckUtils.class);
        mockCheckUtils.when(() -> UpstreamCheckUtils.checkUrl(anyString(), anyInt())).thenReturn(true);
    }

    @AfterEach
    public void tearDown() {
        mockCheckUtils.close();
    }

    /**
     * Handler selector test.
     */
    @Test
    public void handlerDiscoveryUpstreamDataTest() {
        divideUpstreamDataHandler.handlerDiscoveryUpstreamData(discoverySyncData);
        List<Upstream> result = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId("handler");
        assertEquals(discoverySyncData.getUpstreamDataList().get(0).getUrl(), result.get(0).getUrl());
        DiscoverySyncData discoverySyncData = new DiscoverySyncData();
        discoverySyncData.setSelectorId(null);
        divideUpstreamDataHandler.handlerDiscoveryUpstreamData(discoverySyncData);
    }

    /**
     * Plugin named test.
     */
    @Test
    public void pluginNamedTest() {
        assertEquals(divideUpstreamDataHandler.pluginName(), PluginEnum.DIVIDE.getName());
    }
}
