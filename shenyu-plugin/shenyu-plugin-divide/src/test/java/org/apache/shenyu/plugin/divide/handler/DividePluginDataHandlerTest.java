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

import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.DivideUpstream;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.UpstreamCheckUtils;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.MockedStatic;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * The type divide plugin data handler test.
 */
public final class DividePluginDataHandlerTest {

    private SelectorData selectorData;

    private DividePluginDataHandler dividePluginDataHandler;

    private MockedStatic<UpstreamCheckUtils> mockCheckUtils;

    @Before
    public void setUp() {
        this.dividePluginDataHandler = new DividePluginDataHandler();
        List<DivideUpstream> divideUpstreamList = Stream.of(3)
                .map(weight -> DivideUpstream.builder()
                        .upstreamUrl("mock-" + weight)
                        .build())
                .collect(Collectors.toList());
        this.selectorData = mock(SelectorData.class);
        when(selectorData.getId()).thenReturn("handler");
        when(selectorData.getHandle()).thenReturn(GsonUtils.getGson().toJson(divideUpstreamList));

        // mock static
        mockCheckUtils = mockStatic(UpstreamCheckUtils.class);
        mockCheckUtils.when(() -> UpstreamCheckUtils.checkUrl(anyString(), anyInt())).thenReturn(true);
    }

    @After
    public void tearDown() {
        mockCheckUtils.close();
    }

    /**
     * Handler selector test.
     */
    @Test
    public void handlerSelectorTest() {
        dividePluginDataHandler.handlerSelector(selectorData);
        List<Upstream> result = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId("handler");
        Assert.assertEquals(GsonUtils.getInstance().fromList(selectorData.getHandle(), DivideUpstream.class).get(0).getUpstreamUrl(), result.get(0).getUrl());
    }

    /**
     * Remove selector test.
     */
    @Test
    public void removeSelectorTest() {
        dividePluginDataHandler.handlerSelector(selectorData);
        dividePluginDataHandler.removeSelector(selectorData);
        List<Upstream> result = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId("handler");
        Assert.assertNull(result);
    }

    /**
     * Plugin named test.
     */
    @Test
    public void pluginNamedTest() {
        Assert.assertEquals(dividePluginDataHandler.pluginNamed(), PluginEnum.DIVIDE.getName());
    }
}
