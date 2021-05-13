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
import org.apache.shenyu.plugin.divide.cache.UpstreamCacheManager;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The type divide plugin data handler test.
 */
public final class DividePluginDataHandlerTest {

    private SelectorData selectorData;

    private List<DivideUpstream> divideUpstreamList;

    private DividePluginDataHandler dividePluginDataHandler;

    @Before
    public void setUp() {
        this.dividePluginDataHandler = new DividePluginDataHandler();
        this.divideUpstreamList = Stream.of(3)
                .map(weight -> DivideUpstream.builder()
                        .upstreamUrl("mock-" + weight)
                        .build())
                .collect(Collectors.toList());
        this.selectorData = mock(SelectorData.class);
        when(selectorData.getId()).thenReturn("handler");
        when(selectorData.getHandle()).thenReturn(GsonUtils.getGson().toJson(divideUpstreamList));
    }

    /**
     * Handler selector test.
     */
    @Test
    public void handlerSelectorTest() {
        dividePluginDataHandler.handlerSelector(selectorData);
        List<DivideUpstream> result = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId("handler");
        Assert.assertEquals(GsonUtils.getGson().toJson(divideUpstreamList), GsonUtils.getGson().toJson(result));
    }

    /**
     * Remove selector test.
     */
    @Test
    public void removeSelectorTest() {
        UpstreamCacheManager.getInstance().submit(selectorData);
        dividePluginDataHandler.removeSelector(selectorData);
        List<DivideUpstream> result = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId("handler");
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
