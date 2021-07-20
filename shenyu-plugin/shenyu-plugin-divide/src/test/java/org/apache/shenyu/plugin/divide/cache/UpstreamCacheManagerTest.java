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

package org.apache.shenyu.plugin.divide.cache;

import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.DivideUpstream;
import org.apache.shenyu.common.utils.CollectionUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.UpstreamCheckUtils;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.MockedStatic;
import org.mockito.junit.MockitoJUnitRunner;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * The type upstream cache manager test.
 */
@RunWith(MockitoJUnitRunner.class)
public final class UpstreamCacheManagerTest {

    private List<DivideUpstream> loadBalances;

    private SelectorData selectorData;

    private MockedStatic<UpstreamCheckUtils> mockCheckUtils;

    @Before
    public void setUp() {
        this.loadBalances = Stream.of(3, 4, 5)
                .map(weight -> DivideUpstream.builder()
                        .upstreamUrl("divide-upstream-" + weight)
                        .build())
                .collect(Collectors.toList());
        selectorData = mock(SelectorData.class);
        when(selectorData.getId()).thenReturn("mock");
        when(selectorData.getHandle()).thenReturn(GsonUtils.getGson().toJson(loadBalances));

        // mock static
        mockCheckUtils = mockStatic(UpstreamCheckUtils.class);
        mockCheckUtils.when(() -> UpstreamCheckUtils.checkUrl(anyString(), anyInt())).thenReturn(true);
    }

    @After
    public void tearDown() {
        mockCheckUtils.close();
    }

    /**
     * Find upstream list test.
     */
    @Test
    public void findUpstreamListTest() {
        UpstreamCacheManager.getInstance().submit(selectorData);
        List<DivideUpstream> result = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId("mock");
        Assert.assertEquals(loadBalances, result);
    }

    /**
     * Remove by key test.
     */
    @Test
    public void removeByKeyTest() {
        UpstreamCacheManager.getInstance().submit(selectorData);
        UpstreamCacheManager.getInstance().removeByKey("mock");
        List<DivideUpstream> result = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId("mock");
        Assert.assertTrue(CollectionUtils.isEmpty(result));
    }

    /**
     * Submit test.
     */
    @Test
    public void submit() {
        List<DivideUpstream> upstreams = Stream.of(5)
                .map(weight -> DivideUpstream.builder()
                        .upstreamUrl("mock" + weight)
                        .build())
                .collect(Collectors.toList());
        SelectorData selectorData = mock(SelectorData.class);
        when(selectorData.getId()).thenReturn("submit");
        when(selectorData.getHandle()).thenReturn(GsonUtils.getGson().toJson(upstreams));
        UpstreamCacheManager.getInstance().submit(selectorData);
        List<DivideUpstream> result = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId("submit");
        Assert.assertEquals(upstreams.get(0), result.get(0));
    }
}
