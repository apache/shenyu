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

package org.apache.shenyu.plugin.apache.dubbo.cache;

import org.apache.dubbo.common.URL;
import org.apache.dubbo.config.ReferenceConfig;
import org.apache.dubbo.rpc.Invoker;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.ReflectUtils;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;


/**
 * The Test Case For DubboProviderVersionCache.
 */
@RunWith(MockitoJUnitRunner.class)
public final class DubboProviderVersionCacheTest {

    private DubboProviderVersionCache dubboProviderVersionCache;

    @Before
    public void setUp() {
        dubboProviderVersionCache = DubboProviderVersionCache.getInstance();
    }

    @Test
    public void getInstance() {
        assertNotNull(this.dubboProviderVersionCache);
    }

    @Test
    public void testGet() {
        assertNotNull(this.dubboProviderVersionCache.get("/test"));
    }

    @Test
    public void testCacheProviderVersion() {
        final String version = "2.7.3";
        final String path = "/test";

        final DubboProviderVersionCache dubboProviderVersionCache = spy(DubboProviderVersionCache.getInstance());
        ReferenceConfig referenceConfig = mock(ReferenceConfig.class);
        Invoker invoker = mock(Invoker.class);
        URL url = mock(URL.class);
        mockStatic(ReflectUtils.class);
        when(ReflectUtils.getFieldValue(referenceConfig, Constants.DUBBO_REFRENCE_INVOKER)).thenReturn(invoker);
        when(invoker.getUrl()).thenReturn(url);
        when(url.getParameter(anyString())).thenReturn(version);

        dubboProviderVersionCache.cacheProviderVersion(path, referenceConfig);
        assertEquals(version, dubboProviderVersionCache.get(path));

        final String nullVersion = null;
        when(url.getParameter(anyString())).thenReturn(nullVersion);
        dubboProviderVersionCache.cacheProviderVersion(path, referenceConfig);
        assertEquals("", dubboProviderVersionCache.get(path));
        dubboProviderVersionCache.invalidateAll();
    }

    @Test
    public void testInvalidate() {
        this.dubboProviderVersionCache.invalidate("/test");
        this.dubboProviderVersionCache.invalidateAll();
    }
}
