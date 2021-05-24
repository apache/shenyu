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

package org.apache.shenyu.plugin.grpc.cache;

import org.apache.shenyu.common.dto.SelectorData;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The Test Case For {@link ApplicationConfigCache}.
 */
@RunWith(MockitoJUnitRunner.class)
public class ApplicationConfigCacheTest {

    private ApplicationConfigCache applicationConfigCache;

    private SelectorData selector;

    @Before
    public void setUp() {
        applicationConfigCache = ApplicationConfigCache.getInstance();
        selector = mock(SelectorData.class);
        when(selector.getName()).thenReturn("/grpc");
        when(selector.getHandle()).thenReturn("[{\"upstreamUrl\":\"localhost:8080\",\"weight\":50,\"status\":true}]");
    }

    @Test
    public void getInstance() {
        assertNotNull(this.applicationConfigCache);
    }

    @Test
    public void testInitPrx() {
        this.applicationConfigCache.initPrx(selector);
        assertNotNull(applicationConfigCache.get(selector.getName()));
    }

    @Test
    public void testGet() {
        assertNotNull(this.applicationConfigCache.get("/test"));
    }

    @Test
    public void testInvalidate() {
        this.applicationConfigCache.invalidate(selector.getName());
        assert this.applicationConfigCache.get(selector.getName()).getShenyuServiceInstances().size() == 0;
    }

    @Test
    public void testWatch() {
        this.applicationConfigCache.watch(selector.getName(), Assert::assertNotNull);
    }
}
