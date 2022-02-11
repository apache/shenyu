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

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.grpc.resolver.ShenyuServiceInstance;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The Test Case For {@link ApplicationConfigCache}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ApplicationConfigCacheTest {
    
    private ApplicationConfigCache applicationConfigCache;
    
    private SelectorData selector;
    
    @BeforeEach
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
        final List<ShenyuServiceInstance> shenyuServiceInstances = this.applicationConfigCache.get(selector.getName()).getShenyuServiceInstances();
        assertTrue(CollectionUtils.isEmpty(shenyuServiceInstances), "shenyuServiceInstances mast is empty");
    }
    
    @Test
    public void testWatch() {
        this.applicationConfigCache.watch(selector.getName(), Assertions::assertNotNull);
    }
}
