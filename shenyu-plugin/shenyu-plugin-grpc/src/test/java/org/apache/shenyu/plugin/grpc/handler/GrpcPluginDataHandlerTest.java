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

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.grpc.cache.ApplicationConfigCache;
import org.apache.shenyu.plugin.grpc.cache.GrpcClientCache;
import org.apache.shenyu.plugin.grpc.resolver.ShenyuServiceInstance;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The Test Case For {@link GrpcPluginDataHandler}.
 */
public class GrpcPluginDataHandlerTest {
    
    private SelectorData selectorData;
    
    private GrpcPluginDataHandler grpcPluginDataHandler;
    
    @BeforeEach
    public void setUp() {
        this.grpcPluginDataHandler = new GrpcPluginDataHandler();
        List<DivideUpstream> divideUpstreamList = Stream.of(3)
                .map(weight -> DivideUpstream.builder()
                        .upstreamUrl("localhost:8080" + weight)
                        .build())
                .collect(Collectors.toList());
        this.selectorData = mock(SelectorData.class);
        
        when(selectorData.getHandle()).thenReturn(GsonUtils.getGson().toJson(divideUpstreamList));
    }
    
    @Test
    public void testHandlerSelector() {
        when(selectorData.getName()).thenReturn(null);
        grpcPluginDataHandler.handlerSelector(selectorData);
        when(selectorData.getName()).thenReturn("/grpc");
        grpcPluginDataHandler.handlerSelector(selectorData);
        assertNotNull(GrpcClientCache.getGrpcClient(selectorData.getName()));
    }
    
    @Test
    public void testRemoveSelector() {
        when(selectorData.getName()).thenReturn(null);
        grpcPluginDataHandler.removeSelector(selectorData);
        when(selectorData.getName()).thenReturn("/grpc");
        grpcPluginDataHandler.removeSelector(selectorData);
        final List<ShenyuServiceInstance> shenyuServiceInstances = ApplicationConfigCache.getInstance().get(selectorData.getName()).getShenyuServiceInstances();
        assertTrue(CollectionUtils.isEmpty(shenyuServiceInstances), "shenyuServiceInstances mast is empty");
    }
    
    @Test
    public void testPpluginNamed() {
        assertEquals(grpcPluginDataHandler.pluginNamed(), PluginEnum.GRPC.getName());
    }
}
