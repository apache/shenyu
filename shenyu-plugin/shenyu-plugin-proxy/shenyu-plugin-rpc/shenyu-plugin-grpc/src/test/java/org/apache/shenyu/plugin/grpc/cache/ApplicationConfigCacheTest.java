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

    @Test
    public void testObtainGrpcExtInfo() {
        org.apache.shenyu.common.dto.MetaData metaData = org.apache.shenyu.common.dto.MetaData.builder()
                .path("/grpc/echo")
                .serviceName("echoService")
                .methodName("echo")
                .rpcExt("{\"methodType\":\"UNARY\",\"timeout\":2500}")
                .build();

        GrpcExtInfo info1 = this.applicationConfigCache.obtainGrpcExtInfo(metaData);
        org.junit.jupiter.api.Assertions.assertEquals(2500, info1.getTimeout());
        org.junit.jupiter.api.Assertions.assertEquals(io.grpc.MethodDescriptor.MethodType.UNARY, info1.getMethodType());

        GrpcExtInfo info2 = this.applicationConfigCache.obtainGrpcExtInfo(metaData);
        org.junit.jupiter.api.Assertions.assertSame(info1, info2);
    }

    @Test
    public void testObtainGrpcExtInfoDefaults() {
        org.junit.jupiter.api.Assertions.assertNotNull(this.applicationConfigCache.obtainGrpcExtInfo(null));

        org.apache.shenyu.common.dto.MetaData blankMeta = org.apache.shenyu.common.dto.MetaData.builder()
                .path("/grpc/blank")
                .rpcExt("")
                .build();
        GrpcExtInfo blankInfo = this.applicationConfigCache.obtainGrpcExtInfo(blankMeta);
        org.junit.jupiter.api.Assertions.assertEquals(5000, blankInfo.getTimeout());
        org.junit.jupiter.api.Assertions.assertEquals(io.grpc.MethodDescriptor.MethodType.UNARY, blankInfo.getMethodType());

        org.apache.shenyu.common.dto.MetaData emptyJsonMeta = org.apache.shenyu.common.dto.MetaData.builder()
                .path("/grpc/emptyJson")
                .rpcExt("{}")
                .build();
        GrpcExtInfo emptyJsonInfo = this.applicationConfigCache.obtainGrpcExtInfo(emptyJsonMeta);
        org.junit.jupiter.api.Assertions.assertEquals(5000, emptyJsonInfo.getTimeout());
        org.junit.jupiter.api.Assertions.assertEquals(io.grpc.MethodDescriptor.MethodType.UNARY, emptyJsonInfo.getMethodType());
    }

    @Test
    public void testObtainGrpcExtInfoUpdate() {
        org.apache.shenyu.common.dto.MetaData metaData = org.apache.shenyu.common.dto.MetaData.builder()
                .path("/grpc/update")
                .rpcExt("{\"methodType\":\"UNARY\",\"timeout\":1000}")
                .build();

        GrpcExtInfo initial = this.applicationConfigCache.obtainGrpcExtInfo(metaData);
        org.junit.jupiter.api.Assertions.assertEquals(1000, initial.getTimeout());

        metaData.setRpcExt("{\"methodType\":\"SERVER_STREAMING\",\"timeout\":4000}");
        GrpcExtInfo updated = this.applicationConfigCache.obtainGrpcExtInfo(metaData);
        org.junit.jupiter.api.Assertions.assertEquals(4000, updated.getTimeout());
        org.junit.jupiter.api.Assertions.assertEquals(io.grpc.MethodDescriptor.MethodType.SERVER_STREAMING, updated.getMethodType());
        org.junit.jupiter.api.Assertions.assertNotSame(initial, updated);
    }

    @Test
    public void testInvalidateAndCleanGrpcExtInfo() {
        org.apache.shenyu.common.dto.MetaData metaData = org.apache.shenyu.common.dto.MetaData.builder()
                .id("meta-1")
                .path("/grpc/invalidate")
                .serviceName("service1")
                .methodName("method1")
                .rpcExt("{\"methodType\":\"UNARY\",\"timeout\":6000}")
                .build();

        GrpcExtInfo info1 = this.applicationConfigCache.obtainGrpcExtInfo(metaData);
        org.junit.jupiter.api.Assertions.assertSame(info1, this.applicationConfigCache.obtainGrpcExtInfo(metaData));

        this.applicationConfigCache.invalidateGrpcExtInfo(metaData);
        GrpcExtInfo info2 = this.applicationConfigCache.obtainGrpcExtInfo(metaData);
        org.junit.jupiter.api.Assertions.assertNotSame(info1, info2);

        this.applicationConfigCache.cleanGrpcExtInfo();
        GrpcExtInfo info3 = this.applicationConfigCache.obtainGrpcExtInfo(metaData);
        org.junit.jupiter.api.Assertions.assertNotSame(info2, info3);
    }
}
