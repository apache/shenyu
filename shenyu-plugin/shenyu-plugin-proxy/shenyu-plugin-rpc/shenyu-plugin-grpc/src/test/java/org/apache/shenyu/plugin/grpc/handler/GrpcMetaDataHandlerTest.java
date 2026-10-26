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

import io.grpc.MethodDescriptor;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.grpc.cache.ApplicationConfigCache;
import org.apache.shenyu.plugin.grpc.cache.GrpcExtInfo;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

/**
 * The Test Case For {@link GrpcMetaDataHandler}.
 */
public class GrpcMetaDataHandlerTest {

    private GrpcMetaDataHandler grpcMetaDataHandler;

    @BeforeEach
    public void setUp() {
        this.grpcMetaDataHandler = new GrpcMetaDataHandler();
        ApplicationConfigCache.getInstance().cleanGrpcExtInfo();
    }

    @Test
    public void testRpcType() {
        assertEquals(RpcTypeEnum.GRPC.getName(), grpcMetaDataHandler.rpcType());
    }

    @Test
    public void testHandleAndRemoveAndRefresh() {
        MetaData metaData = MetaData.builder()
                .id("1")
                .path("/grpc/test")
                .serviceName("testService")
                .methodName("testMethod")
                .rpcExt("{\"methodType\":\"UNARY\",\"timeout\":3000}")
                .build();

        grpcMetaDataHandler.handle(metaData);
        GrpcExtInfo info1 = ApplicationConfigCache.getInstance().obtainGrpcExtInfo(metaData);
        assertEquals(3000, info1.getTimeout());
        assertEquals(MethodDescriptor.MethodType.UNARY, info1.getMethodType());

        GrpcExtInfo info2 = ApplicationConfigCache.getInstance().obtainGrpcExtInfo(metaData);
        assertSame(info1, info2);

        grpcMetaDataHandler.remove(metaData);
        // After removal, next obtain re-parses and returns new instance
        GrpcExtInfo info3 = ApplicationConfigCache.getInstance().obtainGrpcExtInfo(metaData);
        assertEquals(3000, info3.getTimeout());

        grpcMetaDataHandler.refresh();
        GrpcExtInfo info4 = ApplicationConfigCache.getInstance().obtainGrpcExtInfo(metaData);
        assertEquals(3000, info4.getTimeout());
    }
}
