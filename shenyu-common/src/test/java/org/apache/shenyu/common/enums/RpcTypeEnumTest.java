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

package org.apache.shenyu.common.enums;

import org.apache.shenyu.common.exception.ShenyuException;
import org.junit.Test;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class RpcTypeEnumTest {
    @Test
    public void testGetName() {
        assertEquals("grpc", RpcTypeEnum.GRPC.getName());
        assertEquals("dubbo", RpcTypeEnum.DUBBO.getName());
        assertEquals("http", RpcTypeEnum.HTTP.getName());
        assertEquals("motan", RpcTypeEnum.MOTAN.getName());
        assertEquals("sofa", RpcTypeEnum.SOFA.getName());
        assertEquals("springCloud", RpcTypeEnum.SPRING_CLOUD.getName());
        assertEquals("tars", RpcTypeEnum.TARS.getName());
        assertEquals("websocket", RpcTypeEnum.WEB_SOCKET.getName());
    }

    @Test
    public void testSupport() {
        assertTrue(RpcTypeEnum.GRPC.getSupport());
        assertTrue(RpcTypeEnum.DUBBO.getSupport());
        assertTrue(RpcTypeEnum.HTTP.getSupport());
        assertTrue(RpcTypeEnum.MOTAN.getSupport());
        assertTrue(RpcTypeEnum.SOFA.getSupport());
        assertTrue(RpcTypeEnum.SPRING_CLOUD.getSupport());
        assertTrue(RpcTypeEnum.TARS.getSupport());
        assertTrue(RpcTypeEnum.WEB_SOCKET.getSupport());
    }

    @Test
    public void testAcquireSupport() {
        List<RpcTypeEnum> supportRpcTypeList =
                Arrays.stream(RpcTypeEnum.values())
                        .filter(RpcTypeEnum::getSupport)
                        .collect(Collectors.toList());
        assertEquals(RpcTypeEnum.acquireSupports(), supportRpcTypeList);
    }

    @Test
    public void testGetRpcTypeEnumByNameValid() {
        Arrays.stream(RpcTypeEnum.values())
                .filter(RpcTypeEnum::getSupport)
                .forEach(
                    rpcTypeEnum ->
                            assertEquals(rpcTypeEnum, RpcTypeEnum.acquireByName(rpcTypeEnum.getName())));
    }

    @Test
    public void testAcquireSupportURIs() {
        List<RpcTypeEnum> rpcTypeEnumList = RpcTypeEnum.acquireSupportURIs();
        assertTrue(rpcTypeEnumList.contains(RpcTypeEnum.GRPC));
        assertTrue(rpcTypeEnumList.contains(RpcTypeEnum.HTTP));
        assertTrue(rpcTypeEnumList.contains(RpcTypeEnum.TARS));
    }

    @Test
    public void testAcquireSupportMetadatas() {
        List<RpcTypeEnum> rpcTypeEnumList = RpcTypeEnum.acquireSupportMetadatas();
        assertTrue(rpcTypeEnumList.contains(RpcTypeEnum.DUBBO));
        assertTrue(rpcTypeEnumList.contains(RpcTypeEnum.GRPC));
        assertTrue(rpcTypeEnumList.contains(RpcTypeEnum.SPRING_CLOUD));
        assertTrue(rpcTypeEnumList.contains(RpcTypeEnum.SOFA));
        assertTrue(rpcTypeEnumList.contains(RpcTypeEnum.TARS));
    }

    @Test(expected = ShenyuException.class)
    public void testGetRpcTypeEnumByNameInvalid() {
        RpcTypeEnum.acquireByName("InvalidName");
    }
}
