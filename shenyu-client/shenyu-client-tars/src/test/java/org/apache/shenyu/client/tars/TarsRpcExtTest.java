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

package org.apache.shenyu.client.tars;

import org.apache.shenyu.client.tars.common.dto.TarsRpcExt;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.List;

/**
 * Test case for {@link TarsRpcExt}.
 */
@ExtendWith(MockitoExtension.class)
@TestMethodOrder(MethodOrderer.Alphanumeric.class)
public class TarsRpcExtTest {

    @Test
    public void testTarsRpcExt() {
        TarsRpcExt.RpcExt rpcExt = new TarsRpcExt.RpcExt();
        rpcExt.setMethodName("methodName");
        rpcExt.setParams(null);
        rpcExt.setReturnType("returnType");
        final List<TarsRpcExt.RpcExt> rpcExtList = new ArrayList<>();
        rpcExtList.add(rpcExt);
        TarsRpcExt tarsRpcExt = new TarsRpcExt();
        tarsRpcExt.setMethodInfo(rpcExtList);
        Assertions.assertNotNull(tarsRpcExt.toString());
        Assertions.assertNotNull(tarsRpcExt.getMethodInfo());
        Assertions.assertNotNull(rpcExt.toString());
        Assertions.assertNotNull(rpcExt.getMethodName());
        Assertions.assertNotNull(rpcExt.getReturnType());
        Assertions.assertNull(rpcExt.getParams());
    }
}

