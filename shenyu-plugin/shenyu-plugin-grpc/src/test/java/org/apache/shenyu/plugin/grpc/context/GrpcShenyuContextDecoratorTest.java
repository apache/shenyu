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

package org.apache.shenyu.plugin.grpc.context;

import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * The Test Case For {@link GrpcShenyuContextDecorator}.
 */
public class GrpcShenyuContextDecoratorTest {

    private GrpcShenyuContextDecorator grpcShenyuContextDecorator;

    @Before
    public void setup() {
        grpcShenyuContextDecorator = new GrpcShenyuContextDecorator();
    }

    @Test
    public void testDecorator() {
        MetaData metaData = new MetaData();
        metaData.setAppName("grpc");
        metaData.setServiceName("echo");
        metaData.setRpcType(PluginEnum.GRPC.getName());
        metaData.setContextPath("/grpc");
        assert grpcShenyuContextDecorator.decorator(new ShenyuContext(), metaData) != null;
    }

    @Test
    public void testRpcType() {
        assertEquals(grpcShenyuContextDecorator.rpcType(), PluginEnum.GRPC.getName());
    }
}
