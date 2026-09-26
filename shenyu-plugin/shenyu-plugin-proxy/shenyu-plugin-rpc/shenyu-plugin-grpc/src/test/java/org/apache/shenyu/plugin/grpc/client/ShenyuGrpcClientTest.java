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

package org.apache.shenyu.plugin.grpc.client;

import com.google.common.util.concurrent.Futures;
import io.grpc.CallOptions;
import io.grpc.ManagedChannel;
import io.grpc.MethodDescriptor;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.plugin.grpc.proto.ShenyuGrpcCallRequest;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

/**
 * Test cases for {@link ShenyuGrpcClient}.
 */
public final class ShenyuGrpcClientTest {

    @Test
    public void testCallWithNullRequestCreatesDefaultMessage() {
        assertDefaultRequest(null);
    }

    @Test
    public void testCallWithEmptyRequestListCreatesDefaultMessage() {
        assertDefaultRequest("{\"data\":[]}");
    }

    private void assertDefaultRequest(final String requestJsons) {
        ShenyuGrpcClient client = spy(new ShenyuGrpcClient(mock(ManagedChannel.class)));
        doReturn(Futures.immediateVoidFuture()).when(client).invoke(any(ShenyuGrpcCallRequest.class));
        MetaData metaData = new MetaData();
        metaData.setServiceName("service");
        metaData.setMethodName("method");

        client.call(metaData, CallOptions.DEFAULT, requestJsons, MethodDescriptor.MethodType.UNARY).join();

        ArgumentCaptor<ShenyuGrpcCallRequest> requestCaptor = ArgumentCaptor.forClass(ShenyuGrpcCallRequest.class);
        verify(client).invoke(requestCaptor.capture());
        assertEquals(1, requestCaptor.getValue().getRequests().size());
    }
}
