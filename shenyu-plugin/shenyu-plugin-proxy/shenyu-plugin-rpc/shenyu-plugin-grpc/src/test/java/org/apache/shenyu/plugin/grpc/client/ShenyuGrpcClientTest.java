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

import com.google.common.util.concurrent.SettableFuture;
import io.grpc.CallOptions;
import io.grpc.ManagedChannel;
import io.grpc.MethodDescriptor;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.plugin.grpc.proto.ShenyuGrpcCallRequest;
import org.apache.shenyu.plugin.grpc.proto.ShenyuGrpcResponse;
import org.junit.jupiter.api.Test;

import java.time.Duration;
import java.util.concurrent.CompletableFuture;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTimeoutPreemptively;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;

/**
 * Test cases for {@link ShenyuGrpcClient}.
 */
public final class ShenyuGrpcClientTest {

    @Test
    public void testCallCompletesAsynchronously() {
        ShenyuGrpcClient client = spy(new ShenyuGrpcClient(mock(ManagedChannel.class)));
        SettableFuture<Void> invocation = SettableFuture.create();
        doReturn(invocation).when(client).invoke(any(ShenyuGrpcCallRequest.class));
        MetaData metaData = MetaData.builder()
                .serviceName("echo.EchoService")
                .methodName("echo")
                .build();

        CompletableFuture<ShenyuGrpcResponse> result = assertTimeoutPreemptively(Duration.ofSeconds(1),
                () -> client.call(metaData, CallOptions.DEFAULT,
                        "{\"data\":[{}]}", MethodDescriptor.MethodType.UNARY));

        assertFalse(result.isDone());
        invocation.set(null);
        assertTrue(result.isDone());
        assertFalse(result.isCompletedExceptionally());
    }

    @Test
    public void testCancellationPropagatesToInvocation() {
        ShenyuGrpcClient client = spy(new ShenyuGrpcClient(mock(ManagedChannel.class)));
        SettableFuture<Void> invocation = SettableFuture.create();
        doReturn(invocation).when(client).invoke(any(ShenyuGrpcCallRequest.class));
        MetaData metaData = MetaData.builder()
                .serviceName("echo.EchoService")
                .methodName("echo")
                .build();

        CompletableFuture<ShenyuGrpcResponse> result = assertTimeoutPreemptively(Duration.ofSeconds(1),
                () -> client.call(metaData, CallOptions.DEFAULT,
                        "{\"data\":[{}]}", MethodDescriptor.MethodType.UNARY));
        result.cancel(true);

        assertTrue(invocation.isCancelled());
    }
}
