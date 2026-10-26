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

import com.google.common.util.concurrent.ListenableFuture;
import com.google.common.util.concurrent.SettableFuture;
import com.google.common.util.concurrent.Futures;
import com.google.protobuf.DynamicMessage;
import io.grpc.CallOptions;
import io.grpc.ManagedChannel;
import io.grpc.MethodDescriptor;
import io.grpc.stub.ClientCallStreamObserver;
import io.grpc.stub.ClientCalls;
import io.grpc.stub.ClientResponseObserver;
import io.grpc.stub.StreamObserver;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.plugin.grpc.proto.ShenyuGrpcCallRequest;
import org.apache.shenyu.plugin.grpc.proto.ShenyuGrpcResponse;
import org.apache.shenyu.protocol.grpc.message.JsonMessage;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;
import org.mockito.stubbing.Answer;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTimeoutPreemptively;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

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

    @Test
    public void testClientStreamingDrainsAccordingToReadiness() {
        assertStreamingDrains(MethodDescriptor.MethodType.CLIENT_STREAMING);
    }

    @Test
    public void testBidiStreamingDrainsAccordingToReadiness() {
        assertStreamingDrains(MethodDescriptor.MethodType.BIDI_STREAMING);
    }

    @Test
    public void cancellationStopsPendingStreamingRequests() {
        FakeRequestObserver requestObserver = new FakeRequestObserver();
        ShenyuGrpcCallRequest callParams = newCallParams(MethodDescriptor.MethodType.BIDI_STREAMING, newRequestList());
        ShenyuGrpcClient client = new ShenyuGrpcClient(mock(ManagedChannel.class));

        try (MockedStatic<ClientCalls> calls = mockStatic(ClientCalls.class)) {
            calls.when(() -> ClientCalls.asyncBidiStreamingCall(any(), any())).thenAnswer(readinessAnswer(requestObserver));
            ListenableFuture<Void> future = client.invoke(callParams);
            future.cancel(true);
            requestObserver.setReady(true);
            requestObserver.fireReady();
        }

        assertTrue(requestObserver.getSent().isEmpty(), "a cancelled stream must not send pending requests");
    }

    private void assertStreamingDrains(final MethodDescriptor.MethodType methodType) {
        FakeRequestObserver requestObserver = new FakeRequestObserver();
        List<DynamicMessage> requests = newRequestList();
        ShenyuGrpcCallRequest callParams = newCallParams(methodType, requests);
        ShenyuGrpcClient client = new ShenyuGrpcClient(mock(ManagedChannel.class));

        try (MockedStatic<ClientCalls> calls = mockStatic(ClientCalls.class)) {
            Answer<StreamObserver<DynamicMessage>> answer = readinessAnswer(requestObserver);
            calls.when(() -> ClientCalls.asyncClientStreamingCall(any(), any())).thenAnswer(answer);
            calls.when(() -> ClientCalls.asyncBidiStreamingCall(any(), any())).thenAnswer(answer);
            client.invoke(callParams);
        }

        assertTrue(requestObserver.getSent().isEmpty(), "must not send while the stream is not ready");
        requestObserver.setReady(true);
        requestObserver.fireReady();
        assertEquals(requests, requestObserver.getSent());
        assertEquals(1, requestObserver.getCompleted());
    }

    private List<DynamicMessage> newRequestList() {
        List<DynamicMessage> requests = new ArrayList<>();
        requests.add(JsonMessage.buildJsonMessage());
        requests.add(JsonMessage.buildJsonMessage());
        return requests;
    }

    private ShenyuGrpcCallRequest newCallParams(final MethodDescriptor.MethodType methodType, final List<DynamicMessage> requests) {
        ShenyuGrpcCallRequest callParams = new ShenyuGrpcCallRequest();
        callParams.setChannel(mock(ManagedChannel.class));
        callParams.setCallOptions(CallOptions.DEFAULT);
        callParams.setRequests(requests);
        @SuppressWarnings("unchecked")
        StreamObserver<DynamicMessage> responseObserver = mock(StreamObserver.class);
        callParams.setResponseObserver(responseObserver);
        @SuppressWarnings("unchecked")
        MethodDescriptor<DynamicMessage, DynamicMessage> descriptor = mock(MethodDescriptor.class);
        doReturn(methodType).when(descriptor).getType();
        callParams.setMethodDescriptor(descriptor);
        return callParams;
    }

    private Answer<StreamObserver<DynamicMessage>> readinessAnswer(final FakeRequestObserver requestObserver) {
        return invocation -> {
            StreamObserver<DynamicMessage> observer = invocation.getArgument(1);
            if (observer instanceof ClientResponseObserver) {
                @SuppressWarnings("unchecked")
                ClientResponseObserver<DynamicMessage, DynamicMessage> clientObserver =
                        (ClientResponseObserver<DynamicMessage, DynamicMessage>) observer;
                clientObserver.beforeStart(requestObserver);
            }
            return requestObserver;
        };
    }

    private static final class FakeRequestObserver extends ClientCallStreamObserver<DynamicMessage> {

        private final List<DynamicMessage> sent = new ArrayList<>();

        private boolean ready;

        private boolean compressionEnabled;

        private int requested;

        private int autoInboundDisabled;

        private int completed;

        private int cancelled;

        private Throwable error;

        private Runnable onReadyHandler;

        @Override
        public void onNext(final DynamicMessage value) {
            sent.add(value);
        }

        @Override
        public void onError(final Throwable throwable) {
            this.error = throwable;
        }

        @Override
        public void onCompleted() {
            completed++;
        }

        @Override
        public void cancel(final String message, final Throwable cause) {
            cancelled++;
            this.error = cause;
        }

        @Override
        public boolean isReady() {
            return ready;
        }

        @Override
        public void setOnReadyHandler(final Runnable handler) {
            this.onReadyHandler = handler;
        }

        @Override
        public void request(final int count) {
            this.requested = count;
        }

        @Override
        public void setMessageCompression(final boolean enable) {
            this.compressionEnabled = enable;
        }

        @Override
        public void disableAutoInboundFlowControl() {
            autoInboundDisabled++;
        }

        List<DynamicMessage> getSent() {
            return sent;
        }

        void setReady(final boolean ready) {
            this.ready = ready;
        }

        int getCompleted() {
            return completed;
        }

        void fireReady() {
            if (Objects.nonNull(onReadyHandler)) {
                onReadyHandler.run();
            }
        }
    }
}
