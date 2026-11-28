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
import io.grpc.ClientCall;
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
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;
import org.mockito.stubbing.Answer;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.FutureTask;
import java.util.concurrent.TimeUnit;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTimeoutPreemptively;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
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
    public void testRetiredClientRejectsInvocationBeforeCreatingTransportCall() {
        ManagedChannel channel = mock(ManagedChannel.class);
        final ShenyuGrpcClient client = new ShenyuGrpcClient(channel);
        ShenyuGrpcCallRequest request = newCallParams(MethodDescriptor.MethodType.UNARY, newRequestList());
        request.setChannel(channel);

        client.close();
        client.close();

        ExecutionException failure = assertThrows(ExecutionException.class,
                () -> client.invoke(request).get(5, TimeUnit.SECONDS));
        assertEquals("gRPC client is retired", failure.getCause().getMessage());
        verify(channel).shutdown();
        verify(channel, never()).newCall(any(), any());
    }

    @Test
    @SuppressWarnings("unchecked")
    public void testRetirementWaitsUntilAdmittedCallHasStarted() throws Exception {
        ManagedChannel channel = mock(ManagedChannel.class);
        ClientCall<DynamicMessage, DynamicMessage> transport = mock(ClientCall.class);
        final ShenyuGrpcClient client = new ShenyuGrpcClient(channel);
        ShenyuGrpcCallRequest request = newCallParams(MethodDescriptor.MethodType.UNARY, newRequestList());
        request.setChannel(channel);
        CountDownLatch enteringStart = new CountDownLatch(1);
        CountDownLatch finishStart = new CountDownLatch(1);
        doReturn(transport).when(channel).newCall(any(), any());
        doAnswer(invocation -> {
            enteringStart.countDown();
            assertTrue(finishStart.await(5, TimeUnit.SECONDS));
            return null;
        }).when(transport).start(any(), any());
        FutureTask<ListenableFuture<Void>> start = new FutureTask<>(() -> client.invoke(request));
        FutureTask<Void> retire = new FutureTask<>(client::close, null);
        Thread caller = new Thread(start, "grpc-test-caller");
        Thread closer = new Thread(retire, "grpc-test-retirement");

        caller.start();
        try {
            assertTrue(enteringStart.await(5, TimeUnit.SECONDS));
            closer.start();
            await().atMost(Duration.ofSeconds(5)).until(() -> closer.getState() == Thread.State.BLOCKED || retire.isDone());
            assertFalse(retire.isDone());
            verify(channel, never()).shutdown();
        } finally {
            finishStart.countDown();
        }

        ListenableFuture<Void> invocation = start.get(5, TimeUnit.SECONDS);
        retire.get(5, TimeUnit.SECONDS);
        verify(channel).shutdown();
        assertFalse(invocation.isDone());
        invocation.cancel(true);
    }

    @Test
    @SuppressWarnings("unchecked")
    public void testBlockedSendDoesNotDelayAnotherCallStart() throws Exception {
        final ManagedChannel channel = mock(ManagedChannel.class);
        final ClientCall<DynamicMessage, DynamicMessage> firstTransport = mock(ClientCall.class);
        final ClientCall<DynamicMessage, DynamicMessage> secondTransport = mock(ClientCall.class);
        final ShenyuGrpcClient client = new ShenyuGrpcClient(channel);
        final ShenyuGrpcCallRequest firstRequest = newCallParams(MethodDescriptor.MethodType.UNARY, newRequestList());
        final ShenyuGrpcCallRequest secondRequest = newCallParams(MethodDescriptor.MethodType.UNARY, newRequestList());
        firstRequest.setChannel(channel);
        secondRequest.setChannel(channel);
        final CountDownLatch sending = new CountDownLatch(1);
        final CountDownLatch releaseSend = new CountDownLatch(1);
        doReturn(firstTransport, secondTransport).when(channel).newCall(any(), any());
        doAnswer(invocation -> {
            sending.countDown();
            assertTrue(releaseSend.await(15, TimeUnit.SECONDS));
            return null;
        }).when(firstTransport).sendMessage(any());
        final FutureTask<ListenableFuture<Void>> first = new FutureTask<>(() -> client.invoke(firstRequest));
        final FutureTask<ListenableFuture<Void>> second = new FutureTask<>(() -> client.invoke(secondRequest));

        new Thread(first, "grpc-test-blocked-send").start();
        try {
            assertTrue(sending.await(5, TimeUnit.SECONDS));
            new Thread(second, "grpc-test-second-call").start();
            second.get(5, TimeUnit.SECONDS).cancel(true);
            verify(secondTransport).start(any(), any());
            assertFalse(first.isDone());
        } finally {
            releaseSend.countDown();
        }
        first.get(5, TimeUnit.SECONDS).cancel(true);
    }

    @ParameterizedTest
    @EnumSource(value = MethodDescriptor.MethodType.class, names = {"UNARY", "SERVER_STREAMING", "CLIENT_STREAMING", "BIDI_STREAMING"})
    @SuppressWarnings("unchecked")
    public void testRetirementRejectsPreparedCallBeforeItStarts(final MethodDescriptor.MethodType methodType) throws Exception {
        final ManagedChannel channel = mock(ManagedChannel.class);
        final ClientCall<DynamicMessage, DynamicMessage> transport = mock(ClientCall.class);
        final ShenyuGrpcClient client = new ShenyuGrpcClient(channel);
        final ShenyuGrpcCallRequest request = newCallParams(methodType, newRequestList());
        final CountDownLatch preparing = new CountDownLatch(1);
        final CountDownLatch releasePreparation = new CountDownLatch(1);
        request.setChannel(channel);
        doAnswer(invocation -> {
            preparing.countDown();
            assertTrue(releasePreparation.await(15, TimeUnit.SECONDS));
            return transport;
        }).when(channel).newCall(any(), any());
        final FutureTask<ListenableFuture<Void>> start = new FutureTask<>(() -> client.invoke(request));

        new Thread(start, "grpc-test-preparing-call").start();
        try {
            assertTrue(preparing.await(5, TimeUnit.SECONDS));
            client.close();
        } finally {
            releasePreparation.countDown();
        }
        final ListenableFuture<Void> result = start.get(5, TimeUnit.SECONDS);
        final ExecutionException failure = assertThrows(ExecutionException.class, () -> result.get(5, TimeUnit.SECONDS));
        assertEquals("gRPC client is retired", failure.getCause().getMessage());
        verify(transport).cancel(any(), any());
        verify(transport, never()).start(any(), any());
        verify(transport, never()).sendMessage(any());
    }

    @Test
    @SuppressWarnings("unchecked")
    public void testCancellationFailurePreservesRetirementError() throws Exception {
        final ManagedChannel channel = mock(ManagedChannel.class);
        final ClientCall<DynamicMessage, DynamicMessage> transport = mock(ClientCall.class);
        final ShenyuGrpcClient client = new ShenyuGrpcClient(channel);
        final ShenyuGrpcCallRequest request = newCallParams(MethodDescriptor.MethodType.UNARY, newRequestList());
        request.setChannel(channel);
        final IllegalStateException cancellationFailure = new IllegalStateException("Transport cancellation failed");
        doAnswer(invocation -> {
            client.close();
            return transport;
        }).when(channel).newCall(any(), any());
        doThrow(cancellationFailure).when(transport).cancel(any(), any());

        final ListenableFuture<Void> result = client.invoke(request);

        final ExecutionException failure = assertThrows(ExecutionException.class, () -> result.get(5, TimeUnit.SECONDS));
        assertEquals("gRPC client is retired", failure.getCause().getMessage());
        assertEquals(List.of(cancellationFailure), List.of(failure.getCause().getSuppressed()));
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
