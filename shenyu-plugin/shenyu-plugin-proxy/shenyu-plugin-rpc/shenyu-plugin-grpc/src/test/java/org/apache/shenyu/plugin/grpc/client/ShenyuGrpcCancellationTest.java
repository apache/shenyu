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
import com.google.protobuf.DynamicMessage;
import io.grpc.CallOptions;
import io.grpc.ClientCall;
import io.grpc.ManagedChannel;
import io.grpc.Metadata;
import io.grpc.MethodDescriptor;
import io.grpc.Status;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.plugin.grpc.proto.MessageWriter;
import org.apache.shenyu.plugin.grpc.proto.ShenyuGrpcCallRequest;
import org.apache.shenyu.plugin.grpc.proto.ShenyuGrpcResponse;
import org.apache.shenyu.protocol.grpc.message.JsonMessage;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Exercises the real invocation path against a recording transport, without mocking invoke().
 */
class ShenyuGrpcCancellationTest {

    @ParameterizedTest
    @EnumSource(value = MethodDescriptor.MethodType.class, names = {"UNARY", "SERVER_STREAMING", "CLIENT_STREAMING", "BIDI_STREAMING"})
    void cancellationBeforeResponseReachesTransport(final MethodDescriptor.MethodType type) {
        RecordingCall call = new RecordingCall();
        CompletableFuture<ShenyuGrpcResponse> result = start(call, type);
        assertFalse(call.serverCancelled.isDone());
        assertTrue(result.cancel(true));
        result.cancel(true);
        assertEquals(1, call.cancellations.get());
        assertTrue(call.serverCancelled.isDone());
        assertTrue(result.isCancelled());
    }

    @Test
    void cancellationDuringStreamingReachesTransport() {
        RecordingCall call = new RecordingCall();
        CompletableFuture<ShenyuGrpcResponse> result = start(call, MethodDescriptor.MethodType.SERVER_STREAMING);
        call.listener.onMessage(JsonMessage.buildJsonMessage("{\"value\":1}"));
        assertFalse(result.isDone());
        result.cancel(false);
        assertEquals(1, call.cancellations.get());
        assertTrue(call.serverCancelled.isDone());
    }

    @Test
    void reactorCancellationReachesTransport() {
        RecordingCall call = new RecordingCall();
        CompletableFuture<ShenyuGrpcResponse> result = start(call, MethodDescriptor.MethodType.SERVER_STREAMING);
        StepVerifier.create(Mono.fromFuture(result)).thenCancel().verify();
        assertTrue(call.serverCancelled.isDone());
        assertEquals(1, call.cancellations.get());
    }

    @Test
    void normalCompletionDoesNotCancelTransport() {
        RecordingCall call = new RecordingCall();
        CompletableFuture<ShenyuGrpcResponse> result = start(call, MethodDescriptor.MethodType.UNARY);
        call.listener.onMessage(JsonMessage.buildJsonMessage("{\"value\":1}"));
        call.listener.onClose(Status.OK, new Metadata());
        assertEquals(1, result.join().getResults().size());
        assertFalse(result.cancel(true));
        assertEquals(0, call.cancellations.get());
    }

    @Test
    void upstreamFailureDoesNotCancelTransportAgain() {
        RecordingCall call = new RecordingCall();
        CompletableFuture<ShenyuGrpcResponse> result = start(call, MethodDescriptor.MethodType.SERVER_STREAMING);
        call.listener.onClose(Status.UNAVAILABLE, new Metadata());
        assertTrue(result.isCompletedExceptionally());
        assertFalse(result.cancel(true));
        assertEquals(0, call.cancellations.get());
    }

    @Test
    void completionRacingCancellationCleansTransportAtMostOnce() {
        for (int attempt = 0; attempt < 50; attempt++) {
            RecordingCall call = new RecordingCall();
            CompletableFuture<ShenyuGrpcResponse> result = start(call, MethodDescriptor.MethodType.SERVER_STREAMING);
            CompletableFuture<Void> gate = new CompletableFuture<>();
            CompletableFuture<Void> cancel = gate.thenRunAsync(() -> result.cancel(true));
            CompletableFuture<Void> complete = gate.thenRunAsync(() -> call.listener.onClose(Status.OK, new Metadata()));
            gate.complete(null);
            CompletableFuture.allOf(cancel, complete).join();
            assertTrue(result.isDone());
            // Completion may win on the invocation before its result callback sees cancellation.
            assertTrue(call.cancellations.get() <= 1);
        }
    }

    @Test
    void overflowAndDownstreamCancellationShareTransportCleanup() {
        RecordingCall call = new RecordingCall();
        call.closeOnCancel = false;
        ShenyuGrpcClient client = client(call);
        ShenyuGrpcCallRequest request = new ShenyuGrpcCallRequest();
        request.setChannel(channel(call));
        request.setCallOptions(CallOptions.DEFAULT);
        DynamicMessage message = JsonMessage.buildJsonMessage("{\"value\":1}");
        request.setMethodDescriptor(JsonMessage.createJsonMarshallerMethodDescriptor("cancel.Service", "overflow",
                MethodDescriptor.MethodType.SERVER_STREAMING, message, JsonMessage.buildJsonMessage()));
        request.setRequests(List.of(message));
        ShenyuGrpcResponse response = new ShenyuGrpcResponse();
        request.setResponseObserver(MessageWriter.newInstance(response, 1, 0));
        final ListenableFuture<Void> completion = client.invoke(request);
        call.listener.onMessage(message);
        call.listener.onMessage(message);
        assertTrue(response.getResults().isEmpty());
        assertEquals(1, call.cancellations.get());
        completion.cancel(true);
        assertEquals(1, call.cancellations.get());
    }

    private CompletableFuture<ShenyuGrpcResponse> start(final RecordingCall call, final MethodDescriptor.MethodType type) {
        return client(call).call(MetaData.builder().serviceName("cancel.Service").methodName(type.name()).build(),
                CallOptions.DEFAULT, "{\"data\":[{}]}", type);
    }

    private ShenyuGrpcClient client(final RecordingCall call) {
        return new ShenyuGrpcClient(channel(call));
    }

    private ManagedChannel channel(final RecordingCall call) {
        ManagedChannel channel = mock(ManagedChannel.class);
        when(channel.<DynamicMessage, DynamicMessage>newCall(any(), any())).thenReturn(call);
        return channel;
    }

    private static final class RecordingCall extends ClientCall<DynamicMessage, DynamicMessage> {

        private final AtomicInteger cancellations = new AtomicInteger();

        private final CompletableFuture<Void> serverCancelled = new CompletableFuture<>();

        private Listener<DynamicMessage> listener;

        private boolean closeOnCancel = true;

        @Override
        public void start(final Listener<DynamicMessage> responseListener, final Metadata headers) {
            listener = responseListener;
        }

        @Override
        public void request(final int numMessages) {
        }

        @Override
        public void cancel(final String message, final Throwable cause) {
            cancellations.incrementAndGet();
            serverCancelled.complete(null);
            if (closeOnCancel) {
                listener.onClose(Status.CANCELLED.withDescription(message).withCause(cause), new Metadata());
            }
        }

        @Override
        public void halfClose() {
        }

        @Override
        public void sendMessage(final DynamicMessage message) {
        }
    }
}
