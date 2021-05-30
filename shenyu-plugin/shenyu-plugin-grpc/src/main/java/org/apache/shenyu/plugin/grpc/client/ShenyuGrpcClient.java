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
import com.google.protobuf.Message;
import com.google.protobuf.util.JsonFormat;
import io.grpc.CallOptions;
import io.grpc.ClientCall;
import io.grpc.ManagedChannel;
import io.grpc.MethodDescriptor;
import io.grpc.stub.StreamObserver;
import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.plugin.grpc.proto.CompleteObserver;
import org.apache.shenyu.plugin.grpc.proto.MessageWriter;
import org.apache.shenyu.plugin.grpc.proto.ShenyuGrpcCallRequest;
import org.apache.shenyu.plugin.grpc.proto.ShenyuGrpcResponse;
import org.apache.shenyu.plugin.grpc.proto.CompositeStreamObserver;
import org.apache.shenyu.protocol.grpc.message.JsonReply;
import org.apache.shenyu.protocol.grpc.message.JsonRequest;
import org.apache.shenyu.protocol.grpc.utils.GrpcUtils;

import java.io.Closeable;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import static io.grpc.stub.ClientCalls.asyncServerStreamingCall;
import static io.grpc.stub.ClientCalls.asyncUnaryCall;
import static io.grpc.stub.ClientCalls.asyncClientStreamingCall;
import static io.grpc.stub.ClientCalls.asyncBidiStreamingCall;

/**
 * The shenyu grpc client.
 */
@Slf4j
public class ShenyuGrpcClient implements Closeable {

    private final ManagedChannel channel;

    public ShenyuGrpcClient(final ManagedChannel channel) {
        this.channel = channel;
    }

    /**
     * Grpc call.
     *
     * @param metaData     metadata
     * @param callOptions  callOptions
     * @param requestJsons requestJsons
     * @return CompletableFuture future
     */
    public CompletableFuture<ShenyuGrpcResponse> call(final MetaData metaData, final CallOptions callOptions, final String requestJsons) {
        MethodDescriptor<Message, Message> methodDescriptor = GrpcUtils.createJsonMethodDescriptor(metaData.getServiceName(), metaData.getMethodName());

        JsonFormat.TypeRegistry registry = JsonFormat.TypeRegistry.newBuilder().add(JsonReply.getDescriptor()).build();
        ShenyuGrpcResponse shenyuGrpcResponse = new ShenyuGrpcResponse();
        StreamObserver<Message> streamObserver = MessageWriter.newInstance(registry, shenyuGrpcResponse);

        JsonRequest requestMessages = JsonRequest.newBuilder()
                .setMessage(requestJsons).build();

        ShenyuGrpcCallRequest callParams = ShenyuGrpcCallRequest.builder()
                .methodDescriptor(methodDescriptor)
                .channel(channel)
                .callOptions(callOptions)
                .requests(requestMessages)
                .responseObserver(streamObserver)
                .build();
        try {
            this.invoke(callParams).get();
        } catch (InterruptedException | ExecutionException e) {
            throw new RuntimeException("Caught exception while waiting for rpc :{ " + e.getMessage() + "}");
        }
        return CompletableFuture.completedFuture(shenyuGrpcResponse);
    }

    /**
     * Grpc call.
     *
     * @param callParams callParams
     * @return ListenableFuture future
     */
    public ListenableFuture<Void> invoke(final ShenyuGrpcCallRequest callParams) {
        MethodDescriptor.MethodType methodType = callParams.getMethodDescriptor().getType();
        JsonRequest request = callParams.getRequests();

        StreamObserver<Message> responseObserver = callParams.getResponseObserver();
        CompleteObserver<Message> doneObserver = new CompleteObserver<>();
        StreamObserver<Message> compositeObserver = CompositeStreamObserver.of(responseObserver, doneObserver);

        StreamObserver<Message> requestObserver;
        switch (methodType) {
            case UNARY:
                asyncUnaryCall(createCall(callParams), request, compositeObserver);
                return doneObserver.getCompletionFuture();
            case SERVER_STREAMING:
                asyncServerStreamingCall(createCall(callParams), request, compositeObserver);
                return doneObserver.getCompletionFuture();
            case CLIENT_STREAMING:
                requestObserver = asyncClientStreamingCall(createCall(callParams), compositeObserver);
                requestObserver.onCompleted();
                return doneObserver.getCompletionFuture();
            case BIDI_STREAMING:
                requestObserver = asyncBidiStreamingCall(createCall(callParams), compositeObserver);
                requestObserver.onCompleted();
                return doneObserver.getCompletionFuture();
            default:
                log.info("Unknown methodType:{}", methodType);
                return null;
        }
    }

    @Override
    public void close() {
        this.channel.shutdown();
    }

    private ClientCall<Message, Message> createCall(final ShenyuGrpcCallRequest callParams) {
        return callParams.getChannel().newCall(callParams.getMethodDescriptor(),
                callParams.getCallOptions());
    }
}
