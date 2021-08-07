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
import io.grpc.MethodDescriptor;
import io.grpc.stub.StreamObserver;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.grpc.proto.CompleteObserver;
import org.apache.shenyu.plugin.grpc.proto.MessageWriter;
import org.apache.shenyu.plugin.grpc.proto.ShenyuGrpcCallRequest;
import org.apache.shenyu.plugin.grpc.proto.ShenyuGrpcResponse;
import org.apache.shenyu.plugin.grpc.proto.CompositeStreamObserver;
import org.apache.shenyu.protocol.grpc.message.JsonMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Closeable;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import static io.grpc.stub.ClientCalls.asyncServerStreamingCall;
import static io.grpc.stub.ClientCalls.asyncUnaryCall;
import static io.grpc.stub.ClientCalls.asyncClientStreamingCall;
import static io.grpc.stub.ClientCalls.asyncBidiStreamingCall;

/**
 * The shenyu grpc client.
 */
public class ShenyuGrpcClient implements Closeable {

    private static final Logger LOG = LoggerFactory.getLogger(ShenyuGrpcClient.class);

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
     * @param methodType methodType
     * @return CompletableFuture future
     */
    public CompletableFuture<ShenyuGrpcResponse> call(final MetaData metaData,
                                                      final CallOptions callOptions,
                                                      final String requestJsons,
                                                      final MethodDescriptor.MethodType methodType) {
        List<DynamicMessage> jsonRequestList = JsonMessage.buildJsonMessageList(GsonUtils.getInstance().toObjectMap(requestJsons));
        DynamicMessage jsonResponse = JsonMessage.buildJsonMessage();

        MethodDescriptor<DynamicMessage, DynamicMessage> jsonMarshallerMethodDescriptor = JsonMessage.createJsonMarshallerMethodDescriptor(metaData.getServiceName(),
                metaData.getMethodName(),
                methodType,
                jsonRequestList.get(0),
                jsonResponse);

        ShenyuGrpcResponse shenyuGrpcResponse = new ShenyuGrpcResponse();
        StreamObserver<DynamicMessage> streamObserver = MessageWriter.newInstance(shenyuGrpcResponse);
        ShenyuGrpcCallRequest callParams = new ShenyuGrpcCallRequest();
        callParams.setMethodDescriptor(jsonMarshallerMethodDescriptor);
        callParams.setChannel(channel);
        callParams.setCallOptions(callOptions);
        callParams.setResponseObserver(streamObserver);
        callParams.setRequests(jsonRequestList);
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
        List<DynamicMessage> requestList = callParams.getRequests();

        StreamObserver<DynamicMessage> responseObserver = callParams.getResponseObserver();
        CompleteObserver<DynamicMessage> doneObserver = new CompleteObserver<>();
        StreamObserver<DynamicMessage> compositeObserver = CompositeStreamObserver.of(responseObserver, doneObserver);

        StreamObserver<DynamicMessage> requestObserver;
        switch (methodType) {
            case UNARY:
                asyncUnaryCall(createCall(callParams), requestList.get(0), compositeObserver);
                return doneObserver.getCompletionFuture();
            case SERVER_STREAMING:
                asyncServerStreamingCall(createCall(callParams), requestList.get(0), compositeObserver);
                return doneObserver.getCompletionFuture();
            case CLIENT_STREAMING:
                requestObserver = asyncClientStreamingCall(createCall(callParams), compositeObserver);
                requestList.forEach(requestObserver::onNext);
                requestObserver.onCompleted();
                return doneObserver.getCompletionFuture();
            case BIDI_STREAMING:
                requestObserver = asyncBidiStreamingCall(createCall(callParams), compositeObserver);
                requestList.forEach(requestObserver::onNext);
                requestObserver.onCompleted();
                return doneObserver.getCompletionFuture();
            default:
                LOG.info("Unknown methodType:{}", methodType);
                return null;
        }
    }

    @Override
    public void close() {
        this.channel.shutdown();
    }

    private ClientCall<DynamicMessage, DynamicMessage> createCall(final ShenyuGrpcCallRequest callParams) {
        return callParams.getChannel().newCall(callParams.getMethodDescriptor(),
                callParams.getCallOptions());
    }
}
