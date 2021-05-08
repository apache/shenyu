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

import java.io.Closeable;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import com.google.common.util.concurrent.ListenableFuture;
import com.google.protobuf.DescriptorProtos;
import com.google.protobuf.Descriptors;
import com.google.protobuf.DynamicMessage;
import com.google.protobuf.util.JsonFormat;
import io.grpc.CallOptions;
import io.grpc.ClientCall;
import io.grpc.ManagedChannel;
import io.grpc.MethodDescriptor;
import io.grpc.stub.StreamObserver;
import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.plugin.grpc.proto.CompleteObserver;
import org.apache.shenyu.plugin.grpc.proto.CompositeStreamObserver;
import org.apache.shenyu.plugin.grpc.proto.DynamicMessageMarshaller;
import org.apache.shenyu.plugin.grpc.proto.MessageWriter;
import org.apache.shenyu.plugin.grpc.proto.SoulGrpcCallRequest;
import org.apache.shenyu.plugin.grpc.proto.SoulGrpcResponse;
import org.apache.shenyu.plugin.grpc.reflection.SoulGrpcReflectionClient;
import org.apache.shenyu.plugin.grpc.resolver.ServiceResolver;
import org.apache.shenyu.common.dto.MetaData;

import static io.grpc.stub.ClientCalls.asyncUnaryCall;
import static io.grpc.stub.ClientCalls.asyncServerStreamingCall;
import static io.grpc.stub.ClientCalls.asyncClientStreamingCall;
import static io.grpc.stub.ClientCalls.asyncBidiStreamingCall;


/**
 * The Soul grpc client.
 *
 * @author zhanglei
 */
@Slf4j
public class SoulGrpcClient implements Closeable {

    private final ManagedChannel channel;

    private final SoulGrpcReflectionClient reflectionClient;

    public SoulGrpcClient(final ManagedChannel channel) {
        this.channel = channel;
        this.reflectionClient = SoulGrpcReflectionClient.create(channel);
    }

    /**
     * Grpc call.
     *
     * @param metaData     metadata
     * @param callOptions  callOptions
     * @param requestJsons requestJsons
     * @return CompletableFuture future
     */
    public CompletableFuture<SoulGrpcResponse> call(final MetaData metaData, final CallOptions callOptions, final String requestJsons) {
        DescriptorProtos.FileDescriptorSet fileDescriptorSet = reflectionClient.resolveService(metaData.getServiceName());
        if (fileDescriptorSet == null) {
            return null;
        }
        ServiceResolver serviceResolver = ServiceResolver.fromFileDescriptorSet(fileDescriptorSet);
        Descriptors.MethodDescriptor methodDescriptor = serviceResolver.resolveServiceMethod(metaData);
        JsonFormat.TypeRegistry registry = JsonFormat.TypeRegistry.newBuilder().add(serviceResolver.listMessageTypes()).build();
        DynamicMessage requestMessages = reflectionClient.parseToMessages(registry, methodDescriptor.getInputType(), requestJsons);
        SoulGrpcResponse soulGrpcResponse = new SoulGrpcResponse();
        StreamObserver<DynamicMessage> streamObserver = MessageWriter.newInstance(registry, soulGrpcResponse);
        SoulGrpcCallRequest callParams = SoulGrpcCallRequest.builder()
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
        return CompletableFuture.completedFuture(soulGrpcResponse);
    }

    /**
     * Grpc call.
     *
     * @param callParams callParams
     * @return ListenableFuture future
     */
    public ListenableFuture<Void> invoke(final SoulGrpcCallRequest callParams) {
        MethodDescriptor.MethodType methodType = reflectionClient.fetchMethodType(callParams.getMethodDescriptor());
        DynamicMessage request = callParams.getRequests();
        StreamObserver<DynamicMessage> responseObserver = callParams.getResponseObserver();
        CompleteObserver<DynamicMessage> doneObserver = new CompleteObserver<>();
        StreamObserver<DynamicMessage> compositeObserver = CompositeStreamObserver.of(responseObserver, doneObserver);
        StreamObserver<DynamicMessage> requestObserver;
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
        this.reflectionClient.getFileDescriptorCache().clear();
    }

    private ClientCall<DynamicMessage, DynamicMessage> createCall(final SoulGrpcCallRequest callParams) {
        return callParams.getChannel().newCall(createGrpcMethodDescriptor(callParams.getMethodDescriptor()),
                callParams.getCallOptions());
    }

    private io.grpc.MethodDescriptor<DynamicMessage, DynamicMessage> createGrpcMethodDescriptor(final Descriptors.MethodDescriptor descriptor) {
        return io.grpc.MethodDescriptor.<DynamicMessage, DynamicMessage>newBuilder()
                .setType(reflectionClient.fetchMethodType(descriptor))
                .setFullMethodName(reflectionClient.fetchFullMethodName(descriptor))
                .setRequestMarshaller(new DynamicMessageMarshaller(descriptor.getInputType()))
                .setResponseMarshaller(new DynamicMessageMarshaller(descriptor.getOutputType()))
                .build();
    }
}
