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

import com.google.common.util.concurrent.FutureCallback;
import com.google.common.util.concurrent.Futures;
import com.google.common.util.concurrent.ListenableFuture;
import com.google.common.util.concurrent.MoreExecutors;
import com.google.protobuf.DynamicMessage;
import io.grpc.CallOptions;
import io.grpc.ClientCall;
import io.grpc.ForwardingClientCall;
import io.grpc.ManagedChannel;
import io.grpc.MethodDescriptor;
import io.grpc.stub.StreamObserver;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.convert.plugin.GrpcRegisterConfig;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.grpc.exception.ShenyuGrpcException;
import org.apache.shenyu.plugin.grpc.proto.CompleteObserver;
import org.apache.shenyu.plugin.grpc.proto.MessageWriter;
import org.apache.shenyu.plugin.grpc.proto.ShenyuGrpcCallRequest;
import org.apache.shenyu.plugin.grpc.proto.ShenyuGrpcResponse;
import org.apache.shenyu.plugin.grpc.proto.CompositeStreamObserver;
import org.apache.shenyu.protocol.grpc.message.JsonMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Closeable;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Function;

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
     * @param methodType   methodType
     * @return CompletableFuture future
     */
    public CompletableFuture<ShenyuGrpcResponse> call(final MetaData metaData,
                                                      final CallOptions callOptions,
                                                      final String requestJsons,
                                                      final MethodDescriptor.MethodType methodType) {
        List<DynamicMessage> jsonRequestList = StringUtils.isBlank(requestJsons)
                ? Collections.singletonList(JsonMessage.buildJsonMessage())
                : JsonMessage.buildJsonMessageList(GsonUtils.getInstance().toObjectMap(requestJsons));
        if (CollectionUtils.isEmpty(jsonRequestList)) {
            jsonRequestList = Collections.singletonList(JsonMessage.buildJsonMessage());
        }
        DynamicMessage jsonResponse = JsonMessage.buildJsonMessage();
        
        MethodDescriptor<DynamicMessage, DynamicMessage> jsonMarshallerMethodDescriptor = JsonMessage.createJsonMarshallerMethodDescriptor(metaData.getServiceName(),
                metaData.getMethodName(),
                methodType,
                jsonRequestList.get(0),
                jsonResponse);
        
        ShenyuGrpcResponse shenyuGrpcResponse = new ShenyuGrpcResponse();
        StreamObserver<DynamicMessage> streamObserver = MessageWriter.newInstance(shenyuGrpcResponse,
                resolveMaxAggregatedResultCount(), resolveMaxAggregatedResultBytes());
        
        ShenyuGrpcCallRequest callParams = new ShenyuGrpcCallRequest();
        callParams.setMethodDescriptor(jsonMarshallerMethodDescriptor);
        callParams.setChannel(channel);
        callParams.setCallOptions(callOptions);
        callParams.setResponseObserver(streamObserver);
        callParams.setRequests(jsonRequestList);
        
        ListenableFuture<Void> invocation = this.invoke(callParams);
        CompletableFuture<ShenyuGrpcResponse> result = new CompletableFuture<>();
        Futures.addCallback(invocation, new FutureCallback<>() {
            @Override
            public void onSuccess(final Void ignored) {
                result.complete(shenyuGrpcResponse);
            }

            @Override
            public void onFailure(final Throwable throwable) {
                result.completeExceptionally(new ShenyuGrpcException(
                        "Caught exception while waiting for rpc :{ " + throwable.getMessage() + "}", throwable));
            }
        }, MoreExecutors.directExecutor());
        result.whenComplete((ignored, throwable) -> {
            if (result.isCancelled()) {
                invocation.cancel(true);
            }
        });
        return result;
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

        AtomicBoolean cancelled = new AtomicBoolean();
        ClientCall<DynamicMessage, DynamicMessage> call = new ForwardingClientCall.SimpleForwardingClientCall<>(createCall(callParams)) {
            @Override
            public void cancel(final String message, final Throwable cause) {
                // Downstream cancellation and aggregate overflow share one transport cleanup.
                if (cancelled.compareAndSet(false, true)) {
                    super.cancel(message, cause);
                }
            }
        };
        ListenableFuture<Void> completion = doneObserver.getCompletionFuture();
        completion.addListener(() -> {
            if (completion.isCancelled()) {
                call.cancel("Gateway gRPC invocation cancelled", null);
            }
        }, MoreExecutors.directExecutor());
        if (responseObserver instanceof MessageWriter) {
            // allow the aggregator to cancel the RPC when a response budget is exhausted.
            @SuppressWarnings("unchecked")
            MessageWriter<DynamicMessage> writer = (MessageWriter<DynamicMessage>) responseObserver;
            writer.bindCall(call);
        }

        StreamObserver<DynamicMessage> requestObserver;
        switch (methodType) {
            case UNARY:
                asyncUnaryCall(call, requestList.get(0), compositeObserver);
                return doneObserver.getCompletionFuture();
            case SERVER_STREAMING:
                asyncServerStreamingCall(call, requestList.get(0), compositeObserver);
                return doneObserver.getCompletionFuture();
            case CLIENT_STREAMING:
                StreamingResponseObserver clientStreamingObserver = new StreamingResponseObserver(compositeObserver, requestList);
                requestObserver = asyncClientStreamingCall(call, clientStreamingObserver);
                startStreaming(doneObserver, requestObserver, clientStreamingObserver, requestList);
                return doneObserver.getCompletionFuture();
            case BIDI_STREAMING:
                StreamingResponseObserver bidiStreamingObserver = new StreamingResponseObserver(compositeObserver, requestList);
                requestObserver = asyncBidiStreamingCall(call, bidiStreamingObserver);
                startStreaming(doneObserver, requestObserver, bidiStreamingObserver, requestList);
                return doneObserver.getCompletionFuture();
            default:
                LOG.info("Unknown methodType:{}", methodType);
                return null;
        }
    }

    private long resolveMaxAggregatedResultCount() {
        return resolveBudget(config -> config.getMaxAggregatedResultCount());
    }

    private long resolveMaxAggregatedResultBytes() {
        return resolveBudget(config -> config.getMaxAggregatedResultBytes());
    }

    private long resolveBudget(final Function<GrpcRegisterConfig, Long> getter) {
        GrpcRegisterConfig config = Singleton.INST.get(GrpcRegisterConfig.class);
        if (Objects.isNull(config)) {
            return 0L;
        }
        return Optional.ofNullable(getter.apply(config)).orElse(0L);
    }

    private void startStreaming(final CompleteObserver<DynamicMessage> doneObserver,
                                final StreamObserver<DynamicMessage> requestObserver,
                                final StreamingResponseObserver streamingObserver,
                                final List<DynamicMessage> requestList) {
        if (streamingObserver.start()) {
            doneObserver.getCompletionFuture().addListener(streamingObserver::terminate, MoreExecutors.directExecutor());
            return;
        }
        requestList.forEach(requestObserver::onNext);
        requestObserver.onCompleted();
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
