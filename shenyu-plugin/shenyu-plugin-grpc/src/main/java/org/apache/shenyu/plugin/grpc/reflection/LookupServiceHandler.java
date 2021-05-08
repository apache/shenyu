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

package org.apache.shenyu.plugin.grpc.reflection;

import com.google.common.collect.ImmutableSet;
import com.google.common.util.concurrent.ListenableFuture;
import com.google.common.util.concurrent.SettableFuture;
import com.google.protobuf.ByteString;
import com.google.protobuf.DescriptorProtos;
import com.google.protobuf.InvalidProtocolBufferException;
import io.grpc.reflection.v1alpha.ServerReflectionRequest;
import io.grpc.reflection.v1alpha.ServerReflectionResponse;
import io.grpc.stub.StreamObserver;
import org.apache.shenyu.common.exception.SoulException;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Map;

/**
 * LookupServiceHandler.
 *
 * @author zhanglei
 */
public class LookupServiceHandler implements StreamObserver<ServerReflectionResponse> {

    private final String serviceName;

    private final Set<String> requestedDescriptors;

    private final SettableFuture<DescriptorProtos.FileDescriptorSet> resultFuture;

    private final Map<String, DescriptorProtos.FileDescriptorProto> resolvedDescriptors;

    private StreamObserver<ServerReflectionRequest> requestStream;

    private int outstandingRequests;

    public LookupServiceHandler(final String serviceName) {
        this.serviceName = serviceName;
        this.resultFuture = SettableFuture.create();
        this.resolvedDescriptors = new HashMap<>();
        this.requestedDescriptors = new HashSet<>();
        this.outstandingRequests = 0;
    }

    /**
     * Start the handler.
     *
     * @param requestStream stream
     * @return ListenableFuture future
     */
    public ListenableFuture<DescriptorProtos.FileDescriptorSet> start(final StreamObserver<ServerReflectionRequest> requestStream) {
        this.requestStream = requestStream;
        requestStream.onNext(requestForSymbol(serviceName));
        ++outstandingRequests;
        return resultFuture;
    }

    @Override
    public void onNext(final ServerReflectionResponse response) {
        ServerReflectionResponse.MessageResponseCase responseCase = response.getMessageResponseCase();
        if (responseCase == ServerReflectionResponse.MessageResponseCase.FILE_DESCRIPTOR_RESPONSE) {
            ImmutableSet<DescriptorProtos.FileDescriptorProto> descriptors =
                    parseDescriptors(response.getFileDescriptorResponse().getFileDescriptorProtoList());
            descriptors.forEach(d -> resolvedDescriptors.put(d.getName(), d));
            descriptors.forEach(this::processDependencies);
        }
    }

    @Override
    public void onError(final Throwable t) {
        resultFuture.setException(new RuntimeException("Reflection lookup rpc failed for: " + serviceName, t));
    }

    @Override
    public void onCompleted() {
        if (!resultFuture.isDone()) {
            resultFuture.setException(new RuntimeException("Unexpected end of rpc"));
        }
    }

    private ImmutableSet<DescriptorProtos.FileDescriptorProto> parseDescriptors(final List<ByteString> descriptorBytes) {
        ImmutableSet.Builder<DescriptorProtos.FileDescriptorProto> resultBuilder = ImmutableSet.builder();
        for (ByteString fileDescriptorBytes : descriptorBytes) {
            try {
                resultBuilder.add(DescriptorProtos.FileDescriptorProto.parseFrom(fileDescriptorBytes));
            } catch (InvalidProtocolBufferException e) {
                throw new SoulException(e);
            }
        }
        return resultBuilder.build();
    }

    private void processDependencies(final DescriptorProtos.FileDescriptorProto fileDescriptor) {
        fileDescriptor.getDependencyList().forEach(dep -> {
            if (!resolvedDescriptors.containsKey(dep) && !requestedDescriptors.contains(dep)) {
                requestedDescriptors.add(dep);
                ++outstandingRequests;
                requestStream.onNext(requestForDescriptor(dep));
            }
        });

        --outstandingRequests;
        if (outstandingRequests == 0) {
            resultFuture.set(DescriptorProtos.FileDescriptorSet.newBuilder()
                    .addAllFile(resolvedDescriptors.values())
                    .build());
            requestStream.onCompleted();
        }
    }

    private static ServerReflectionRequest requestForDescriptor(final String name) {
        return ServerReflectionRequest.newBuilder()
                .setFileByFilename(name)
                .build();
    }

    private static ServerReflectionRequest requestForSymbol(final String symbol) {
        return ServerReflectionRequest.newBuilder()
                .setFileContainingSymbol(symbol)
                .build();
    }
}
