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

import com.google.common.util.concurrent.ListenableFuture;
import com.google.protobuf.DescriptorProtos;
import com.google.protobuf.Descriptors;
import com.google.protobuf.DynamicMessage;
import com.google.protobuf.InvalidProtocolBufferException;
import com.google.protobuf.util.JsonFormat;
import io.grpc.Channel;
import io.grpc.MethodDescriptor;
import io.grpc.reflection.v1alpha.ServerReflectionGrpc;
import io.grpc.reflection.v1alpha.ServerReflectionRequest;
import io.grpc.stub.StreamObserver;
import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.common.exception.SoulException;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

import static io.grpc.MethodDescriptor.generateFullMethodName;

/**
 * The Soul Grpc Reflection Client.
 *
 * @author zhanglei
 */
@Slf4j
public final class SoulGrpcReflectionClient {

    private final Channel channel;

    /**
     * key   : package + service name.
     * value : proro file.
     */
    private final Map<String, DescriptorProtos.FileDescriptorSet> fileDescriptorCache = new ConcurrentHashMap<>();

    private SoulGrpcReflectionClient(final Channel channel) {
        this.channel = channel;
    }

    /**
     * A new reflection client using the supplied channel.
     *
     * @param channel channel
     * @return SoulGrpcReflectionClient client
     */
    public static SoulGrpcReflectionClient create(final Channel channel) {
        return new SoulGrpcReflectionClient(channel);
    }

    /**
     * Get Grpc service by the remote server.
     *
     * @param serviceName serviceName
     * @return ListenableFuture future
     */
    public ListenableFuture<DescriptorProtos.FileDescriptorSet> lookupService(final String serviceName) {
        LookupServiceHandler rpcHandler = new LookupServiceHandler(serviceName);
        StreamObserver<ServerReflectionRequest> requestStream = ServerReflectionGrpc.newStub(channel)
                .withDeadlineAfter(10000, TimeUnit.MILLISECONDS)
                .serverReflectionInfo(rpcHandler);
        return rpcHandler.start(requestStream);
    }

    /**
     * Resolve services.
     *
     * @param serviceName serviceName
     * @return FileDescriptorSet fleDescriptorSet
     */
    public DescriptorProtos.FileDescriptorSet resolveService(final String serviceName) {
        return fileDescriptorCache.computeIfAbsent(serviceName, k -> {
            try {
                return this.lookupService(k).get();
            } catch (InterruptedException | ExecutionException e) {
                log.error("Resolve services get error", e);
                throw new SoulException(e);
            }
        });
    }

    /**
     * Fetch full method name.
     *
     * @param methodDescriptor methodDescriptor
     * @return String str
     */
    public String fetchFullMethodName(final Descriptors.MethodDescriptor methodDescriptor) {
        String serviceName = methodDescriptor.getService().getFullName();
        String methodName = methodDescriptor.getName();
        return generateFullMethodName(serviceName, methodName);
    }

    /**
     * Fetch method type.
     *
     * @param methodDescriptor methodDescriptor
     * @return MethodType
     */
    public MethodDescriptor.MethodType fetchMethodType(final Descriptors.MethodDescriptor methodDescriptor) {
        boolean clientStreaming = methodDescriptor.toProto().getClientStreaming();
        boolean serverStreaming = methodDescriptor.toProto().getServerStreaming();
        if (clientStreaming && serverStreaming) {
            return MethodDescriptor.MethodType.BIDI_STREAMING;
        } else if (!clientStreaming && !serverStreaming) {
            return MethodDescriptor.MethodType.UNARY;
        } else if (!clientStreaming) {
            return MethodDescriptor.MethodType.SERVER_STREAMING;
        } else {
            return MethodDescriptor.MethodType.SERVER_STREAMING;
        }
    }

    /**
     * Parse message.
     *
     * @param registry   registry
     * @param descriptor descriptor
     * @param jsons      jsons
     * @return DynamicMessage
     */
    public DynamicMessage parseToMessages(final JsonFormat.TypeRegistry registry, final Descriptors.Descriptor descriptor, final String jsons) {
        JsonFormat.Parser parser = JsonFormat.parser().usingTypeRegistry(registry);
        try {
            DynamicMessage.Builder messageBuilder = DynamicMessage.newBuilder(descriptor);
            parser.merge(jsons, messageBuilder);
            return messageBuilder.build();
        } catch (InvalidProtocolBufferException e) {
            throw new IllegalArgumentException("Unable to parse json text", e);
        }
    }

    /**
     * Get FileDescriptor cache.
     *
     * @return map
     */
    public Map<String, DescriptorProtos.FileDescriptorSet> getFileDescriptorCache() {
        return fileDescriptorCache;
    }
}
