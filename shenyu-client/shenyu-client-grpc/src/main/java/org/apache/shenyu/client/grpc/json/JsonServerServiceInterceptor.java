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

package org.apache.shenyu.client.grpc.json;

import com.google.common.collect.Maps;
import io.grpc.MethodDescriptor;
import io.grpc.ServerMethodDefinition;
import io.grpc.ServerServiceDefinition;
import io.grpc.ServiceDescriptor;
import io.grpc.ServerCallHandler;
import io.grpc.ServerCall;
import io.grpc.Metadata;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.ReflectUtils;
import org.apache.shenyu.protocol.grpc.constant.GrpcConstants;
import org.apache.shenyu.protocol.grpc.message.JsonMessage;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Support json invoke.
 */
public class JsonServerServiceInterceptor {

    private static Map<String, Class<?>> requestClazzMap = Maps.newConcurrentMap();

    private static Map<String, MethodDescriptor.MethodType> methodTypeMap = Maps.newConcurrentMap();

    /**
     * wrap ServerServiceDefinition to get json ServerServiceDefinition.
     * @param serviceDef ServerServiceDefinition
     * @return json ServerServiceDefinition
     * @throws IllegalArgumentException IllegalArgumentException
     * @throws IllegalAccessException IllegalAccessException
     */
    public static ServerServiceDefinition useJsonMessages(final ServerServiceDefinition serviceDef)
            throws IllegalArgumentException, IllegalAccessException {
        return useMarshalledMessages(serviceDef,
                io.grpc.protobuf.ProtoUtils.marshaller(JsonMessage.buildJsonMessage()));
    }

    /**
     * wrap method.
     * @param serviceDef ServerServiceDefinition
     * @param marshaller message
     * @param <T> message type
     * @return wrap ServerServiceDefinition
     * @throws IllegalArgumentException IllegalArgumentException
     * @throws IllegalAccessException IllegalAccessException
     */
    public static <T> ServerServiceDefinition useMarshalledMessages(final ServerServiceDefinition serviceDef,
                                                                    final MethodDescriptor.Marshaller<T> marshaller)
            throws IllegalArgumentException, IllegalAccessException {
        List<ServerMethodDefinition<?, ?>> wrappedMethods = new ArrayList<>();
        List<MethodDescriptor<?, ?>> wrappedDescriptors = new ArrayList<>();

        // Wrap the descriptors
        for (final ServerMethodDefinition<?, ?> definition : serviceDef.getMethods()) {
            MethodDescriptor.Marshaller<?> requestMarshaller = definition.getMethodDescriptor().getRequestMarshaller();
            Field defaultInstanceField = ReflectUtils.getField(requestMarshaller.getClass(), "defaultInstance");
            if (Objects.isNull(defaultInstanceField)) {
                throw new ShenyuException(String.format("can not get defaultInstance Field of %s", requestMarshaller.getClass()));
            }

            defaultInstanceField.setAccessible(true);

            String fullMethodName = definition.getMethodDescriptor().getFullMethodName();
            MethodDescriptor.MethodType methodType = definition.getMethodDescriptor().getType();
            methodTypeMap.put(fullMethodName, methodType);

            String[] splitMethodName = fullMethodName.split("/");
            fullMethodName = splitMethodName[0] + GrpcConstants.GRPC_JSON_SERVICE + "/" + splitMethodName[1];
            requestClazzMap.put(fullMethodName, defaultInstanceField.get(requestMarshaller).getClass());

            final MethodDescriptor<?, ?> originalMethodDescriptor = definition.getMethodDescriptor();
            final MethodDescriptor<T, T> wrappedMethodDescriptor = originalMethodDescriptor
                    .toBuilder(marshaller, marshaller).build();
            wrappedDescriptors.add(wrappedMethodDescriptor);
            wrappedMethods.add(wrapMethod(definition, wrappedMethodDescriptor));
        }

        // Build the new service descriptor
        ServiceDescriptor.Builder build = ServiceDescriptor.newBuilder(serviceDef.getServiceDescriptor().getName() + GrpcConstants.GRPC_JSON_SERVICE);
        for (MethodDescriptor<?, ?> md : wrappedDescriptors) {
            Field fullMethodNameField = ReflectUtils.getField(md.getClass(), "fullMethodName");
            if (Objects.isNull(fullMethodNameField)) {
                throw new ShenyuException(String.format("can not get fullMethodName Field of %s", md.getClass()));
            }
            fullMethodNameField.setAccessible(true);
            String fullMethodName = (String) fullMethodNameField.get(md);
            String[] splitMethodName = fullMethodName.split("/");
            fullMethodName = splitMethodName[0] + GrpcConstants.GRPC_JSON_SERVICE + "/" + splitMethodName[1];
            fullMethodNameField.set(md, fullMethodName);

            Field serviceNameField = ReflectUtils.getField(md.getClass(), "serviceName");
            if (Objects.isNull(serviceNameField)) {
                throw new ShenyuException(String.format("can not get serviceName Field Field of %s", md.getClass()));
            }
            serviceNameField.setAccessible(true);
            String serviceName = (String) serviceNameField.get(md);
            serviceName = serviceName + GrpcConstants.GRPC_JSON_SERVICE;
            serviceNameField.set(md, serviceName);

            build.addMethod(md);
        }
        final ServerServiceDefinition.Builder serviceBuilder = ServerServiceDefinition
                .builder(build.build());

        // Create the new service definition
        for (ServerMethodDefinition<?, ?> definition : wrappedMethods) {
            serviceBuilder.addMethod(definition);
        }
        return serviceBuilder.build();
    }

    /**
     * wrap Method.
     * @param definition ServerMethodDefinition
     * @param wrappedMethod MethodDescriptor
     * @param <R> origin request message
     * @param <P> origin response message
     * @param <W> wrap request message
     * @param <M> wrap response message
     * @return wrap method
     */
    private static <R, P, W, M> ServerMethodDefinition<W, M> wrapMethod(
            final ServerMethodDefinition<R, P> definition,
            final MethodDescriptor<W, M> wrappedMethod) {
        final ServerCallHandler<W, M> wrappedHandler = wrapHandler(definition.getServerCallHandler());
        return ServerMethodDefinition.create(wrappedMethod, wrappedHandler);
    }

    /**
     * wrap handler.
     * @param originalHandler original handler
     * @param <R> origin request message
     * @param <P> origin response message
     * @param <W> wrap request message
     * @param <M> wrap response message
     * @return wrap handler
     */
    @SuppressWarnings("unchecked")
    private static <R, P, W, M> ServerCallHandler<W, M> wrapHandler(
            final ServerCallHandler<R, P> originalHandler) {
        return new ServerCallHandler<W, M>() {
            @SuppressWarnings("rawtypes")
            @Override
            public ServerCall.Listener<W> startCall(final ServerCall<W, M> call, final Metadata headers) {
                final ServerCall<R, P> unwrappedCall = new JsonForwardingServerCall<>((ServerCall<P, P>) call);
                final ServerCall.Listener<R> originalListener = originalHandler.startCall(unwrappedCall, headers);
                return new JsonServerCallListener(originalListener, unwrappedCall);
            }
        };
    }

    /**
     * get RequestClazzMap.
     * @return requestClazzMap
     */
    public static Map<String, Class<?>> getRequestClazzMap() {
        return requestClazzMap;
    }

    /**
     * get MethodTypeMap.
     * @return methodTypeMap
     */
    public static Map<String, MethodDescriptor.MethodType> getMethodTypeMap() {
        return methodTypeMap;
    }
}
