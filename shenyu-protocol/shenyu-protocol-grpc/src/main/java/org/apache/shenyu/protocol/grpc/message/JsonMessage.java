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

package org.apache.shenyu.protocol.grpc.message;

import com.google.common.collect.Maps;
import com.google.protobuf.DescriptorProtos;
import com.google.protobuf.Descriptors;
import com.google.protobuf.DynamicMessage;
import com.google.protobuf.ExtensionRegistryLite;
import io.grpc.MethodDescriptor;
import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.protocol.grpc.constant.GrpcConstants;

import java.io.IOException;
import java.io.InputStream;
import java.util.Map;

/**
 * JsonMessage.
 */
@Slf4j
public class JsonMessage {
    /**
     * methodDescriptorCache.
     */
    private static Map<String, MethodDescriptor<DynamicMessage, DynamicMessage>> methodDescriptorCache = Maps.newConcurrentMap();

    /**
     * Dynamic build JsonMarshaller Descriptor.
     *
     * @return Descriptors.Descriptor
     */
    private static Descriptors.Descriptor buildJsonMarshallerDescriptor() {
        // build Descriptor Proto
        DescriptorProtos.DescriptorProto.Builder jsonMarshaller = DescriptorProtos.DescriptorProto.newBuilder();
        jsonMarshaller.setName(GrpcConstants.JSON_DESCRIPTOR_PROTO_NAME);
        jsonMarshaller.addFieldBuilder()
                .setName(GrpcConstants.JSON_DESCRIPTOR_PROTO_FIELD_NAME)
                .setNumber(1)
                .setType(DescriptorProtos.FieldDescriptorProto.Type.TYPE_STRING);

        // build File Descriptor Proto
        DescriptorProtos.FileDescriptorProto.Builder fileDescriptorProtoBuilder = DescriptorProtos.FileDescriptorProto.newBuilder();
        fileDescriptorProtoBuilder.addMessageType(jsonMarshaller);

        DescriptorProtos.FileDescriptorProto fileDescriptorProto = fileDescriptorProtoBuilder.build();
        try {
            Descriptors.FileDescriptor fileDescriptor = Descriptors.FileDescriptor
                    .buildFrom(fileDescriptorProto, new Descriptors.FileDescriptor[0]);
            return fileDescriptor.findMessageTypeByName(GrpcConstants.JSON_DESCRIPTOR_PROTO_NAME);
        } catch (Descriptors.DescriptorValidationException e) {
            log.error("dynamic build JsonMarshaller descriptor is fail: {}", e.getMessage());
            throw new RuntimeException("dynamic build JsonMarshaller descriptor is fail", e);
        }
    }

    /**
     * buildJsonMessage.
     *
     * @param jsonParam jsonParam
     * @return DynamicMessage
     */
    public static DynamicMessage buildJsonMessage(final String jsonParam) {
        // build Descriptor and set request param
        Descriptors.Descriptor jsonDescriptor = buildJsonMarshallerDescriptor();
        DynamicMessage.Builder jsonDynamicMessage = DynamicMessage.newBuilder(jsonDescriptor);
        jsonDynamicMessage.setField(jsonDescriptor.findFieldByName(GrpcConstants.JSON_DESCRIPTOR_PROTO_FIELD_NAME), jsonParam);
        return jsonDynamicMessage.build();
    }

    /**
     * buildJsonMessage.
     *
     * @return DynamicMessage
     */
    public static DynamicMessage buildJsonMessage() {
        Descriptors.Descriptor jsonDescriptor = buildJsonMarshallerDescriptor();
        DynamicMessage.Builder jsonDynamicMessage = DynamicMessage.newBuilder(jsonDescriptor);
        return jsonDynamicMessage.build();
    }

    /**
     * get data from DynamicMessage.
     *
     * @param message message
     * @return data
     */
    public static String getDataFromDynamicMessage(final DynamicMessage message) {
        for (Map.Entry<Descriptors.FieldDescriptor, Object> entry : message.getAllFields().entrySet()) {
            Descriptors.FieldDescriptor key = entry.getKey();
            Object value = entry.getValue();

            String fullName = key.getFullName();
            String jsonMessageFullName = GrpcConstants.JSON_DESCRIPTOR_PROTO_NAME + "." + GrpcConstants.JSON_DESCRIPTOR_PROTO_FIELD_NAME;
            if (jsonMessageFullName.equals(fullName)) {
                return (String) value;
            }
        }
        return "";
    }

    /**
     * Create json marshaller MethodDescriptor.
     *
     * @param serviceName service name
     * @param methodName  method name
     * @param request     request marshaller
     * @param response    response marshaller
     * @return MethodDescriptor
     */
    public static MethodDescriptor<DynamicMessage, DynamicMessage> createJsonMarshallerMethodDescriptor(final String serviceName,
                                                                                                        final String methodName,
                                                                                                        final DynamicMessage request,
                                                                                                        final DynamicMessage response) {
        MethodDescriptor<DynamicMessage, DynamicMessage> methodDescriptor = methodDescriptorCache.get(serviceName + GrpcConstants.GRPC_JSON_SERVICE + methodName);
        if (methodDescriptor == null) {
            methodDescriptor = MethodDescriptor.<DynamicMessage, DynamicMessage>newBuilder()
                    .setType(MethodDescriptor.MethodType.UNARY)
                    .setFullMethodName(MethodDescriptor.generateFullMethodName(serviceName + GrpcConstants.GRPC_JSON_SERVICE, methodName))
                    .setRequestMarshaller(new DynamicMessageMarshaller(request.getDescriptorForType()))
                    .setResponseMarshaller(new DynamicMessageMarshaller(response.getDescriptorForType()))
                    .build();
            methodDescriptorCache.put(serviceName + GrpcConstants.GRPC_JSON_SERVICE + methodName, methodDescriptor);

        }
        return methodDescriptor;
    }

    /**
     * DynamicMessageMarshaller.
     */
    private static final class DynamicMessageMarshaller implements MethodDescriptor.Marshaller<DynamicMessage> {

        private final Descriptors.Descriptor messageDescriptor;

        private DynamicMessageMarshaller(final Descriptors.Descriptor messageDescriptor) {
            this.messageDescriptor = messageDescriptor;
        }

        @Override
        public DynamicMessage parse(final InputStream inputStream) {
            try {
                return DynamicMessage.newBuilder(messageDescriptor)
                        .mergeFrom(inputStream, ExtensionRegistryLite.getEmptyRegistry())
                        .build();
            } catch (IOException e) {
                throw new RuntimeException("Unable to merge from the supplied input stream", e);
            }
        }

        @Override
        public InputStream stream(final DynamicMessage abstractMessage) {
            return abstractMessage.toByteString().newInput();
        }
    }
}
