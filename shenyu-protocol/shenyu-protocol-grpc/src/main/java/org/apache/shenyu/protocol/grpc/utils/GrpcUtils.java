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

package org.apache.shenyu.protocol.grpc.utils;

import com.google.common.collect.Maps;
import com.google.protobuf.Message;
import io.grpc.MethodDescriptor;
import io.grpc.MethodDescriptor.MethodType;
import org.apache.shenyu.protocol.grpc.constant.GrpcConstants;
import org.apache.shenyu.protocol.grpc.message.JsonReply;
import org.apache.shenyu.protocol.grpc.message.JsonRequest;

import java.lang.reflect.Constructor;
import java.util.Map;

/**
 * Grpc Utils.
 */
@SuppressWarnings("unchecked")
public class GrpcUtils {
    private static Map<String, Object> methodDescriptorCache = Maps.newHashMap();

    /**
     * create json MethodDescriptor.
     *
     * @param clazzName  clazz name
     * @param methodName method name
     * @return MethodDescriptor
     */
    public static MethodDescriptor<Message, Message> createJsonMethodDescriptor(final String clazzName,
                                                                                final String methodName) {
        MethodDescriptor<Message, Message> methodDescriptor = (MethodDescriptor<Message, Message>) methodDescriptorCache
                .get(clazzName + GrpcConstants.GRPC_JSON_GENERIC_SERVICE + methodName);
        if (methodDescriptor == null) {
            Message argsReq = createDefaultInstance(JsonRequest.class);
            Message argsRep = createDefaultInstance(JsonReply.class);
            methodDescriptor = MethodDescriptor.<Message, Message>newBuilder().setType(MethodType.UNARY)
                    .setFullMethodName(MethodDescriptor.generateFullMethodName(clazzName + GrpcConstants.GRPC_JSON_GENERIC_SERVICE, methodName))
                    .setRequestMarshaller(io.grpc.protobuf.ProtoUtils.marshaller(argsReq))
                    .setResponseMarshaller(io.grpc.protobuf.ProtoUtils.marshaller(argsRep))
                    .setSafe(false)
                    .setIdempotent(false)
                    .build();
            methodDescriptorCache.put(clazzName + GrpcConstants.GRPC_JSON_GENERIC_SERVICE + methodName, methodDescriptor);

        }
        return methodDescriptor;
    }

    /**
     * create a instance by the type.
     *
     * @param type grpc message class
     * @return grpc message
     */
    public static Message createDefaultInstance(final Class<?> type) {
        Class<? extends Message> messageType = (Class<? extends Message>) type;
        Object obj = classInstance(messageType);
        return (Message) obj;
    }

    /**
     * create a class instance.
     *
     * @param clazz class type
     * @return a instance
     */
    public static Object classInstance(final Class<?> clazz) {
        try {
            Constructor<?> con = clazz.getDeclaredConstructor();
            con.setAccessible(true);
            return con.newInstance();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }
}
