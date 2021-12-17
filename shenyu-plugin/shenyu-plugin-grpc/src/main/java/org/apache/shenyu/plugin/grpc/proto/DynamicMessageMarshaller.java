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

package org.apache.shenyu.plugin.grpc.proto;

import com.google.protobuf.Descriptors.Descriptor;
import com.google.protobuf.DynamicMessage;
import com.google.protobuf.ExtensionRegistryLite;
import io.grpc.MethodDescriptor.Marshaller;
import org.apache.shenyu.plugin.grpc.exception.ShenyuGrpcException;

import java.io.IOException;
import java.io.InputStream;

/**
 * Dynamic messages.
 */
public class DynamicMessageMarshaller implements Marshaller<DynamicMessage> {

    private final Descriptor messageDescriptor;

    public DynamicMessageMarshaller(final Descriptor messageDescriptor) {
        this.messageDescriptor = messageDescriptor;
    }

    @Override
    public DynamicMessage parse(final InputStream inputStream) {
        try {
            return DynamicMessage.newBuilder(messageDescriptor)
                    .mergeFrom(inputStream, ExtensionRegistryLite.getEmptyRegistry())
                    .build();
        } catch (IOException e) {
            throw new ShenyuGrpcException("Unable to merge from the supplied input stream", e);
        }
    }

    @Override
    public InputStream stream(final DynamicMessage abstractMessage) {
        return abstractMessage.toByteString().newInput();
    }
}
